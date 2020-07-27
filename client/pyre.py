# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import argparse
import logging
import os
import shutil
import sys
import time
import traceback
from dataclasses import dataclass
from typing import List, Optional

from . import (
    analysis_directory,
    buck,
    commands,
    log,
    recently_used_configurations,
    statistics,
)
from .commands import CommandParser, ExitCode, IncrementalStyle
from .exceptions import EnvironmentException
from .find_directories import find_project_root
from .version import __version__


LOG: logging.Logger = logging.getLogger(__name__)


@dataclass
class FailedOutsideLocalConfigurationException(Exception):
    exit_code: ExitCode
    command: CommandParser
    exception_message: str


def _set_default_command(arguments: argparse.Namespace) -> None:
    if shutil.which("watchman"):
        arguments.command = commands.Incremental.from_arguments
        arguments.nonblocking = False
        arguments.incremental_style = IncrementalStyle.FINE_GRAINED
        arguments.no_start = False
    else:
        watchman_link = "https://facebook.github.io/watchman/docs/install"
        LOG.warning(
            "No watchman binary found. \n"
            "To enable pyre incremental, "
            "you can install watchman: {}".format(watchman_link)
        )
        LOG.warning("Defaulting to non-incremental check.")
        arguments.command = commands.Check.from_arguments


def _log_statistics(
    command: CommandParser,
    arguments: argparse.Namespace,
    start_time: float,
    client_exception_message: str,
    error_message: Optional[str],
    exit_code: int,
    should_log: bool = True,
) -> None:
    configuration = command.configuration
    if should_log and configuration and configuration.logger:
        statistics.log(
            category=statistics.LoggerCategory.USAGE,
            arguments=arguments,
            configuration=configuration,
            integers={
                "exit_code": exit_code,
                "runtime": int((time.time() - start_time) * 1000),
            },
            normals={
                "root": configuration.local_root,
                "cwd": os.getcwd(),
                "client_version": __version__,
                "command": command.NAME,
                "client_exception": client_exception_message,
                "error_message": error_message,
            },
        )


def run_pyre(arguments: argparse.Namespace) -> ExitCode:
    start_time = time.time()

    command: Optional[CommandParser] = None
    client_exception_message = ""
    should_log_statistics = True
    # Having this as a fails-by-default helps flag unexpected exit
    # from exception flows.
    exit_code = ExitCode.FAILURE
    try:
        original_directory = os.getcwd()
        # TODO(T57959968): Stop changing the directory in the client
        os.chdir(find_project_root(original_directory))

        if arguments.version:
            try:
                # TODO(T64512953): Decouple configuration creation with command creation
                configuration = arguments.command(
                    arguments, original_directory
                ).configuration
                if configuration:
                    binary_version = configuration.get_binary_version()
                    if binary_version:
                        log.stdout.write(f"Binary version: {binary_version}\n")
            except Exception:
                pass
            log.stdout.write(f"Client version: {__version__}\n")
            exit_code = ExitCode.SUCCESS
        else:
            command = arguments.command(arguments, original_directory)
            log.start_logging_to_directory(
                arguments.noninteractive, command.log_directory
            )
            exit_code = command.run().exit_code()
    except analysis_directory.NotWithinLocalConfigurationException as error:
        if arguments.command == commands.Persistent.from_arguments:
            try:
                commands.Persistent.run_null_server(timeout=3600 * 12)
                exit_code = ExitCode.SUCCESS
            except Exception as error:
                client_exception_message = str(error)
                exit_code = ExitCode.FAILURE
            except KeyboardInterrupt:
                LOG.warning("Interrupted by user")
                exit_code = ExitCode.SUCCESS
        elif not command:
            client_exception_message = str(error)
            exit_code = ExitCode.FAILURE
        else:
            should_log_statistics = False
            raise FailedOutsideLocalConfigurationException(
                exit_code, command, str(error)
            )
    except (buck.BuckException, EnvironmentException) as error:
        if arguments.command == commands.Persistent.from_arguments:
            try:
                commands.Persistent.run_null_server(timeout=3600 * 12)
                exit_code = ExitCode.SUCCESS
            except Exception as error:
                client_exception_message = str(error)
                exit_code = ExitCode.FAILURE
            except KeyboardInterrupt:
                LOG.warning("Interrupted by user")
                exit_code = ExitCode.SUCCESS
        else:
            client_exception_message = str(error)
            exit_code = (
                ExitCode.BUCK_ERROR
                if isinstance(error, buck.BuckException)
                else ExitCode.FAILURE
            )
    except commands.ClientException as error:
        client_exception_message = str(error)
        exit_code = ExitCode.FAILURE
    except Exception:
        client_exception_message = traceback.format_exc()
        exit_code = ExitCode.FAILURE
    except KeyboardInterrupt:
        LOG.warning("Interrupted by user")
        LOG.debug(traceback.format_exc())
        exit_code = ExitCode.SUCCESS
    finally:
        if len(client_exception_message) > 0:
            LOG.error(client_exception_message)
        log.cleanup()
        if command:
            result = command.result()
            error_message = result.error if result else None
            command.cleanup()
            _log_statistics(
                command,
                arguments,
                start_time,
                client_exception_message,
                error_message,
                exit_code,
                should_log_statistics,
            )
    return exit_code


def _run_pyre_with_retry(arguments: argparse.Namespace) -> ExitCode:
    try:
        return run_pyre(arguments)
    except FailedOutsideLocalConfigurationException as exception:
        command = exception.command
        exit_code = exception.exit_code
        client_exception_message = exception.exception_message

    configurations = recently_used_configurations.Cache(
        command._dot_pyre_directory
    ).get_all_items()
    if not configurations:
        LOG.error(client_exception_message)
        return exit_code

    LOG.warning(
        f"Could not find a Pyre local configuration at `{command._original_directory}`."
    )
    local_root_for_rerun = recently_used_configurations.prompt_user_for_local_root(
        configurations
    )
    if not local_root_for_rerun:
        return exit_code

    arguments.local_configuration = local_root_for_rerun
    LOG.warning(
        f"Rerunning the command in recent local configuration `{local_root_for_rerun}`."
    )
    try:
        return run_pyre(arguments)
    except FailedOutsideLocalConfigurationException:
        LOG.error(f"Failed to rerun command in `{local_root_for_rerun}`.")
        return ExitCode.FAILURE


# Need the default argument here since this is our entry point in setup.py
def main(argv: List[str] = sys.argv[1:]) -> int:
    parser = argparse.ArgumentParser(
        allow_abbrev=False,
        formatter_class=argparse.RawTextHelpFormatter,
        epilog="environment variables:"
        "\n   `PYRE_BINARY` overrides the pyre binary used."
        "\n   `PYRE_VERSION_HASH` overrides the pyre version set in the "
        "configuration files.",
    )
    commands.Command.add_arguments(parser)

    # Subcommands.
    subcommand_names = ", ".join(
        [command.NAME for command in commands.COMMANDS if not command.HIDDEN]
    )
    parsed_commands = parser.add_subparsers(
        metavar="{}".format(subcommand_names),
        help="""
        The pyre command to run; defaults to `incremental`.
        Run `pyre command --help` for documentation on a specific command.
        """,
    )

    for command in commands.COMMANDS:
        command.add_subparser(parsed_commands)

    arguments = parser.parse_args(argv)

    log.initialize(arguments.noninteractive)

    if not hasattr(arguments, "command"):
        _set_default_command(arguments)

    return _run_pyre_with_retry(arguments)


if __name__ == "__main__":
    try:
        os.getcwd()
    except FileNotFoundError:
        LOG.error(
            "Pyre could not determine the current working directory. "
            "Has it been removed?\nExiting."
        )
        sys.exit(ExitCode.FAILURE)
    sys.exit(main(sys.argv[1:]))
