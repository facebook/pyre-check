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
from typing import Optional

from . import buck, commands, find_project_root, get_binary_version, log, log_statistics
from .commands import CommandParser, ExitCode, IncrementalStyle
from .exceptions import EnvironmentException
from .version import __version__


LOG: logging.Logger = logging.getLogger(__name__)


def main() -> int:
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

    arguments = parser.parse_args()

    if not hasattr(arguments, "command"):
        if shutil.which("watchman"):
            arguments.command = commands.Incremental
            arguments.nonblocking = False
            arguments.incremental_style = IncrementalStyle.FINE_GRAINED
            arguments.no_start = False
        else:
            watchman_link = "https://facebook.github.io/watchman/docs/install.html"
            LOG.warning(
                "No watchman binary found. \n"
                "To enable pyre incremental, "
                "you can install watchman: {}".format(watchman_link)
            )
            LOG.warning("Defaulting to non-incremental check.")
            arguments.command = commands.Check

    command: Optional[CommandParser] = None
    client_exception_message = ""
    # Having this as a fails-by-default helps flag unexpected exit
    # from exception flows.
    exit_code = ExitCode.FAILURE
    start = time.time()
    try:
        original_directory = os.getcwd()
        # TODO(T57959968): Stop changing the directory in the client
        os.chdir(find_project_root(original_directory))
        command = arguments.command(arguments, original_directory)

        if arguments.version:
            configuration = command.configuration
            if configuration:
                binary_version = (
                    get_binary_version(configuration)
                    or "Cannot get version from binary"
                )
            else:
                binary_version = "Cannot find Pyre binary"
            log.stdout.write(
                "Binary version: {}\nClient version: {}".format(
                    binary_version, __version__
                )
            )
            exit_code = ExitCode.SUCCESS
        else:
            log.initialize(command.noninteractive, command.log_directory)
            exit_code = command.run().exit_code()
    except buck.BuckException as error:
        client_exception_message = str(error)
        if arguments.command == commands.Persistent:
            commands.Persistent.run_null_server(timeout=3600 * 12)
        exit_code = ExitCode.BUCK_ERROR
    except EnvironmentException as error:
        client_exception_message = str(error)
        if arguments.command == commands.Persistent:
            commands.Persistent.run_null_server(timeout=3600 * 12)
        exit_code = ExitCode.FAILURE
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
            command.cleanup()
            configuration = command.configuration
            if configuration and configuration.logger:
                log_statistics(
                    "perfpipe_pyre_usage",
                    arguments=arguments,
                    configuration=configuration,
                    integers={
                        "exit_code": exit_code,
                        "runtime": int((time.time() - start) * 1000),
                    },
                    normals={
                        "root": configuration.local_configuration_root,
                        "cwd": os.getcwd(),
                        "client_version": __version__,
                        "command": command.NAME,
                        "client_exception": client_exception_message,
                    },
                )

    return exit_code


if __name__ == "__main__":
    try:
        os.getcwd()
    except FileNotFoundError:
        LOG.error(
            "Pyre could not determine the current working directory. "
            "Has it been removed?\nExiting."
        )
        sys.exit(ExitCode.FAILURE)
    sys.exit(main())
