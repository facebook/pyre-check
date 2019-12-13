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
from typing import Type  # noqa

from . import (
    buck,
    commands,
    find_project_root,
    get_binary_version_from_file,
    log,
    log_statistics,
)
from .commands import (  # noqa
    Command,
    ExitCode,
    IncrementalStyle,
    ProfileOutput,
    reporting,
)
from .exceptions import EnvironmentException
from .version import __version__


LOG = logging.getLogger(__name__)  # type: logging.Logger


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
    subcommand_names = ", ".join([command.NAME for command in commands.COMMANDS])
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
            # pyre-fixme[16]: `Namespace` has no attribute `command`.
            arguments.command = commands.Incremental
            # pyre-fixme[16]: `Namespace` has no attribute `nonblocking`.
            arguments.nonblocking = False
            # pyre-fixme[16]: `Namespace` has no attribute `incremental_style`.
            arguments.incremental_style = None
            # pyre-fixme[16]: `Namespace` has no attribute `no_start`.
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

    command = None
    # Having this as a fails-by-default helps flag unexpected exit
    # from exception flows.
    exit_code = ExitCode.FAILURE
    start = time.time()
    try:
        if arguments.version:
            binary_version = get_binary_version_from_file(arguments.local_configuration)
            log.stdout.write(
                "Binary version: {}\nClient version: {}".format(
                    binary_version, __version__
                )
            )
            return ExitCode.SUCCESS
        original_directory = os.getcwd()
        # TODO(T57959968): Stop changing the directory in the client
        os.chdir(find_project_root(original_directory))
        command = arguments.command(arguments, original_directory)

        log.initialize(command.noninteractive, command.log_directory)
        exit_code = command.run().exit_code()
    except buck.BuckException as error:
        LOG.error(str(error))
        if arguments.command == commands.Persistent:
            commands.Persistent.run_null_server(timeout=3600 * 12)
        exit_code = ExitCode.BUCK_ERROR
    except EnvironmentException as error:
        LOG.error(str(error))
        if arguments.command == commands.Persistent:
            commands.Persistent.run_null_server(timeout=3600 * 12)
        exit_code = ExitCode.FAILURE
    except commands.ClientException as error:
        LOG.error(str(error))
        exit_code = ExitCode.FAILURE
    except Exception as error:
        LOG.error(str(error))
        LOG.info(traceback.format_exc())
        exit_code = ExitCode.FAILURE
    except KeyboardInterrupt:
        LOG.warning("Interrupted by user")
        LOG.debug(traceback.format_exc())
        exit_code = ExitCode.SUCCESS
    finally:
        log.cleanup()
        if command and isinstance(command, Command):
            command.analysis_directory.cleanup()
            configuration = command._configuration
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
                        "cwd": os.getcwd(),
                        "client_version": __version__,
                        "command": command.NAME,
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
