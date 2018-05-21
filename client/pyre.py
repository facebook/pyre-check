# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
import os
import sys
import time
import traceback

from . import (
    FAILURE,
    JSON,
    SUCCESS,
    TEXT,
    EnvironmentException,
    buck,
    commands,
    get_version,
    is_capable_terminal,
    log,
    log_statistics,
    merge_source_directories,
    resolve_source_directories,
    switch_root,
)
from .configuration import Configuration


LOG = logging.getLogger(__name__)


def main() -> int:

    def readable_directory(directory: str) -> str:
        if not os.path.isdir(directory):
            raise argparse.ArgumentTypeError(
                "{} is not a valid directory".format(directory)
            )
        if not os.access(directory, os.R_OK):
            raise argparse.ArgumentTypeError(
                "{} is not a readable directory".format(directory)
            )
        return directory

    parser = argparse.ArgumentParser(allow_abbrev=False)

    parser.add_argument(
        "-l", "--local-configuration", type=str, help="Use a local configuration"
    )

    parser.add_argument("--debug", action="store_true", help=argparse.SUPPRESS)
    parser.add_argument("--sequential", action="store_true", help=argparse.SUPPRESS)
    parser.add_argument("--strict", action="store_true", help=argparse.SUPPRESS)

    parser.add_argument(
        "--show-error-traces",
        action="store_true",
        help="Display errors trace information",
    )

    # Logging.
    parser.add_argument(
        "--output", choices=[TEXT, JSON], default=TEXT, help="How to format output"
    )
    parser.add_argument("--verbose", action="store_true", help="Enable verbose logging")
    parser.add_argument(
        "--noninteractive", action="store_true", help="Disable interactive logging"
    )
    # Enable sectional logging.
    parser.add_argument("--logging-sections", help=argparse.SUPPRESS)
    # Add given identifier to logged samples.
    parser.add_argument(
        "--log-identifier", action="store", default="", help=argparse.SUPPRESS
    )

    parser.add_argument(
        "--version", action="store_true", help="Print the pyre version to be used"
    )

    # Link tree determination.
    buck_arguments = parser.add_argument_group("buck")
    buck_arguments.add_argument(
        "--build", action="store_true", help="Build all the necessary artifacts."
    )
    buck_arguments.add_argument(
        "--target", action="append", help="The buck target to check"
    )

    source_directory = parser.add_argument_group("source-directory")
    source_directory.add_argument(
        "--source-directory", action="append", help="The source directory to check"
    )

    parser.add_argument(
        "--use-global-shared-source-directory",
        action="store_true",
        help=argparse.SUPPRESS,
    )

    # Handling of search path
    parser.add_argument(
        "--search-path",
        action="append",
        default=[],
        type=readable_directory,
        help="Additional directories with modules and stubs "
        "to include in type environment",
    )
    parser.add_argument(
        "--preserve-pythonpath",
        action="store_true",
        default=False,
        help="Preserves the value of the PYTHONPATH environment variable",
    )

    # Typeshed stubs location
    parser.add_argument(
        "--typeshed",
        default=None,
        type=readable_directory,
        help="Location of the typeshed stubs",
    )

    # Subcommands.
    parsed_commands = parser.add_subparsers(
        metavar="{check, kill, incremental, initialize (init), "
        "rage, restart, start, stop}"
    )

    incremental = parsed_commands.add_parser(commands.Incremental.NAME)
    incremental.set_defaults(command=commands.incremental.Incremental)

    rage = parsed_commands.add_parser(commands.Rage.NAME)
    rage.set_defaults(command=commands.Rage)

    check = parsed_commands.add_parser(commands.Check.NAME)
    check.set_defaults(command=commands.Check)

    analyze = parsed_commands.add_parser(commands.Analyze.NAME)
    analyze.set_defaults(command=commands.Analyze)

    persistent = parsed_commands.add_parser(commands.Persistent.NAME)
    persistent.add_argument(
        "--no-watchman",
        action="store_true",
        help="Do not spawn a watchman client in the background.",
    )
    persistent.set_defaults(command=commands.Persistent, noninteractive=True)

    start = parsed_commands.add_parser(commands.Start.NAME)
    start.add_argument(
        "--terminal", action="store_true", help="Run the server in the terminal."
    )
    start.add_argument(
        "--no-watchman",
        action="store_true",
        help="Do not spawn a watchman client in the background.",
    )
    start.set_defaults(command=commands.Start)

    stop = parsed_commands.add_parser(commands.Stop.NAME)
    stop.set_defaults(command=commands.Stop)

    restart = parsed_commands.add_parser(commands.Restart.NAME)
    restart.add_argument(
        "--terminal", action="store_true", help="Run the server in the terminal."
    )
    restart.add_argument(
        "--no-watchman",
        action="store_true",
        help="Do not spawn a watchman client in the background.",
    )
    restart.set_defaults(command=commands.Restart)

    kill = parsed_commands.add_parser(commands.Kill.NAME)
    kill.add_argument(
        "--with-fire", action="store_true", help="Adds emphasis to the command."
    )
    kill.set_defaults(command=commands.Kill)

    initialize = parsed_commands.add_parser(commands.Initialize.NAME, aliases=["init"])
    initialize.set_defaults(command=commands.Initialize)

    arguments = parser.parse_args()

    if not hasattr(arguments, "command"):
        arguments.command = commands.Incremental

    configuration = None
    try:
        start = time.time()
        exit_code = SUCCESS

        arguments.capable_terminal = is_capable_terminal()
        if arguments.debug or not arguments.capable_terminal:
            arguments.noninteractive = True

        switch_root(arguments)
        log.initialize(arguments)

        source_directories = []

        if arguments.command in [commands.Initialize]:
            configuration = None
            source_directory = None
        else:
            configuration = Configuration(
                original_directory=arguments.original_directory,
                local_configuration=arguments.local_configuration,
                search_path=arguments.search_path,
                typeshed=arguments.typeshed,
                preserve_pythonpath=arguments.preserve_pythonpath,
            )
            if configuration.disabled():
                LOG.log(
                    log.SUCCESS, "Pyre will not run due to being explicitly disabled"
                )
                return SUCCESS

            if arguments.version:
                log.stdout.write(get_version(configuration))
                return SUCCESS

            configuration.validate()

            if getattr(arguments, "with_fire", False):
                source_directories = ["."]
            else:
                prompt = arguments.command not in [commands.Incremental, commands.Check]
                source_directories = resolve_source_directories(
                    arguments, configuration, prompt=prompt
                )

            isolate = (
                arguments.command in [commands.Check]
                and not arguments.use_global_shared_source_directory
            )
            source_directory = merge_source_directories(source_directories, isolate)

        arguments.command(arguments, configuration, source_directory).run()
    except (buck.BuckException, commands.ClientException) as error:
        LOG.error(str(error))
        arguments.command(
            arguments, configuration, source_directory
        ).on_client_exception()
        exit_code = FAILURE
    except EnvironmentException as error:
        LOG.error(str(error))
        exit_code = FAILURE
    except Exception as error:
        LOG.error(str(error))
        LOG.info(traceback.format_exc())
        exit_code = FAILURE
    except KeyboardInterrupt:
        LOG.warning("Interrupted by user")
        LOG.debug(traceback.format_exc())
        exit_code = SUCCESS
    finally:
        log.cleanup(arguments)
        if configuration and configuration.logger:
            log_statistics(
                "perfpipe_pyre_usage",
                arguments,
                configuration,
                ints={
                    "exit_code": exit_code,
                    "runtime": int((time.time() - start) * 1000),
                },
            )

    return exit_code


if __name__ == "__main__":
    try:
        os.getcwd()
    except FileNotFoundError as error:
        print(
            "Pyre could not determine the current working directory. "
            "Has it been removed?\nExiting."
        )
        sys.exit(FAILURE)
    sys.exit(main())
