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

from . import (
    EnvironmentException,
    SharedAnalysisDirectory,
    assert_readable_directory,
    buck,
    commands,
    get_binary_version,
    is_capable_terminal,
    log,
    log_statistics,
    resolve_analysis_directories,
    switch_root,
    translate_arguments,
)
from .commands import ExitCode
from .configuration import Configuration
from .filesystem import AnalysisDirectory
from .version import __version__


LOG = logging.getLogger(__name__)


def main() -> int:
    def readable_directory(directory: str) -> str:
        assert_readable_directory(directory)
        return directory

    parser = argparse.ArgumentParser(
        allow_abbrev=False,
        formatter_class=argparse.RawTextHelpFormatter,
        epilog="environment variables:"
        "\n   `PYRE_BINARY` overrides the pyre binary used."
        "\n   `PYRE_VERSION_HASH` overrides the pyre version set in the "
        "configuration files.",
    )

    parser.add_argument(
        "-l", "--local-configuration", type=str, help="Use a local configuration"
    )

    parser.add_argument(
        "--version", action="version", version="%(prog)s version " + __version__
    )
    parser.add_argument(
        "--binary-version",
        action="store_true",
        help="Print the pyre.bin version to be used",
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
        "--output",
        choices=[commands.reporting.TEXT, commands.reporting.JSON],
        default=commands.reporting.TEXT,
        help="How to format output",
    )
    parser.add_argument("--verbose", action="store_true", help="Enable verbose logging")
    parser.add_argument(
        "--noninteractive", action="store_true", help="Disable interactive logging"
    )
    parser.add_argument(
        "--show-parse-errors",
        action="store_true",
        help="Display detailed information about parse errors",
    )
    parser.add_argument(
        "--logging-sections", help=argparse.SUPPRESS  # Enable sectional logging.
    )
    parser.add_argument(
        "--log-identifier",
        default="",
        help=argparse.SUPPRESS,  # Add given identifier to logged samples.
    )
    parser.add_argument(
        "--logger", help=argparse.SUPPRESS  # Specify custom logging binary.
    )

    # Link tree determination.
    buck_arguments = parser.add_argument_group("buck")
    buck_arguments.add_argument(
        "--build", action="store_true", help="Build all the necessary artifacts."
    )
    buck_arguments.add_argument(
        "--target", action="append", help="The buck target to check"
    )

    analysis_directory = parser.add_argument_group("analysis-directory")
    analysis_directory.add_argument(
        "--analysis-directory", action="append", help="The analysis directory to check"
    )

    source_directory = parser.add_argument_group("source-directory")
    source_directory.add_argument(
        "--source-directory", action="append", help="The analysis directory to check"
    )

    parser.add_argument(
        "--use-global-shared-analysis-directory",
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
        metavar="{analyze, check, kill, incremental, initialize (init), "
        "query, rage, restart, start, stop}"
    )

    incremental = parsed_commands.add_parser(commands.Incremental.NAME)
    incremental.set_defaults(command=commands.incremental.Incremental)

    rage = parsed_commands.add_parser(commands.Rage.NAME)
    rage.set_defaults(command=commands.Rage)

    check = parsed_commands.add_parser(commands.Check.NAME)
    check.set_defaults(command=commands.Check)

    analyze = parsed_commands.add_parser(commands.Analyze.NAME)
    analyze.set_defaults(command=commands.Analyze)
    analyze.add_argument(
        "--taint-models-path",
        default=None,
        type=readable_directory,
        help="Location of taint models",
    )

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
    initialize.add_argument(
        "--local",
        action="store_true",
        help="Initializes a local configuration in a project subdirectory.",
    )
    initialize.set_defaults(command=commands.Initialize)

    query = parsed_commands.add_parser(commands.Query.NAME)
    query_message = """One of:
    `help`,
    `type_check(path, ...)`,
    `less_or_equal(left, right)`,
    `meet(left, right)`,
    `join(left, right)`,
    `normalize_type(type)`,
    `superclasses(type)`
    """
    query.add_argument("query", help=query_message)
    query.set_defaults(command=commands.Query)

    arguments = parser.parse_args()

    if arguments.source_directory:
        arguments.analysis_directory = arguments.source_directory

    if not hasattr(arguments, "command"):
        if shutil.which("watchman"):
            arguments.command = commands.Incremental
        else:
            watchman_link = "https://facebook.github.io/watchman/docs/install.html"
            LOG.warning(
                "No watchman binary found. \n"
                "To enable pyre incremental, "
                "you can install watchman: {}".format(watchman_link)
            )
            LOG.warning("Defaulting to non-incremental check.")
            arguments.command = commands.Check

    configuration = None
    analysis_directories = []
    shared_analysis_directory = None
    # Having this as a fails-by-default helps flag unexpected exit
    # from exception flows.
    exit_code = ExitCode.FAILURE
    try:
        start = time.time()

        arguments.capable_terminal = is_capable_terminal()
        if arguments.debug or not arguments.capable_terminal:
            arguments.noninteractive = True

        switch_root(arguments)
        translate_arguments(commands, arguments)
        log.initialize(arguments)

        if arguments.command not in [commands.Initialize]:
            configuration = Configuration(
                local_configuration_directory=arguments.local_configuration_directory,
                local_configuration=arguments.local_configuration,
                search_path=arguments.search_path,
                typeshed=arguments.typeshed,
                preserve_pythonpath=arguments.preserve_pythonpath,
            )
            if configuration.disabled:
                LOG.log(
                    log.SUCCESS, "Pyre will not run due to being explicitly disabled"
                )
                return ExitCode.SUCCESS

            if arguments.binary_version:
                log.stdout.write(get_binary_version(configuration))
                return ExitCode.SUCCESS

            if arguments.command in [commands.Kill]:
                analysis_directories = ["."]
            else:
                prompt = arguments.command not in [commands.Incremental, commands.Check]
                analysis_directories = resolve_analysis_directories(
                    arguments, configuration, prompt=prompt
                )

            if len(analysis_directories) == 1:
                analysis_directory = AnalysisDirectory(analysis_directories.pop())
            else:
                local_configuration_path = configuration.local_configuration
                if local_configuration_path:
                    local_root = os.path.dirname(
                        os.path.relpath(
                            local_configuration_path, arguments.current_directory
                        )
                    )
                else:
                    local_root = None
                isolate = (
                    arguments.command in [commands.Check]
                    and not arguments.use_global_shared_analysis_directory
                )
                shared_analysis_directory = SharedAnalysisDirectory(
                    analysis_directories, local_root, isolate
                )
                analysis_directory = shared_analysis_directory

        exit_code = (
            arguments.command(arguments, configuration, analysis_directory)
            .run()
            .exit_code()
        )
    except (buck.BuckException, EnvironmentException) as error:
        LOG.error(str(error))
        if arguments.command == commands.Persistent:
            commands.Persistent.run_null_server(timeout=3600)
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
        log.cleanup(arguments)
        if shared_analysis_directory:
            shared_analysis_directory.cleanup()
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
        sys.exit(ExitCode.FAILURE)
    sys.exit(main())
