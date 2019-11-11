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
    find_log_directory,
    get_binary_version_from_file,
    is_capable_terminal,
    log,
    log_statistics,
    readable_directory,
    switch_root,
    translate_arguments,
)
from .analysis_directory import AnalysisDirectory, resolve_analysis_directory
from .commands import (  # noqa
    Command,
    ExitCode,
    IncrementalStyle,
    ProfileOutput,
    reporting,
)
from .configuration import Configuration
from .exceptions import EnvironmentException
from .version import __version__


LOG = logging.getLogger(__name__)  # type: logging.Logger


def main() -> int:
    def executable_file(file_path: str) -> str:
        if not os.path.isfile(file_path):
            raise EnvironmentException("%s is not a valid file" % file_path)
        if not os.access(file_path, os.X_OK):
            raise EnvironmentException("%s is not an executable file" % file_path)
        return file_path

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
        "--version",
        action="store_true",
        help="Print the client and binary versions of Pyre.",
    )

    parser.add_argument("--debug", action="store_true", help=argparse.SUPPRESS)
    parser.add_argument("--sequential", action="store_true", help=argparse.SUPPRESS)
    parser.add_argument("--strict", action="store_true", help=argparse.SUPPRESS)
    parser.add_argument("--additional-check", action="append", help=argparse.SUPPRESS)

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
        "--enable-profiling", action="store_true", help=argparse.SUPPRESS
    )
    parser.add_argument(
        "--enable-memory-profiling", action="store_true", help=argparse.SUPPRESS
    )
    parser.add_argument(
        "-n",
        "--noninteractive",
        action="store_true",
        help="Disable interactive logging",
    )
    parser.add_argument(
        "--hide-parse-errors",
        action="store_true",
        help="Hide detailed information about parse errors",
    )
    parser.add_argument(
        "--show-parse-errors",
        action="store_true",
        help="[DEPRECATED] Show detailed information about parse errors",
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
        "--log-directory", help=argparse.SUPPRESS  # Override default location for logs
    )
    parser.add_argument(
        "--logger", help=argparse.SUPPRESS  # Specify custom logging binary.
    )
    parser.add_argument("--formatter", help=argparse.SUPPRESS)

    # Link tree determination.
    buck_arguments = parser.add_argument_group("buck")
    buck_arguments.add_argument(
        "--target", action="append", dest="targets", help="The buck target to check"
    )
    buck_arguments.add_argument(
        "--build",
        action="store_true",
        help="Freshly build all the necessary artifacts.",
    )
    buck_arguments.add_argument(
        "--use-buck-builder",
        action="store_true",
        help="Use Pyre's experimental builder for Buck projects.",
    )
    buck_arguments.add_argument(
        "--use-legacy-builder",
        action="store_true",
        help="Use Pyre's legacy builder for Buck projects.",
    )
    buck_arguments.add_argument(
        "--buck-builder-debug", action="store_true", help=argparse.SUPPRESS
    )

    source_directories = parser.add_argument_group("source-directories")
    source_directories.add_argument(
        "--source-directory",
        action="append",
        dest="source_directories",
        help="The source directory to check",
        type=os.path.abspath,
    )
    source_directories.add_argument(
        "--filter-directory", help=argparse.SUPPRESS  # override filter directory
    )

    parser.add_argument(
        "--use-global-shared-analysis-directory",
        action="store_true",
        help=argparse.SUPPRESS,
    )
    parser.add_argument(
        "--no-saved-state",
        action="store_true",
        help="Don't attempt to load Pyre from a saved state.",
    )

    # Handling of search path
    parser.add_argument(
        "--search-path",
        action="append",
        default=[],
        type=readable_directory,
        help="Add an additional directory of modules and stubs to include"
        " in the type environment",
    )
    parser.add_argument(
        "--preserve-pythonpath",
        action="store_true",
        default=False,
        help="Preserve the value of the PYTHONPATH environment variable and "
        "inherit the current python environment's search path",
    )

    parser.add_argument(
        "--binary",
        default=None,
        type=executable_file,
        help="Location of the pyre binary",
    )

    parser.add_argument(
        "--buck-builder-binary",
        default=None,
        help="Location of the buck builder binary",
    )
    parser.add_argument("--buck-builder-target", default=None, help=argparse.SUPPRESS)

    parser.add_argument(
        "--exclude",
        action="append",
        default=[],
        help="Exclude files and directories matching this regexp from parsing",
    )

    # Typeshed stubs location
    parser.add_argument(
        "--typeshed",
        default=None,
        type=readable_directory,
        help="Location of the typeshed stubs",
    )
    parser.add_argument(
        "--save-initial-state-to",
        default=None,
        help="Path to serialize pyre's initial state to.",
    )
    parser.add_argument(
        "--load-initial-state-from", default=None, type=str, help=argparse.SUPPRESS
    )
    parser.add_argument(
        "--changed-files-path", default=None, type=str, help=argparse.SUPPRESS
    )
    parser.add_argument(
        "--saved-state-project", default=None, type=str, help=argparse.SUPPRESS
    )
    # Temporary flag to help migrate to json sockets for incremental and query commands.
    parser.add_argument(
        "--use-json-sockets", action="store_true", default=False, help=argparse.SUPPRESS
    )
    # Subcommands.
    parsed_commands = parser.add_subparsers(
        metavar="{analyze, check, color, kill, incremental, initialize (init), "
        "query, rage, restart, statistics, start, stop}",
        help="""
        The pyre command to run; defaults to `incremental`.
        Run `pyre command --help` for documentation on a specific command.
        """,
    )

    commands.Incremental.add_subparser(parsed_commands)
    commands.Rage.add_subparser(parsed_commands)
    commands.Check.add_subparser(parsed_commands)
    commands.Color.add_subparser(parsed_commands)
    commands.Deobfuscate.add_subparser(parsed_commands)
    commands.Analyze.add_subparser(parsed_commands)
    commands.Persistent.add_subparser(parsed_commands)
    commands.Start.add_subparser(parsed_commands)
    commands.Stop.add_subparser(parsed_commands)
    commands.Restart.add_subparser(parsed_commands)
    commands.Kill.add_subparser(parsed_commands)
    commands.Initialize.add_subparser(parsed_commands)
    commands.Query.add_subparser(parsed_commands)
    commands.Infer.add_subparser(parsed_commands)
    commands.Statistics.add_subparser(parsed_commands)
    commands.Profile.add_subparser(parsed_commands)

    arguments = parser.parse_args()

    if not hasattr(arguments, "command"):
        if shutil.which("watchman"):
            # pyre-fixme[16]: `Namespace` has no attribute `command`.
            arguments.command = commands.Incremental
            # pyre-fixme[16]: `Namespace` has no attribute `nonblocking`.
            arguments.nonblocking = False
            # pyre-fixme[16]: `Namespace` has no attribute `transitive`.
            arguments.incremental_style = IncrementalStyle.SHALLOW
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
    analysis_directory = None
    # Having this as a fails-by-default helps flag unexpected exit
    # from exception flows.
    exit_code = ExitCode.FAILURE
    start = time.time()
    try:
        # pyre-fixme[16]: `Namespace` has no attribute `capable_terminal`.
        arguments.capable_terminal = is_capable_terminal()
        if arguments.debug or not arguments.capable_terminal:
            # pyre-fixme[16]: `Namespace` has no attribute `noninteractive`.
            arguments.noninteractive = True

        switch_root(arguments)
        translate_arguments(commands, arguments)
        find_log_directory(arguments)
        log.initialize(arguments)

        if arguments.command in [commands.Initialize]:
            analysis_directory = AnalysisDirectory(".")
        else:
            if arguments.version:
                binary_version = get_binary_version_from_file(
                    arguments.local_configuration
                )
                log.stdout.write(
                    "binary version: {}\nclient version: {}".format(
                        binary_version, __version__
                    )
                )
                return ExitCode.SUCCESS
            configuration = Configuration(
                local_configuration=arguments.local_configuration,
                search_path=arguments.search_path,
                binary=arguments.binary,
                typeshed=arguments.typeshed,
                preserve_pythonpath=arguments.preserve_pythonpath,
                excludes=arguments.exclude,
                logger=arguments.logger,
                formatter=arguments.formatter,
                log_directory=arguments.log_directory,
            )
            if configuration.disabled:
                LOG.log(
                    log.SUCCESS, "Pyre will not run due to being explicitly disabled"
                )
                return ExitCode.SUCCESS

            if arguments.command in [commands.Kill]:
                analysis_directory = AnalysisDirectory(".")
            else:
                isolate = arguments.command in [commands.Analyze, commands.Check]
                analysis_directory = resolve_analysis_directory(
                    arguments, commands, configuration, isolate=isolate
                )

        command = arguments.command
        exit_code = (
            # pyre-fixme[6]: Expected Configuration for 2nd anonymous parameter
            # to call Kill.__init__ but got Optional[Configuration].
            command(arguments, configuration, analysis_directory)
            .run()
            .exit_code()
        )
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
        log.cleanup(arguments)
        if analysis_directory:
            analysis_directory.cleanup()
        if configuration and configuration.logger:
            log_statistics(
                "perfpipe_pyre_usage",
                arguments=arguments,
                configuration=configuration,
                integers={
                    "exit_code": exit_code,
                    "runtime": int((time.time() - start) * 1000),
                },
                normals={"cwd": os.getcwd(), "client_version": __version__},
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
