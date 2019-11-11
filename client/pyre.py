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
