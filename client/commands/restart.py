# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
from typing import Optional

from ..analysis_directory import AnalysisDirectory, resolve_analysis_directory
from ..configuration import Configuration
from .command import Command, CommandArguments, ExitCode, IncrementalStyle
from .incremental import Incremental
from .start import Start  # noqa
from .stop import Stop


class Restart(Command):
    NAME = "restart"

    def __init__(
        self,
        command_arguments: CommandArguments,
        original_directory: str,
        *,
        configuration: Optional[Configuration] = None,
        analysis_directory: Optional[AnalysisDirectory] = None,
        terminal: bool,
        store_type_check_resolution: bool,
        use_watchman: bool,
        incremental_style: IncrementalStyle,
    ) -> None:
        super(Restart, self).__init__(
            command_arguments, original_directory, configuration, analysis_directory
        )
        self._terminal: bool = terminal
        self._store_type_check_resolution: bool = store_type_check_resolution
        self._use_watchman: bool = use_watchman
        self._incremental_style: IncrementalStyle = incremental_style

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace,
        original_directory: str,
        configuration: Optional[Configuration] = None,
        analysis_directory: Optional[AnalysisDirectory] = None,
    ) -> "Restart":
        return Restart(
            CommandArguments.from_arguments(arguments),
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            terminal=arguments.terminal,
            store_type_check_resolution=arguments.store_type_check_resolution,
            use_watchman=not arguments.no_watchman,
            incremental_style=arguments.incremental_style,
        )

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        restart = parser.add_parser(
            cls.NAME, epilog="Restarts a server. Equivalent to `pyre stop && pyre`."
        )
        restart.set_defaults(command=cls.from_arguments)
        restart.add_argument(
            "--terminal", action="store_true", help="Run the server in the terminal."
        )
        restart.add_argument(
            "--store-type-check-resolution",
            action="store_true",
            help="Store extra information for `types` queries.",
        )
        restart.add_argument(
            "--no-watchman",
            action="store_true",
            help="Do not spawn a watchman client in the background.",
        )
        restart.add_argument(
            "--incremental-style",
            type=IncrementalStyle,
            choices=list(IncrementalStyle),
            default=IncrementalStyle.FINE_GRAINED,
            help="How to approach doing incremental checks.",
        )

    def generate_analysis_directory(self) -> AnalysisDirectory:
        return resolve_analysis_directory(
            self._source_directories,
            self._targets,
            self._configuration,
            self._original_directory,
            self._project_root,
            filter_directory=self._filter_directory,
            use_buck_builder=self._use_buck_builder,
            debug=self._debug,
            buck_mode=self._buck_mode,
            relative_local_root=self.relative_local_root,
        )

    def _run(self) -> None:
        exit_code = (
            Stop(
                self._command_arguments,
                self._original_directory,
                configuration=self._configuration,
                analysis_directory=self._analysis_directory,
                from_restart=True,
            )
            .run()
            .exit_code()
        )
        if exit_code != ExitCode.SUCCESS:
            self._exit_code = ExitCode.FAILURE
            return
        exit_code = (
            Incremental(
                self._command_arguments,
                self._original_directory,
                configuration=self._configuration,
                analysis_directory=self._analysis_directory,
                # Force the incremental run to be blocking.
                nonblocking=False,
                incremental_style=self._incremental_style,
                no_start_server=False,
                no_watchman=not self._use_watchman,
            )
            .run()
            .exit_code()
        )
        self._exit_code = exit_code
