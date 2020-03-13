# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
from typing import Optional

from ..analysis_directory import AnalysisDirectory, resolve_analysis_directory
from ..configuration import Configuration
from .command import Command, ExitCode, IncrementalStyle
from .incremental import Incremental
from .start import Start  # noqa
from .stop import Stop


class Restart(Command):
    NAME = "restart"

    def __init__(
        self,
        arguments: argparse.Namespace,
        original_directory: str,
        configuration: Optional[Configuration] = None,
        analysis_directory: Optional[AnalysisDirectory] = None,
    ) -> None:
        super(Restart, self).__init__(
            arguments, original_directory, configuration, analysis_directory
        )
        self._terminal: bool = arguments.terminal
        self._store_type_check_resolution: bool = arguments.store_type_check_resolution
        self._use_watchman: bool = not arguments.no_watchman
        self._incremental_style: IncrementalStyle = arguments.incremental_style

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        restart = parser.add_parser(
            cls.NAME, epilog="Restarts a server. Equivalent to `pyre stop && pyre`."
        )
        restart.set_defaults(command=cls)
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
            self._arguments,
            self._configuration,
            self._original_directory,
            self._current_directory,
            build=True,
        )

    def _run(self) -> None:
        exit_code = (
            Stop(
                self._arguments,
                self._original_directory,
                self._configuration,
                self._analysis_directory,
                from_restart=True,
            )
            .run()
            .exit_code()
        )
        if exit_code != ExitCode.SUCCESS:
            self._exit_code = ExitCode.FAILURE
            return
        # Force the incremental run to be blocking.
        self._arguments.nonblocking = False
        self._arguments.no_start = False
        exit_code = (
            Incremental(
                self._arguments,
                self._original_directory,
                self._configuration,
                self._analysis_directory,
            )
            .run()
            .exit_code()
        )
        self._exit_code = exit_code
