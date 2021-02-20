# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Optional

from .. import command_arguments
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
        command_arguments: command_arguments.CommandArguments,
        original_directory: str,
        *,
        configuration: Configuration,
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

    def generate_analysis_directory(self) -> AnalysisDirectory:
        return resolve_analysis_directory(
            self._command_arguments.source_directories,
            self._command_arguments.targets,
            self._configuration,
            self._original_directory,
            filter_directory=self._command_arguments.filter_directory,
            relative_local_root=self._configuration.relative_local_root,
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
