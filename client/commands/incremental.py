# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
from logging import Logger
from typing import List, Optional

from .. import json_rpc
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from ..project_files_monitor import ProjectFilesMonitor
from .command import (
    CommandArguments,
    ExitCode,
    IncrementalStyle,
    Result,
    State,
    typeshed_search_path,
)
from .reporting import Reporting
from .start import Start


LOG: Logger = logging.getLogger(__name__)


class Incremental(Reporting):
    NAME = "incremental"

    def __init__(
        self,
        command_arguments: CommandArguments,
        original_directory: str,
        *,
        configuration: Optional[Configuration] = None,
        analysis_directory: Optional[AnalysisDirectory] = None,
        nonblocking: bool,
        incremental_style: IncrementalStyle,
        no_start_server: bool,
        no_watchman: bool,
    ) -> None:
        super(Incremental, self).__init__(
            command_arguments, original_directory, configuration, analysis_directory
        )
        self._nonblocking = nonblocking
        self._incremental_style = incremental_style
        self._no_start_server = no_start_server
        self._no_watchman = no_watchman

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace,
        original_directory: str,
        configuration: Optional[Configuration] = None,
        analysis_directory: Optional[AnalysisDirectory] = None,
    ) -> "Incremental":
        return Incremental(
            CommandArguments.from_arguments(arguments),
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            nonblocking=arguments.nonblocking,
            incremental_style=arguments.incremental_style,
            no_start_server=arguments.no_start,
            no_watchman=getattr(arguments, "no_watchman", False),
        )

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        incremental_help = """
        Connects to a running Pyre server and returns the current type errors for your
        project. If no server exists for your projects, starts a new one. Running `pyre`
        implicitly runs `pyre incremental`.

        By default, incremental checks ensure that all dependencies of changed files are
        analyzed before returning results. If you'd like to get partial type checking
        results eagerly, you can run `pyre incremental --nonblocking`.
        """
        incremental = parser.add_parser(cls.NAME, epilog=incremental_help)
        incremental.set_defaults(command=cls.from_arguments)
        incremental.add_argument(
            "--nonblocking",
            action="store_true",
            help=(
                "Ask the server to return partial results immediately, "
                "even if analysis is still in progress."
            ),
        )
        incremental.add_argument(
            "--incremental-style",
            type=IncrementalStyle,
            choices=list(IncrementalStyle),
            default=IncrementalStyle.FINE_GRAINED,
            help="How to approach doing incremental checks.",
        )
        incremental.add_argument(
            "--no-start", action="store_true", help=argparse.SUPPRESS
        )
        # This is mostly to allow `restart` to pass on the flag to `start`.
        incremental.add_argument(
            "--no-watchman", action="store_true", help=argparse.SUPPRESS
        )

    def _run(self) -> None:
        if (not self._no_start_server) and self._state() == State.DEAD:
            LOG.info("Starting server at `%s`.", self._analysis_directory.get_root())
            exit_code = (
                Start(
                    self._command_arguments,
                    self._original_directory,
                    configuration=self._configuration,
                    analysis_directory=self._analysis_directory,
                    terminal=False,
                    store_type_check_resolution=False,
                    use_watchman=not self._no_watchman,
                    incremental_style=self._incremental_style,
                )
                .run()
                .exit_code()
            )
            if exit_code != ExitCode.SUCCESS:
                self._exit_code = ExitCode.FAILURE
                return
        else:
            self._restart_file_monitor_if_needed()

        if self._state() != State.DEAD:
            LOG.info("Waiting for server...")

        with self._analysis_directory.acquire_shared_reader_lock():
            request = json_rpc.Request(
                method="displayTypeErrors", parameters={"files": []}
            )
            self._send_and_handle_socket_request(request, self._version_hash)

    def _socket_result_handler(self, result: Result) -> None:
        errors = self._get_errors(result)
        self._print(errors)
        if errors:
            self._exit_code = ExitCode.FOUND_ERRORS

    def _flags(self) -> List[str]:
        flags = super()._flags()
        flags.extend(["-expected-binary-version", self._configuration.version_hash])

        search_path = self._configuration.search_path + typeshed_search_path(
            self._configuration.typeshed
        )
        if search_path:
            flags.extend(["-search-path", ",".join(search_path)])

        excludes = self._configuration.excludes
        for exclude in excludes:
            flags.extend(["-exclude", exclude])

        extensions = self._configuration.extensions
        for extension in extensions:
            flags.extend(["-extension", extension])

        if self._nonblocking:
            flags.append("-nonblocking")

        return flags

    def _restart_file_monitor_if_needed(self) -> None:
        if self._no_watchman:
            return
        ProjectFilesMonitor.restart_if_dead(
            self._configuration, self._project_root, self._analysis_directory
        )
