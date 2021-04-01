# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
from logging import Logger
from typing import List, Optional

from .. import (
    command_arguments,
    configuration_monitor,
    filesystem,
    json_rpc,
    project_files_monitor,
)
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from .command import ExitCode, IncrementalStyle, Result, State
from .reporting import Reporting
from .start import Start


LOG: Logger = logging.getLogger(__name__)


class ClientInitializationError(Exception):
    pass


class Incremental(Reporting):
    NAME = "incremental"

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        original_directory: str,
        *,
        configuration: Configuration,
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

    def _ensure_server_and_monitors_are_initialized(self) -> None:
        LOG.info("Waiting for server...")
        client_lock = os.path.join(self._configuration.log_directory, "client.lock")

        # The client lock guards the critical section of initializing the
        # configuration monitor, the analysis directory, the server, and the
        # file monitor.
        #
        # * Otherwise, there may be a race where the server has started up but not yet
        # finished initializing. That would mean the file monitor wouldn't be
        # alive yet, which would lead to spurious "file monitor is down" failures.
        # * Another race is where the analysis directory has been built by a
        # background `pyre start` but the server has not yet been started up.
        # That would make Incremental unnecessarily run Start again.
        with filesystem.acquire_lock(client_lock, blocking=True):
            if self._state() == State.DEAD:
                if not self._no_start_server:
                    LOG.info(
                        "Starting server at `%s`.", self._analysis_directory.get_root()
                    )
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
                        raise ClientInitializationError
            else:
                if not self._no_watchman and (
                    not project_files_monitor.ProjectFilesMonitor.is_alive(
                        self._configuration
                    )
                    or not configuration_monitor.ConfigurationMonitor.is_alive(
                        self._configuration
                    )
                ):
                    LOG.warning(
                        "Pyre's file watching service is down."
                        + " Results may be inconsistent with full checks."
                        + " Please run `pyre restart` to bring Pyre server to a "
                        + "consistent state again."
                    )
                    self._exit_code = ExitCode.INCONSISTENT_SERVER
                    raise ClientInitializationError

    def _run(self) -> None:
        try:
            self._ensure_server_and_monitors_are_initialized()
        except ClientInitializationError:
            return

        with self._analysis_directory.acquire_shared_reader_lock():
            request = json_rpc.Request(
                method="displayTypeErrors",
                parameters=json_rpc.ByNameParameters({"files": []}),
            )
            self._send_and_handle_socket_request(request, self._version_hash)

    def _socket_result_handler(self, result: Result) -> None:
        errors = self._get_errors(result)
        self._print(errors)
        if errors:
            self._exit_code = ExitCode.FOUND_ERRORS

    def _flags(self) -> List[str]:
        flags = super()._flags()
        flags.extend(
            [
                "-expected-binary-version",
                self._configuration.get_version_hash_respecting_override()
                or "unversioned",
            ]
        )

        search_path = [
            search_path.command_line_argument()
            for search_path in self._configuration.get_existent_search_paths()
        ]
        if search_path:
            flags.extend(["-search-path", ",".join(search_path)])

        excludes = self._configuration.excludes
        for exclude in excludes:
            flags.extend(["-exclude", exclude])

        extensions = [
            extension.command_line_argument()
            for extension in self._configuration.extensions
        ]
        for extension in extensions:
            flags.extend(["-extension", extension])

        if self._nonblocking:
            flags.append("-nonblocking")

        return flags
