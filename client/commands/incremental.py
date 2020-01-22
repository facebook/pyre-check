# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import atexit
import json
import logging
import os
import subprocess
from logging import Logger
from typing import IO, List, Optional, cast

from .. import json_rpc
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from ..project_files_monitor import MonitorException, ProjectFilesMonitor
from .command import (
    ClientException,
    ExitCode,
    IncrementalStyle,
    Result,
    State,
    typeshed_search_path,
)
from .reporting import Reporting
from .start import Start


LOG: Logger = logging.getLogger(__name__)


def _convert_to_result(response: json_rpc.Response) -> Result:
    error_code = ExitCode.FAILURE if response.error else ExitCode.SUCCESS
    return Result(output=json.dumps(response.result), code=error_code)


class Incremental(Reporting):
    NAME = "incremental"

    def __init__(
        self,
        arguments: argparse.Namespace,
        original_directory: str,
        configuration: Optional[Configuration] = None,
        analysis_directory: Optional[AnalysisDirectory] = None,
    ) -> None:
        super(Incremental, self).__init__(
            arguments, original_directory, configuration, analysis_directory
        )
        self._nonblocking: bool = arguments.nonblocking
        self._incremental_style: bool = arguments.incremental_style
        self._no_start_server: bool = arguments.no_start

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
        incremental.set_defaults(command=cls)
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
            default=None,
            help="How to approach doing incremental checks.",
        )
        incremental.add_argument(
            "--no-start", action="store_true", help=argparse.SUPPRESS
        )

    def _run(self) -> None:
        if (not self._no_start_server) and self._state() == State.DEAD:
            LOG.warning("Starting server at `%s`.", self._analysis_directory.get_root())
            arguments = self._arguments
            arguments.terminal = False
            arguments.store_type_check_resolution = False
            arguments.no_watchman = False
            exit_code = (
                Start(
                    arguments,
                    self._original_directory,
                    self._configuration,
                    self._analysis_directory,
                )
                .run()
                .exit_code()
            )
            if exit_code != ExitCode.SUCCESS:
                self._exit_code = ExitCode.FAILURE
                return
        else:
            self._refresh_file_monitor()

        if self._state() != State.DEAD:
            LOG.info("Waiting for server...")

        if self._configuration._use_json_sockets:
            request = json_rpc.Request(
                method="displayTypeErrors", parameters={"files": []}
            )
            self._send_and_handle_socket_request(request, self._version_hash)
        else:
            result = self._call_client(command=self.NAME)
            try:
                result.check()
                errors = self._get_errors(result)
                self._print(errors)

                if errors:
                    self._exit_code = ExitCode.FOUND_ERRORS
            except ClientException as exception:
                LOG.error("Error while waiting for server: %s", str(exception))
                LOG.error("Run `pyre restart` in order to restart the server.")
                self._exit_code = ExitCode.FAILURE

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

        if self._incremental_style == IncrementalStyle.TRANSITIVE:
            flags.append("-transitive")

        return flags

    def _read_stderr(self, _stream) -> None:
        stderr_file = os.path.join(self._log_directory, "server/server.stdout")
        with subprocess.Popen(
            ["tail", "--follow", "--lines=0", stderr_file],
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
        ) as stderr_tail:
            atexit.register(stderr_tail.terminate)
            super(Incremental, self)._read_stderr(cast(IO[bytes], stderr_tail.stdout))

    def _refresh_file_monitor(self) -> None:
        if not ProjectFilesMonitor.is_alive(self._configuration):
            LOG.info("File monitor is not running.")
            try:
                ProjectFilesMonitor(
                    self._configuration,
                    self._current_directory,
                    self._analysis_directory,
                ).daemonize()
                LOG.info("Restarted file monitor.")
            except MonitorException as exception:
                LOG.warning("Failed to restart file monitor: %s", exception)
