# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import atexit
import logging
import os
import subprocess
import sys
from typing import IO, List, cast

from ..project_files_monitor import Monitor, MonitorException
from .command import ClientException, ExitCode, State
from .reporting import Reporting
from .start import Start


LOG = logging.getLogger(__name__)


class Incremental(Reporting):
    NAME = "incremental"

    def __init__(self, arguments, configuration, analysis_directory) -> None:
        super(Incremental, self).__init__(arguments, configuration, analysis_directory)
        self._nonblocking = arguments.nonblocking  # type: bool

    def _run(self) -> None:
        if self._state() == State.DEAD:
            LOG.warning("Starting server at `%s`.", self._analysis_directory.get_root())
            arguments = self._arguments
            arguments.terminal = False
            arguments.store_type_check_resolution = False
            arguments.no_watchman = False
            exit_code = (
                Start(arguments, self._configuration, self._analysis_directory)
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

    def _flags(self) -> List[str]:
        flags = super()._flags()
        flags.extend(
            [
                "-typeshed",
                self._configuration.typeshed,
                "-expected-binary-version",
                self._configuration.version_hash,
            ]
        )

        search_path = self._configuration.search_path
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

    def _read_stderr(self, _stream) -> None:
        stderr_file = os.path.join(
            self._analysis_directory.get_root(), ".pyre/server/server.stdout"
        )
        with subprocess.Popen(
            ["tail", "--follow", "--lines=0", stderr_file],
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
        ) as stderr_tail:
            atexit.register(stderr_tail.terminate)
            super(Incremental, self)._read_stderr(cast(IO[bytes], stderr_tail.stdout))

    def _refresh_file_monitor(self) -> None:
        if not Monitor.is_alive(self._analysis_directory.get_root()):
            LOG.info("File monitor is not running.")
            try:
                Monitor(
                    self._arguments, self._configuration, self._analysis_directory
                ).daemonize()
                LOG.info("Restarted file monitor.")
            except MonitorException as exception:
                LOG.warning("Failed to restart file monitor: %s", exception)
