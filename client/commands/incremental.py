# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import atexit
import logging
import os
import subprocess
import sys
from typing import List

from .command import ClientException, ExitCode, State
from .reporting import Reporting
from .start import Start


LOG = logging.getLogger(__name__)


class Incremental(Reporting):
    NAME = "incremental"

    def __init__(self, arguments, configuration, analysis_directory) -> None:
        super(Incremental, self).__init__(arguments, configuration, analysis_directory)

    def _run(self) -> None:
        if self._state() == State.DEAD:
            LOG.warning("Starting server at `%s`.", self._analysis_directory.get_root())
            arguments = self._arguments
            arguments.terminal = False
            arguments.no_watchman = False
            Start(arguments, self._configuration, self._analysis_directory).run()

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
            arguments = sys.argv[:-1] if sys.argv[-1] == "incremental" else sys.argv
            LOG.error(
                "Run `%s restart` in order to restart the server.", " ".join(arguments)
            )
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

        return flags

    # pyre-ignore: T31696900
    def _read_stderr(self, _stream, analysis_directory) -> None:
        stderr_file = os.path.join(analysis_directory, ".pyre/server/server.stdout")
        with subprocess.Popen(
            ["tail", "-f", stderr_file],
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
        ) as stderr_tail:
            atexit.register(stderr_tail.terminate)
            super(Incremental, self)._read_stderr(
                stderr_tail.stdout, analysis_directory
            )
