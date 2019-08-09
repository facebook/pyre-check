# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
import time
from typing import List

from ..project_files_monitor import Monitor
from .command import ClientException, Command, State
from .kill import Kill


LOG = logging.getLogger(__name__)


class Stop(Command):
    NAME = "stop"

    def __init__(self, arguments, configuration, analysis_directory) -> None:
        super(Stop, self).__init__(arguments, configuration, analysis_directory)

    def _flags(self) -> List[str]:
        return []

    def _run(self) -> None:
        def _kill():
            arguments = self._arguments
            arguments.with_fire = False
            Kill(arguments, self._configuration, self._analysis_directory).run()

        if self._state() == State.DEAD:
            LOG.warning("No server running, cleaning up any left over Pyre processes.")
            _kill()
        else:
            try:
                with open(
                    os.path.join(
                        self._analysis_directory.get_pyre_server_directory(),
                        "server.pid",
                    )
                ) as pid_file:
                    pid_to_poll = int(pid_file.read())
            except (OSError, ValueError):
                pid_to_poll = None
            try:
                stopped = False
                # If this call fails, check() will throw a ClientException.
                self._call_client(command=self.NAME).check()
                # Poll for a second to ensure that the server has a chance to exit.
                if pid_to_poll is not None:
                    stop_time = time.time() + 1.0
                    LOG.info("Polling for server's process to stop...")
                    while time.time() < stop_time:
                        # send a null signal to validate the process's existence. If the
                        # process has terminated, a ProcessLookupError will be thrown.
                        os.kill(pid_to_poll, 0)
                        time.sleep(0.1)
            except ClientException:
                # An error was encountered when running `pyre stop`.
                stopped = False
            except ProcessLookupError:
                LOG.info("The server process has stopped.")
                stopped = True

            if not stopped:
                LOG.warning("Could not stop server, attempting to kill.")
                _kill()
            else:
                LOG.info("Stopped server at `%s`", self._analysis_directory.get_root())

        try:
            pid_path = Monitor.pid_path(self._analysis_directory.get_root())
            with open(pid_path) as file:
                pid = int(file.read())
                os.kill(pid, 2)  # sigint
        except (FileNotFoundError, OSError, ValueError):
            pass
