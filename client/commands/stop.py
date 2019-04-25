# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
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
                self._call_client(command=self.NAME).check()
                LOG.info("Stopped server at `%s`", self._analysis_directory.get_root())
            except ClientException:
                LOG.warning("Could not stop server, attempting to kill.")
                _kill()

        try:
            pid_path = Monitor.pid_path(self._analysis_directory.get_root())
            with open(pid_path) as file:
                pid = int(file.read())
                os.kill(pid, 2)  # sigint
        except (FileNotFoundError, OSError, ValueError):
            pass
