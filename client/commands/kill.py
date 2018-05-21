# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
import signal
import subprocess
import traceback

from .. import BINARY_NAME
from ..filesystem import remove_if_exists
from .command import Command, State


LOG = logging.getLogger(__name__)


class Kill(Command):
    NAME = "kill"

    def __init__(self, arguments, configuration, source_directory) -> None:
        super(Kill, self).__init__(arguments, configuration, source_directory)
        self.with_fire = arguments.with_fire

    def _run(self) -> None:
        if self.with_fire:
            self._kill_all_processes()
            return

        state = self._state()
        if state == State.RUNNING:
            self._kill(os.path.join(self._source_directory, ".pyre/server/server.pid"))
            remove_if_exists(
                os.path.join(self._source_directory, ".pyre/server/server.lock")
            )
            remove_if_exists(
                os.path.join(self._source_directory, ".pyre/server/server.sock")
            )
            self._kill(
                os.path.join(self._source_directory, ".pyre/watchman/watchman.pid")
            )
            remove_if_exists(
                os.path.join(self._source_directory, ".pyre/watchman/watchman.lock")
            )
            LOG.info("Terminated server at `%s`", self._source_directory)
        else:
            LOG.warning("No server running")

    def _kill(self, path) -> None:
        if not os.path.exists(path):
            LOG.debug("No process running at `%s`", path)
            return

        try:
            LOG.debug("Terminating process at `%s`", path)
            with open(path, "r") as file:
                pid = int(file.read())
                if pid > 0:
                    os.kill(int(pid), signal.SIGTERM)
                os.remove(path)
        except (IOError, OSError) as error:
            LOG.error("Encountered error during kill: %s", str(error))
            LOG.debug(traceback.format_exc())

    def _kill_all_processes(self) -> None:
        """
            Kills all processes that have the same binary as the one specified
            in the # configuration.
        """
        subprocess.run(["pkill", "-f", "{}".format(BINARY_NAME)])
