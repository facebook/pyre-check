# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging

from .command import ClientException, Command, State
from .kill import Kill


LOG = logging.getLogger(__name__)


class Stop(Command):
    NAME = "stop"

    def __init__(self, arguments, configuration, source_directory) -> None:
        super(Stop, self).__init__(arguments, configuration, source_directory)

    def _run(self) -> None:
        if self._state() == State.DEAD:
            LOG.info("No server running")
        else:
            try:
                self._call_client(command=self.NAME).check()
                LOG.info("Stopped server at `%s`", self._source_directory)
            except ClientException:
                LOG.info("Could not stop server, attempting to kill.")
                arguments = self._arguments
                arguments.with_fire = False
                Kill(arguments, self._configuration, self._source_directory).run()
