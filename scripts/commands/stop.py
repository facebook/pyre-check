# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging

from .command import (
    Command,
    State,
)
from .. import filesystem

LOG = logging.getLogger(__name__)


class Stop(Command):
    NAME = 'stop'

    def __init__(self, arguments, configuration, source_directory) -> None:
        super(Stop, self).__init__(arguments, configuration, source_directory)

    def _run(self) -> None:
        if self._state() == State.DEAD:
            LOG.info('No server running')
        else:
            self._call_client(command=self.NAME).check()
            LOG.info('Stopped server at `%s`', self._source_directory)
