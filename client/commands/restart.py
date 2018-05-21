# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


from .command import Command
from .incremental import Incremental
from .start import Start
from .stop import Stop


class Restart(Command):
    NAME = "restart"

    def __init__(
        self, arguments, configuration, source_directory, blocking=True
    ) -> None:
        super(Restart, self).__init__(arguments, configuration, source_directory)
        self._blocking = blocking

    def _run(self) -> None:
        Stop(self._arguments, self._configuration, self._source_directory).run()
        if self._blocking:
            Incremental(
                self._arguments, self._configuration, self._source_directory
            ).run()
        else:
            Start(self._arguments, self._configuration, self._source_directory).run()
