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

    def __init__(self, arguments, configuration, analysis_directory) -> None:
        super(Restart, self).__init__(arguments, configuration, analysis_directory)

    def _run(self) -> None:
        Stop(self._arguments, self._configuration, self._analysis_directory).run()
        # Force the incremental run to be blocking.
        self._arguments.nonblocking = False
        Incremental(
            self._arguments, self._configuration, self._analysis_directory
        ).run()
