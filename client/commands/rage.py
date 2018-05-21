# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from .command import Command


class Rage(Command):
    NAME = "rage"

    def __init__(self, arguments, configuration, source_directory) -> None:
        super(Rage, self).__init__(arguments, configuration, source_directory)
        self._arguments.command = self.NAME

    def _run(self) -> None:
        self._call_client(command=self.NAME, capture_output=False).check()
