# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from .. import SUCCESS, log
from .command import Command


class Query(Command):
    NAME = "query"

    def __init__(self, arguments, configuration, source_directory) -> None:
        self.query = arguments.query
        super(Query, self).__init__(arguments, configuration, source_directory)

    def _flags(self):
        return [self.query]

    def _run(self) -> int:
        result = self._call_client(command=self.NAME, flags=self._flags())
        result.check()
        log.stdout.write(result.output)
        return SUCCESS
