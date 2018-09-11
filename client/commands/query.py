# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import List

from .. import log
from .command import Command


class Query(Command):
    NAME = "query"

    def __init__(self, arguments, configuration, analysis_directory) -> None:
        self.query = arguments.query
        super(Query, self).__init__(arguments, configuration, analysis_directory)

    def _flags(self) -> List[str]:
        return [self.query]

    def _run(self) -> None:
        result = self._call_client(command=self.NAME)
        result.check()
        log.stdout.write(result.output)
