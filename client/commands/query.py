# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
import re
from typing import Dict, List, Optional

from .. import log
from .command import Command


LOG = logging.getLogger(__name__)


class Query(Command):
    NAME = "query"

    def _rewrite_paths(self, query: str) -> str:
        paths = re.findall(r"'[a-zA-Z_\-\.\/0-9]+\.py'", query)
        symbolic_link_mapping: Optional[Dict[str, str]] = None
        for path in paths:
            # Lazily compute the symbolic link mapping, as it can add over a second of
            # latency for large projects.
            if symbolic_link_mapping is None:
                symbolic_link_mapping = (
                    self._analysis_directory.compute_symbolic_links()
                )
            path = path[1:-1]
            absolute_path = os.path.abspath(path)
            if absolute_path in symbolic_link_mapping:
                query = query.replace(path, symbolic_link_mapping[absolute_path])
        return query

    def __init__(self, arguments, configuration, analysis_directory) -> None:
        super(Query, self).__init__(arguments, configuration, analysis_directory)
        self.query = self._rewrite_paths(arguments.query)

    def _flags(self) -> List[str]:
        return [self.query]

    def _run(self) -> None:
        result = self._call_client(command=self.NAME)
        result.check()
        log.stdout.write(result.output)
