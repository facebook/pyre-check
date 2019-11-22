# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
from multiprocessing import Pool
from typing import Any, Dict, Iterable

from .analysis_output import AnalysisOutput
from .base_parser import BaseParser


log: logging.Logger = logging.getLogger("sapp")
logging.basicConfig(format="%(asctime)s [%(levelname)s] %(message)s")


# We are going to call this per process, so we need to pass in and return
# serializable data. And as a single arg, as far as I can tell. Which is why the
# args type looks so silly.
def parse(args):
    (base_parser, repo_dir, metadata), path = args

    parser = base_parser(repo_dir)
    parser.initialize(metadata)

    with open(path) as handle:
        return list(parser.parse_handle(handle))


class ParallelParser(BaseParser):
    def __init__(self, parser_class, repo_dir=None) -> None:
        super().__init__(repo_dir)
        self.parser = parser_class

    def parse(self, input: AnalysisOutput) -> Iterable[Dict[str, Any]]:
        log.info("Parsing in parallel")
        files = list(input.file_names())

        # Pair up the arguments with each file.
        args = zip([(self.parser, self.repo_dir, input.metadata)] * len(files), files)

        with Pool(processes=None) as pool:
            for f in pool.imap_unordered(parse, args):
                yield from f
