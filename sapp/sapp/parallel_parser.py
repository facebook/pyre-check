#!/usr/bin/env python3

import logging
from multiprocessing import Pool

from sapp.base_parser import BaseParser


log: logging.Logger = logging.getLogger()
logging.basicConfig(format="%(asctime)s [%(levelname)s] %(message)s")

# We are going to call this per process, so we need to pass in and return
# serializable data. And as a single arg, as far as I can tell. Which is why the
# args type looks so silly.
def parse(args):
    (base_parser, repo_dir, extractor), path = args
    with open(path) as handle:
        return list(base_parser(repo_dir, extractor).parse_handle(handle))


class ParallelParser(BaseParser):
    def __init__(self, parser_class, repo_dir=None, extractor=None) -> None:
        super().__init__(repo_dir, extractor)
        self.parser = parser_class

    def parse(self, input):
        log.info("Parsing in parallel")
        files = list(input.file_names())

        # Pair up the arguments with each file.
        args = zip([(self.parser, self.repo_dir, self.extractor)] * len(files), files)

        with Pool(processes=None) as pool:
            for f in pool.imap_unordered(parse, args):
                yield from f
