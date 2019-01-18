#!/usr/bin/env python3
"""Abstract Parser for Zoncolan like output"""

import logging
import os
import pprint
from enum import Enum
from typing import Any, Dict, Iterable, TextIO

import xxhash  # pyre-ignore
from tools.sapp.analysis_output import AnalysisOutput


log = logging.getLogger()


class ParseType(Enum):
    ISSUE = "issue"
    PRECONDITION = "precondition"
    POSTCONDITION = "postcondition"


def log_trace_keyerror(func):
    def wrapper(self, json):
        try:
            return func(self, json)
        except KeyError:
            # The most common problem with parsing json is not finding
            # a field you expect, so we'll catch those and log them, but move
            # on.
            log.exception(
                "Unable to parse trace for the following:\n%s", pprint.pformat(json)
            )
            return ([], {})

    return wrapper


def log_trace_keyerror_in_generator(func):
    def wrapper(self, json):
        try:
            yield from func(self, json)
        except KeyError:
            # The most common problem with parsing json is not finding
            # a field you expect, so we'll catch those and log them, but move
            # on.
            log.exception(
                "Unable to parse trace for the following:\n%s", pprint.pformat(json)
            )
            return
            yield

    return wrapper


class BaseParser(object):
    """The parser takes a json file as input, and provides a simplified output
    for the Processor.
    """

    def __init__(self, repo_dir=None, extractor=None):
        self.repo_dir = os.path.realpath(repo_dir) if repo_dir else None
        self.extractor = extractor
        self.version = None

    def get_version(self):
        return self.version

    # @abstractmethod
    def parse(self, input: AnalysisOutput) -> Iterable[Dict[str, Any]]:
        """Must return objects with a 'type': ParseType field.
        """
        assert False, "Abstract method called!"
        return
        yield

    # @abstractmethod
    def parse_handle(self, handle: TextIO) -> Iterable[Dict[str, Any]]:
        """Must return objects with a 'type': ParseType field.
        """
        assert False, "Abstract method called!"
        return
        yield

    @staticmethod
    def compute_master_handle(callable, line, start, end, code):
        key = "{callable}:{line}|{start}|{end}:{code}".format(
            callable=callable, line=line, start=start, end=end, code=code
        )
        return BaseParser.compute_handle_from_key(key)

    @staticmethod
    def compute_diff_handle(filename, old_line, code):
        """Uses the absolute line and ignores the callable/character offsets.
        Used only in determining whether new issues are old issues.
        """
        key = "{filename}:{old_line}:{code}".format(
            filename=filename, old_line=old_line, code=code
        )
        return BaseParser.compute_handle_from_key(key)

    @staticmethod
    def compute_handle_from_key(key):
        hash_gen = xxhash.xxh64()
        hash_gen.update(key)
        hash_ = hash_gen.hexdigest()
        return key[: 255 - len(hash_) - 1] + ":" + hash_
