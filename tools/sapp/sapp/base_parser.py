#!/usr/bin/env python3
"""Abstract Parser for Zoncolan like output"""

import logging
import os
import pprint
from collections import defaultdict
from enum import Enum
from typing import Any, Dict, Iterable, List, Set, TextIO, Tuple

import xxhash

from .analysis_output import AnalysisOutput
from .pipeline import DictEntries, InputFiles, Optional, PipelineStep, Summary


# if these imports have the same name we get a linter error
try:
    import ujson as json
except ImportError:
    import json  # noqa


log = logging.getLogger("sapp")


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


class BaseParser(PipelineStep[InputFiles, DictEntries]):
    """The parser takes a json file as input, and provides a simplified output
    for the Processor.
    """

    def __init__(self, repo_dir=None):
        self.repo_dir = os.path.realpath(repo_dir) if repo_dir else None
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

    def _analysis_output_to_parsed_types(
        self, input: AnalysisOutput
    ) -> Iterable[Tuple[ParseType, Any, Dict[str, Any]]]:
        entries = self.parse(input)

        for e in entries:
            typ = e["type"]
            if typ == ParseType.ISSUE:
                key = e["handle"]
            elif e["type"] == ParseType.PRECONDITION:
                key = (e["caller"], e["caller_port"])
            elif e["type"] == ParseType.POSTCONDITION:
                key = (e["caller"], e["caller_port"])
            yield typ, key, e

    def analysis_output_to_dict_entries(
        self,
        inputfile: AnalysisOutput,
        previous_inputfile: Optional[AnalysisOutput],
        previous_issue_handles: Optional[AnalysisOutput],
        linemapfile: Optional[str],
    ) -> DictEntries:
        """Here we take input generators and return a dict with issues,
        preconditions, and postconditions separated. If there is only a single
        generator file, it's simple. If we also pass in a generator from a
        previous inputfile then there are a couple extra steps:

        1. If an issue was seen in the previous inputfile then we won't return
        it, because it's not new.
        2. In addition, we take an optional linemap file that maps for each
        filename, each new file line position to a list of old file line
        position. This is used to adjust handles to we can recognize when issues
        moved.
        """

        issues = []
        previous_handles: Set[str] = set()
        conditions: Dict[ParseType, Dict[str, List[Dict[str, Any]]]] = {
            ParseType.PRECONDITION: defaultdict(list),
            ParseType.POSTCONDITION: defaultdict(list),
        }

        # If we have a mapfile, create the map.
        if linemapfile:
            log.info("Parsing linemap file")
            with open(linemapfile, "r") as f:
                linemap = json.load(f)
        else:
            linemap = None

        # Save entry info from the parent analysis, if there is one.
        # If previous issue handles file is provided, use it over
        # previous_inputfile (contains the full JSON)
        if previous_issue_handles:
            log.info("Parsing previous issue handles")
            for f in previous_issue_handles.file_handles():
                handles = f.read().splitlines()
                previous_handles = {handle for handle in handles}
        elif previous_inputfile:
            log.info("Parsing previous hh_server output")
            for typ, master_key, e in self._analysis_output_to_parsed_types(
                previous_inputfile
            ):
                if typ == ParseType.ISSUE:
                    diff_handle = BaseParser.compute_diff_handle(
                        e["filename"], e["line"], e["code"]
                    )
                    previous_handles.add(diff_handle)
                    # Use exact handle match too in case linemap is missing.
                    previous_handles.add(master_key)

        log.info("Parsing hh_server output")
        for typ, key, e in self._analysis_output_to_parsed_types(inputfile):
            if typ == ParseType.ISSUE:
                # We are only interested in issues that weren't in the previous
                # analysis.
                if not self._is_existing_issue(linemap, previous_handles, e, key):
                    issues.append(e)
            else:
                conditions[typ][key].append(e)

        return {
            "issues": issues,
            "preconditions": conditions[ParseType.PRECONDITION],
            "postconditions": conditions[ParseType.POSTCONDITION],
        }

    def _is_existing_issue(self, linemap, old_handles, new_issue, new_handle):
        if new_handle in old_handles:
            return True
        if not linemap:
            return False
        filename = new_issue["filename"]
        old_map = linemap.get(filename, {})
        old_lines = old_map.get(str(new_issue["line"]), [])
        # Once this works, we should remove the "relative" line from the handle
        # and use the absolute one to avoid having to map both the start of the
        # method and the line in the method.

        # Consider all possible old lines
        for old_line in old_lines:
            old_handle = BaseParser.compute_diff_handle(
                filename, old_line, new_issue["code"]
            )
            if old_handle in old_handles:
                return True
        return False

    def run(self, input: InputFiles, summary: Summary) -> Tuple[DictEntries, Summary]:
        inputfile, previous_inputfile = input

        return (
            self.analysis_output_to_dict_entries(
                inputfile,
                previous_inputfile,
                summary.get("previous_issue_handles"),
                summary.get("old_linemap_file"),
            ),
            summary,
        )

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
