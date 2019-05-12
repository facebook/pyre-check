#!/usr/bin/env python3
"""Parse Pysa/Taint output for Zoncolan processing"""

import logging
from typing import Any, Dict, Iterable, List, Tuple

import ujson as json

from . import errors
from .analysis_output import AnalysisOutput
from .base_parser import BaseParser, ParseType, log_trace_keyerror_in_generator


log = logging.getLogger("sapp")


class Parser(BaseParser):
    """The parser takes a json file as input, and provides a simplified output
    for the Processor.
    """

    def __init__(self, repo_dir=None):
        super(Parser, self).__init__(repo_dir)

    def parse_raw(self, input):
        return self._parse(input)

    def parse(self, input: AnalysisOutput) -> Iterable[Dict[str, Any]]:
        for handle in input.file_handles():
            for entry in self.parse_handle(handle):
                yield entry

    def parse_handle(self, handle) -> Iterable[Dict[str, Any]]:
        data = json.load(handle)
        config = data["config"]
        self.repo_dir = config["repo"]
        results = data["results"]
        for entry in results:
            yield from self._parse_by_type(entry)

    def _parse_by_type(self, entry):
        if entry["kind"] == "model":
            yield from self._parse_model(entry["data"])
        elif entry["kind"] == "issue":
            yield from self._parse_issue(entry["data"])

    @log_trace_keyerror_in_generator
    def _parse_model(self, json):
        callable = json["callable"]
        yield from self._parse_model_sources(callable, json["sources"])
        yield from self._parse_model_sinks(callable, json["sinks"])

    def _parse_model_sources(self, callable, source_traces):
        for source_trace in source_traces:
            port = source_trace["port"]
            for fragment in self._parse_trace_fragments(
                "source", source_trace["taint"]
            ):
                yield {
                    "type": ParseType.POSTCONDITION,
                    "caller": callable,
                    "callee": fragment["callee"],
                    "callee_location": fragment["location"],
                    "filename": fragment["location"]["filename"],
                    "sources": list(fragment["leaves"]),
                    "caller_port": port,
                    "callee_port": fragment["port"],
                    "type_interval": {},
                    "features": [],
                }

    def _parse_model_sinks(self, callable, sink_traces):
        for sink_trace in sink_traces:
            port = sink_trace["port"]
            for fragment in self._parse_trace_fragments("sink", sink_trace["taint"]):
                yield {
                    "type": ParseType.PRECONDITION,
                    "caller": callable,
                    "callee": fragment["callee"],
                    "callee_location": fragment["location"],
                    "filename": fragment["location"]["filename"],
                    "titos": fragment["titos"],
                    "sinks": list(fragment["leaves"]),
                    "caller_port": port,
                    "callee_port": fragment["port"],
                    "type_interval": {},
                    "features": [],
                }

    @log_trace_keyerror_in_generator
    def _parse_issue(self, json):
        issue = {}

        issue["type"] = ParseType.ISSUE
        issue["code"] = json["code"]
        issue["line"] = json["line"]
        issue["callable_line"] = json["callable_line"]
        issue["start"] = json["start"]
        issue["end"] = json["end"]
        issue["callable"] = json["callable"]
        issue["handle"] = self._generate_issue_master_handle(issue)
        issue["message"] = json["message"]
        issue["filename"] = self._extract_filename(json["filename"])

        issue["preconditions"], issue[
            "final_sinks"
        ], bw_features = self._parse_issue_traces(json["traces"], "backward", "sink")
        issue["postconditions"], issue[
            "initial_sources"
        ], fw_features = self._parse_issue_traces(json["traces"], "forward", "source")
        issue["features"] = bw_features + fw_features
        yield issue

    def _generate_issue_master_handle(self, issue):
        line = issue["line"] - issue["callable_line"]
        return self.compute_master_handle(
            callable=issue["callable"],
            line=line,
            start=issue["start"],
            end=issue["end"],
            code=issue["code"],
        )

    def _extract_filename(self, complete_filename):
        repo_dir = self.repo_dir
        if repo_dir is None:
            return complete_filename
        if not complete_filename.startswith("/"):
            # already relative
            return complete_filename
        if not complete_filename.startswith(repo_dir):
            raise errors.AIException(
                "Expected filename ({}) to start with repo_dir ({}). "
                "Check the --repo-dir option.".format(complete_filename, repo_dir)
            )
        repo_dir = repo_dir.rstrip("/")
        if repo_dir == "":
            return complete_filename
        return complete_filename[len(repo_dir) + 1 :]

    def _parse_issue_traces(self, traces, name, leaf_port):
        for trace in traces:
            if trace["name"] == name:
                return self._parse_issue_trace_fragments(leaf_port, trace["roots"])
        return ([], set(), set())

    def _get_position_or_default(self, json):
        return json.get("position", {"line": 0, "start": 0, "end": 0})

    def _parse_issue_trace_fragments(self, leaf_port, traces):
        fragments = []
        leaf_distances = set()
        all_features = []

        for trace in traces:
            for fragment in self._parse_trace_fragment(leaf_port, trace):
                fragments.append(fragment)
                leaf_distances.update(fragment["leaves"])
                all_features.extend(fragment["features"])

        return (fragments, leaf_distances, all_features)

    def _parse_trace_fragments(self, leaf_port, traces):
        for trace in traces:
            yield from self._parse_trace_fragment(leaf_port, trace)

    def _parse_trace_fragment(self, leaf_port, trace):
        # For now we don't have leaf distances.
        leaves = self._parse_leaves(trace.get("leaves", []))
        if "root" in trace:
            yield {
                "callee": "leaf",
                "port": leaf_port,
                "location": trace["root"],
                "leaves": leaves,
                "titos": trace.get("tito", []),
                "features": trace.get("features", []),
                "type_interval": {},
            }
        elif "call" in trace:
            call = trace["call"]
            location = call["position"]
            port = call["port"]
            resolves_to = call.get("resolves_to", [])
            length = call.get("length", 0)
            leaves = [(leaf, length) for (leaf, _) in leaves]

            for resolved in resolves_to:
                yield {
                    "callee": resolved,
                    "port": port,
                    "location": location,
                    "leaves": leaves,
                    "titos": trace.get("tito", []),
                    "features": trace.get("features", []),
                    "type_interval": {},
                }

    def _leaf_name(self, leaf) -> str:
        return leaf.get("name", leaf["kind"])

    def _parse_leaves(self, leaves) -> List[Tuple[str, int]]:
        """Returns a list of pairs (leaf_name, distance)"""
        return [(self._leaf_name(leaf), 0) for leaf in leaves]
