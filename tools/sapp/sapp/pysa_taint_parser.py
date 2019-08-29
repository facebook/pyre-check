# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""Parse Pysa/Taint output for Zoncolan processing"""

import logging
from typing import IO, Any, Dict, Iterable, List, Tuple

import ujson as json

from . import errors
from .analysis_output import AnalysisOutput, Metadata
from .base_parser import (
    BaseParser,
    EntryPosition,
    ParseType,
    log_trace_keyerror_in_generator,
)


log = logging.getLogger("sapp")


class Parser(BaseParser):
    """The parser takes a json file as input, and provides a simplified output
    for the Processor.
    """

    def parse(self, input: AnalysisOutput) -> Iterable[Dict[str, Any]]:
        for handle in input.file_handles():
            for entry in self.parse_handle(handle):
                yield entry

    def parse_handle(self, handle) -> Iterable[Dict[str, Any]]:
        for entry in self._parse_basic(handle):
            yield from self._parse_by_type(entry)

    # Instead of returning the actual json from the AnalysisOutput, we return
    # location information so it can be retrieved later.
    def get_json_file_offsets(self, input: AnalysisOutput) -> Iterable[EntryPosition]:
        for handle in input.file_handles():
            for entry, position in self._parse_v2(handle):
                callable = self._get_callable(entry["data"].get("callable")).lstrip(
                    "\\"
                )
                yield EntryPosition(
                    callable=callable,
                    shard=position["shard"],
                    offset=position["offset"],
                )

    # Given a path and an offset, return the json in mostly-raw form.
    def get_json_from_file_offset(self, path: str, offset: int) -> Dict[str, Any]:
        with open(path) as fh:
            fh.seek(offset)
            return json.loads(fh.readline())

    def _parse_basic(self, handle: IO[str]) -> Iterable[Dict[str, Any]]:
        file_version = self._guess_file_version(handle)
        if file_version == 2:
            for entry, _ in self._parse_v2(handle):
                yield entry
        else:
            yield from self._parse_v1(handle)

    def _parse_v1(self, handle: IO[str]) -> Iterable[Dict[str, Any]]:
        data = json.load(handle)
        config = data["config"]
        self.repo_dir = config["repo"]
        results = data["results"]
        return results

    def _parse_v2(
        self, handle: IO[str]
    ) -> Iterable[Tuple[Dict[str, Any], Dict[str, int]]]:
        """Parse analysis in jsonlines format:
            { "file_version": 2, "config": <json> }
            { <error1> }
            { <error2> }
            ...
        """
        header = json.loads(handle.readline())
        assert header["file_version"] == 2

        shard = 0

        offset, line = handle.tell(), handle.readline()
        while line:
            entry = json.loads(line)
            if entry:
                position = {"shard": shard, "offset": offset}
                yield entry, position
            offset, line = handle.tell(), handle.readline()

    def _guess_file_version(self, handle: IO[str]) -> int:
        first_line = handle.readline()
        try:
            json_first_line = json.loads(first_line)
            version = json_first_line["file_version"]
        # Falling back on v1 for expected errors
        except KeyError as e:
            if e.args[0] != "file_version":
                raise
            version = 1
        except ValueError:
            version = 1
            pass

        handle.seek(0)
        return version

    def _parse_by_type(self, entry):
        if entry["kind"] == "model":
            yield from self._parse_model(entry["data"])
        elif entry["kind"] == "issue":
            yield from self._parse_issue(entry["data"])

    @staticmethod
    def _get_callable(callable):
        return callable

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
                    "sources": [
                        (kind, distance) for (_, kind, distance) in fragment["leaves"]
                    ],
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
                    "sinks": [
                        (kind, distance) for (_, kind, distance) in fragment["leaves"]
                    ],
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
                # Stripping the leaf_detail away for areas that
                #   only expect (leaf_kind, depth)
                new_fragment = fragment.copy()
                new_fragment["leaves"] = [
                    (kind, length) for (_, kind, length) in fragment["leaves"]
                ]
                fragments.append(new_fragment)
                # Leaf distances should be represented as:
                #   (leaf_detail, leaf_kind, depth)
                leaf_info = fragment["leaves"]
                leaf_distances.update(leaf_info)
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
            leaves = [(name, kind, length) for (name, kind, _) in leaves]

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
        return leaf.get("name", None)

    def _parse_leaves(self, leaves) -> List[Tuple[str, str, int]]:
        """Returns a list of pairs (leaf_name, leaf_kind, distance)"""
        return [(self._leaf_name(leaf), leaf["kind"], 0) for leaf in leaves]

    @staticmethod
    def is_supported(metadata: Metadata):
        return metadata.tool == "pysa"
