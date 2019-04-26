#!/usr/bin/env python3

import datetime
import logging
import os
from collections import defaultdict
from typing import Any, Dict, List, Optional, Tuple

import ujson as json

from .models import (
    DBID,
    SHARED_TEXT_LENGTH,
    Issue,
    IssueDBID,
    IssueInstance,
    IssueInstanceFixInfo,
    IssueStatus,
    Run,
    RunStatus,
    SharedText,
    SharedTextKind,
    SourceLocation,
    TraceFrame,
    TraceFrameAnnotation,
    TraceKind,
)
from .pipeline import DictEntries, PipelineStep, Summary
from .trace_graph import TraceGraph


log = logging.getLogger("sapp")


class ModelGenerator(PipelineStep[DictEntries, TraceGraph]):
    def __init__(self) -> None:
        super().__init__()
        self.summary: Summary
        self.graph: TraceGraph

    def run(self, input: DictEntries, summary: Summary) -> Tuple[TraceGraph, Summary]:
        self.summary = summary

        self.summary["precondition_entries"] = defaultdict(
            list
        )  # Dict[Tuple[str, str], Any]
        self.summary["postcondition_entries"] = defaultdict(
            list
        )  # Dict[Tuple[str, str], Any]
        self.summary["missing_preconditions"] = set()  # Set[Tuple[str, str]]
        self.summary["missing_postconditions"] = set()  # Set[Tuple[str, str]]
        self.summary["bad_preconditions"] = set()  # Set[Tuple[str, str, int]]

        self.graph = TraceGraph()
        self.summary["run"] = self._create_empty_run(status=RunStatus.INCOMPLETE)
        self.summary["run"].id = DBID()

        self.summary["precondition_entries"] = input["preconditions"]
        self.summary["postcondition_entries"] = input["postconditions"]
        callables = self._compute_callables_count(input)

        log.info("Generating instances")
        for entry in input["issues"]:
            self._generate_issue(self.summary["run"], entry, callables)

        if self.summary.get("store_unused_models"):
            for _key, entries in self.summary["postcondition_entries"].items():
                for entry in entries:
                    self._generate_postcondition(self.summary["run"], entry)

            for _key, entries in self.summary["precondition_entries"].items():
                for entry in entries:
                    self._generate_precondition(self.summary["run"], entry)

        return self.graph, self.summary

    def _compute_callables_count(self, iters: Dict[str, Any]):
        """Iterate over all issues and count the number of times each callable
        is seen."""
        count = dict.fromkeys([issue["callable"] for issue in iters["issues"]], 0)
        for issue in iters["issues"]:
            count[issue["callable"]] += 1

        return count

    def _create_empty_run(
        self, status=RunStatus.FINISHED, status_description=None
    ) -> Run:
        """setting boilerplate when creating a Run object"""
        run = Run(
            job_id=self.summary["job_id"],
            issue_instances=[],
            date=datetime.datetime.now(),
            status=status,
            status_description=status_description,
            repository=self.summary["repository"],
            branch=self.summary["branch"],
            commit_hash=self.summary["commit_hash"],
            kind=self.summary["run_kind"],
        )
        return run

    def _get_minimum_trace_length(self, entries: List[Dict]) -> int:
        length = None
        for entry in entries:
            for (_leaf, depth) in entry["leaves"]:
                if length is None or length > depth:
                    length = depth
        if length is not None:
            return length
        return 0

    def _generate_issue(self, run, entry, callablesCount):
        """Insert the issue instance into a run. This includes creating (for
        new issues) or finding (for existing issues) Issue objects to associate
        with the instances.
        Also create sink entries and associate related issues"""

        trace_frames = []

        for p in entry["preconditions"]:
            tf = self._generate_issue_precondition(run, entry, p)
            trace_frames.append(tf)

        for p in entry["postconditions"]:
            tf = self._generate_issue_postcondition(run, entry, p)
            trace_frames.append(tf)

        features = set()
        for f in entry["features"]:
            features.update(self._generate_issue_feature_contents(entry, f))

        callable = entry["callable"]
        handle = self._get_issue_handle(entry)
        # TODO: record depth in issue_sink and issue_source assoc, but this can
        # be different per instance, so should be stored with the instance.
        initial_sources = {
            self._get_shared_text(SharedTextKind.SOURCE, s)
            for (s, _) in entry["initial_sources"]
        }
        final_sinks = {
            self._get_shared_text(SharedTextKind.SINK, s)
            for (s, _) in entry["final_sinks"]
        }

        issue = Issue.Record(
            id=IssueDBID(),
            code=entry["code"],
            handle=handle,
            status=IssueStatus.UNCATEGORIZED,
            first_seen=run.date,
            run_id=run.id,
        )

        self.graph.add_issue(issue)

        fix_info = None
        fix_info_id = None
        if entry.get("fix_info") is not None:
            fix_info = IssueInstanceFixInfo.Record(
                id=DBID(), fix_info=json.dumps(entry["fix_info"])
            )
            fix_info_id = fix_info.id

        message = self._get_shared_text(SharedTextKind.MESSAGE, entry["message"])
        filename_record = self._get_shared_text(
            SharedTextKind.FILENAME, entry["filename"]
        )
        callable_record = self._get_shared_text(SharedTextKind.CALLABLE, callable)

        instance = IssueInstance.Record(
            id=DBID(),
            issue_id=issue.id,
            location=self.get_location(entry),
            filename_id=filename_record.id,
            callable_id=callable_record.id,
            run_id=run.id,
            fix_info_id=fix_info_id,
            message_id=message.id,
            rank=0,
            min_trace_length_to_sources=self._get_minimum_trace_length(
                entry["postconditions"]
            ),
            min_trace_length_to_sinks=self._get_minimum_trace_length(
                entry["preconditions"]
            ),
            callable_count=callablesCount[callable],
        )

        for sink in final_sinks:
            self.graph.add_issue_instance_shared_text_assoc(instance, sink)
        for source in initial_sources:
            self.graph.add_issue_instance_shared_text_assoc(instance, source)

        if fix_info is not None:
            self.graph.add_issue_instance_fix_info(instance, fix_info)

        for trace_frame in trace_frames:
            self.graph.add_issue_instance_trace_frame_assoc(instance, trace_frame)

        for feature in features:
            feature = self._get_shared_text(SharedTextKind.FEATURE, feature)
            self.graph.add_issue_instance_shared_text_assoc(instance, feature)

        self.graph.add_issue_instance(instance)

    def _generate_issue_postcondition(self, run, issue, callinfo):
        # Generates a synthetic postcondition from the forward_trace in callinfo
        # that represents a call edge from the issue callable to the start of a
        # forward trace.
        # Generate all dependencies of this postcondition as well.
        caller = issue["callable"]
        callee = callinfo["callee"]
        callee_port = callinfo["port"]
        call_tf = self._generate_raw_postcondition(
            run,
            issue["filename"],
            caller,
            "root",
            callee,
            callee_port,
            callinfo["location"],
            callinfo["leaves"],  # sources
            callinfo["type_interval"],
        )
        keys = [(call_tf.callee_id, callee_port)]
        while len(keys) > 0:
            key = keys.pop()
            if self.graph.has_postconditions_with_caller(key[0], key[1]):
                continue

            key = (self.graph.get_text(key[0]), key[1])
            new = [
                self._generate_postcondition(run, e)
                for e in self.summary["postcondition_entries"].pop(key, [])
            ]
            if len(new) == 0 and key[1] != "source":
                self.summary["missing_postconditions"].add(key)

            keys.extend([(tf.callee_id, tf.callee_port) for tf in new])

        return call_tf

    def _generate_postcondition(self, run, entry):
        callee_location = entry["callee_location"]
        assert "caller_port" in entry, str(entry)
        assert "callee_port" in entry, str(entry)

        return self._generate_raw_postcondition(
            run,
            filename=entry["filename"],
            caller=entry["caller"],
            caller_port=entry["caller_port"],
            callee=entry["callee"],
            callee_port=entry["callee_port"],
            callee_location=callee_location,
            sources=entry["sources"],
            type_interval=entry["type_interval"],
        )

    def _generate_raw_postcondition(
        self,
        run,
        filename,
        caller,
        caller_port,
        callee,
        callee_port,
        callee_location,
        sources,
        type_interval,
    ):
        lb, ub, preserves_type_context = self._get_interval(type_interval)

        caller_record = self._get_shared_text(SharedTextKind.CALLABLE, caller)
        callee_record = self._get_shared_text(SharedTextKind.CALLABLE, callee)
        filename_record = self._get_shared_text(SharedTextKind.FILENAME, filename)
        trace_frame = TraceFrame.Record(
            id=DBID(),
            kind=TraceKind.POSTCONDITION,
            caller_id=caller_record.id,
            callee_id=callee_record.id,
            callee_location=SourceLocation(
                callee_location["line"],
                callee_location["start"],
                callee_location["end"],
            ),
            filename_id=filename_record.id,
            run_id=run.id,
            caller_port=caller_port,
            callee_port=callee_port,
            preserves_type_context=preserves_type_context,
            type_interval_lower=lb,
            type_interval_upper=ub,
            migrated_id=None,
            titos=[],
        )

        for (source, depth) in sources:
            source_record = self._get_shared_text(SharedTextKind.SOURCE, source)
            self.graph.add_trace_frame_leaf_assoc(trace_frame, source_record, depth)

        self.graph.add_trace_frame(trace_frame)
        return trace_frame

    def _generate_issue_precondition(self, run, issue, callinfo):
        # Generates a synthetic precondition from the backward_trace in callinfo
        # that represents a call edge from the issue callable to the start of a
        # backward trace.
        # Generate all dependencies of this precondition as well.
        caller = issue["callable"]
        callee = callinfo["callee"]
        callee_port = callinfo["port"]
        call_tf = self._generate_raw_precondition(
            run,
            issue["filename"],
            caller,
            "root",
            callee,
            callee_port,
            callinfo["location"],
            [],  # titos
            callinfo["leaves"],  # sinks
            callinfo["type_interval"],
            callinfo["features"],
        )
        keys = [(call_tf.callee_id, callee_port)]
        while len(keys) > 0:
            key = keys.pop()
            if self.graph.has_preconditions_with_caller(key[0], key[1]):
                continue

            key = (self.graph.get_text(key[0]), key[1])
            new = [
                self._generate_precondition(run, e)
                for e in self.summary["precondition_entries"].pop(key, [])
            ]
            if len(new) == 0 and key[1] != "sink":
                self.summary["missing_preconditions"].add(key)

            keys.extend([(tf.callee_id, tf.callee_port) for tf in new])

        return call_tf

    def _generate_precondition(self, run, entry):
        callee_location = entry["callee_location"]

        titos = [
            SourceLocation(t["line"], t["start"], t["end"])
            for t in entry.get("titos", [])
        ]
        if len(titos) > 200:
            pre_key: Tuple[str, str, int] = (
                entry["filename"],
                entry["caller"],
                len(titos),
            )
            if pre_key not in self.summary["bad_preconditions"]:
                log.info("Bad Precondition: %s", str(pre_key))
                self.summary["bad_preconditions"].add(pre_key)
            titos = titos[:200]

        return self._generate_raw_precondition(
            run,
            filename=entry["filename"],
            caller=entry["caller"],
            caller_port=entry["caller_port"],
            callee=entry["callee"],
            callee_port=entry["callee_port"],
            callee_location=callee_location,
            titos=titos,
            sinks=entry["sinks"],
            type_interval=entry["type_interval"],
            features=entry["features"],
        )

    def _generate_raw_precondition(
        self,
        run,
        filename,
        caller,
        caller_port,
        callee,
        callee_port,
        callee_location,
        titos,
        sinks,
        type_interval,
        features,
    ):
        lb, ub, preserves_type_context = self._get_interval(type_interval)
        caller_record = self._get_shared_text(SharedTextKind.CALLABLE, caller)
        callee_record = self._get_shared_text(SharedTextKind.CALLABLE, callee)
        filename_record = self._get_shared_text(SharedTextKind.FILENAME, filename)
        trace_frame = TraceFrame.Record(
            id=DBID(),
            kind=TraceKind.PRECONDITION,
            caller_id=caller_record.id,
            caller_port=caller_port,
            callee_id=callee_record.id,
            callee_port=callee_port,
            callee_location=SourceLocation(
                callee_location["line"],
                callee_location["start"],
                callee_location["end"],
            ),
            filename_id=filename_record.id,
            titos=titos,
            run_id=run.id,
            preserves_type_context=preserves_type_context,
            type_interval_lower=lb,
            type_interval_upper=ub,
            migrated_id=None,
        )

        for (sink, depth) in sinks:
            sink_record = self._get_shared_text(SharedTextKind.SINK, sink)
            self.graph.add_trace_frame_leaf_assoc(trace_frame, sink_record, depth)

        self.graph.add_trace_frame(trace_frame)
        self._generate_trace_annotations(trace_frame.id, features)
        return trace_frame

    def _generate_issue_feature_contents(self, issue, feature):
        # Generates a synthetic feature from the extra/feature
        features = set()
        for key in feature:
            value = feature[key]
            if isinstance(value, str) and value:
                features.add(key + ":" + value)
            else:
                features.add(key)
        return features

    def _get_interval(self, ti) -> Tuple[Optional[int], Optional[int], bool]:
        lower = ti.get("start", None)
        upper = ti.get("finish", None)
        preserves_type_context = ti.get("preserves_type_context", False)
        return (lower, upper, preserves_type_context)

    def _generate_trace_annotations(self, frame_id, features) -> None:
        for f in features:
            if "extra_trace" in f:
                annotation = f["extra_trace"]
                location = annotation["position"]
                self.graph.add_trace_annotation(
                    TraceFrameAnnotation.Record(
                        id=DBID(),
                        trace_frame_id=frame_id,
                        location=SourceLocation(
                            location["line"], location["start"], location["end"]
                        ),
                        message=annotation["msg"],
                        link=annotation.get("link", None),
                        trace_key=annotation.get("trace", None),
                    )
                )

    def _get_issue_handle(self, entry):
        return entry["handle"]

    def _get_shared_text(self, kind, name):
        name = name[:SHARED_TEXT_LENGTH]
        shared_text = self.graph.get_shared_text(kind, name)
        if shared_text is None:
            shared_text = SharedText.Record(id=DBID(), contents=name, kind=kind)
            self.graph.add_shared_text(shared_text)
        return shared_text

    @staticmethod
    def get_location(entry, is_relative=False):
        line = entry["line"]
        if is_relative:
            line -= entry["callable_line"]
        return SourceLocation(line, entry["start"], entry["end"])

    @staticmethod
    def get_callable_location(entry):
        line = entry["callable_line"]
        return SourceLocation(line, entry["start"], entry["end"])
