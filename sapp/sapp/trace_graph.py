#!/usr/bin/env python3
# pyre-strict

from collections import defaultdict
from typing import DefaultDict, Dict, Iterable, List, Optional, Set, Tuple

from .bulk_saver import BulkSaver
from .models import (
    DBID,
    Issue,
    IssueInstance,
    IssueInstanceFixInfo,
    SharedText,
    SharedTextKind,
    TraceFrame,
    TraceFrameAnnotation,
    TraceKind,
)


class TraceGraph(object):
    """Represents a graph of the Zoncolan trace steps. Nodes of the graph are
    the issues, preconditions, postconditions, sources and sinks. Edges are
    the the assocs, and for pre/postconditions, the map of 'caller->callee'
    gives a one direction edge to the next pre/postcondition, and the map of
    'callee->caller' gives the reverse edge.
    """

    def __init__(self) -> None:
        self._issues: Dict[int, Issue] = {}
        self._issue_instances: Dict[int, IssueInstance] = {}
        self._trace_annotations: Dict[int, TraceFrameAnnotation] = {}

        # Create a mapping of (caller, caller_port) to the corresponding
        # trace frame's id.
        self._trace_frames_map: DefaultDict[  # pyre-ignore: T41307149
            Tuple[str, str], Set[int]
        ] = defaultdict(set)

        # Similar to _trace_frames_map, but maps the reverse direction
        # of the trace graph, i.e. (callee[, callee_port]) to the
        # trace_frame_id.
        self._trace_frames_rev_map: DefaultDict[  # pyre-ignore: T41307149
            Tuple[str, str], Set[int]
        ] = defaultdict(set)

        self._trace_frames: Dict[int, TraceFrame] = {}

        self._shared_texts: Dict[int, SharedText] = {}
        # pyre-fixme[8]: Attribute has type `DefaultDict[SharedTextKind, Dict[str, in...
        self._shared_text_lookup: (
            DefaultDict[SharedTextKind, Dict[str, int]]
        ) = defaultdict(dict)

        self._trace_frame_leaf_assoc: DefaultDict[  # pyre-ignore: T41307149
            int, Set[Tuple[int, int]]
        ] = defaultdict(set)

        self._trace_frame_issue_instance_assoc: DefaultDict[  # pyre-ignore: T41307149
            int, Set[int]
        ] = defaultdict(set)
        self._issue_instance_trace_frame_assoc: DefaultDict[  # pyre-ignore: T41307149
            int, Set[int]
        ] = defaultdict(set)

        self._issue_instance_shared_text_assoc: DefaultDict[  # pyre-ignore: T41307149
            int, Set[int]
        ] = defaultdict(set)
        self._shared_text_issue_instance_assoc: DefaultDict[  # pyre-ignore: T41307149
            int, Set[int]
        ] = defaultdict(set)

        self._issue_instance_fix_info: Dict[int, IssueInstanceFixInfo] = {}

        # !!!!! IMPORTANT !!!!!
        # IF YOU ARE ADDING MORE FIELDS/EDGES TO THIS GRAPH, CHECK IF
        # TrimmedTraceGraph NEEDS TO BE UPDATED AS WELL.
        #
        # TrimmedTraceGraph will populate itself from this object. It searches
        # TrimmedGraph for nodes and edges of 'affected_files' and copies them
        # over. If new fields/edges are added, these may need to be copied in
        # TrimmedTraceGraph as well.

    def add_issue(self, issue: Issue) -> None:
        assert issue.id.local_id not in self._issues, "Issue already exists"
        self._issues[issue.id.local_id] = issue

    def get_issue(self, issue_id: DBID) -> Issue:
        return self._issues[issue_id.local_id]

    def add_issue_instance(self, instance: IssueInstance) -> None:
        assert (
            instance.id.local_id not in self._issue_instances
        ), "Instance already exists"
        self._issue_instances[instance.id.local_id] = instance

    def get_issue_instances(self) -> Iterable[IssueInstance]:
        return (instance for instance in self._issue_instances.values())

    def add_issue_instance_fix_info(
        self, instance: IssueInstance, fix_info: IssueInstanceFixInfo
    ) -> None:
        assert (
            instance.id.local_id not in self._issue_instance_fix_info
        ), "Instance fix info already exists"
        self._issue_instance_fix_info[instance.id.local_id] = fix_info

    def get_shared_text(
        self, kind: SharedTextKind, content: str
    ) -> Optional[SharedText]:
        if kind in self._shared_text_lookup:
            contents = self._shared_text_lookup[kind]
            if content in contents and contents[content] in self._shared_texts:
                return self._shared_texts[contents[content]]
        return None

    def has_postconditions_with_caller(self, caller: str, caller_port: str) -> bool:
        key = (caller, caller_port)
        post_ids = {
            tf_id
            for tf_id in self._trace_frames_map[key]
            if self._trace_frames[tf_id].kind == TraceKind.POSTCONDITION
        }
        return len(post_ids) != 0

    def has_preconditions_with_caller(self, caller: str, caller_port: str) -> bool:
        key = (caller, caller_port)
        pre_ids = {
            tf_id
            for tf_id in self._trace_frames_map[key]
            if self._trace_frames[tf_id].kind == TraceKind.PRECONDITION
        }
        return len(pre_ids) != 0

    def add_trace_annotation(self, annotation: TraceFrameAnnotation) -> None:
        self._trace_annotations[annotation.id.local_id] = annotation

    def get_precondition_annotations(self, pre_id: int) -> List[TraceFrameAnnotation]:
        return [
            t
            for t in self._trace_annotations.values()
            if t.trace_frame_id.local_id == pre_id
        ]

    def add_trace_frame(self, trace_frame: TraceFrame) -> None:
        key = (trace_frame.caller, trace_frame.caller_port)
        rev_key = (trace_frame.callee, trace_frame.callee_port)
        self._trace_frames_map[key].add(trace_frame.id.local_id)
        self._trace_frames_rev_map[rev_key].add(trace_frame.id.local_id)
        self._trace_frames[trace_frame.id.local_id] = trace_frame

    def has_trace_frame_with_caller(self, caller: str, caller_port: str) -> bool:
        key = (caller, caller_port)
        return key in self._trace_frames_map

    def get_trace_frames_from_caller(
        self, caller: str, caller_port: str
    ) -> List[TraceFrame]:
        if self.has_trace_frame_with_caller(caller, caller_port):
            key = (caller, caller_port)
            return [
                self._trace_frames[trace_frame_id]
                for trace_frame_id in self._trace_frames_map[key]
            ]
        else:
            return []

    def get_trace_frame_from_id(self, id: int) -> TraceFrame:
        return self._trace_frames[id]

    def add_shared_text(self, shared_text: SharedText) -> None:
        assert (
            shared_text.id.local_id not in self._shared_texts
        ), "Shared text already exists"

        self._shared_texts[shared_text.id.local_id] = shared_text

        # Allow look up of SharedTexts by name and kind (to optimize
        # get_shared_text which is called when parsing each issue instance)
        self._shared_text_lookup[shared_text.kind][
            shared_text.contents
        ] = shared_text.id.local_id

    def add_trace_frame_leaf_assoc(
        self, trace_frame: TraceFrame, leaf: SharedText, depth: int
    ) -> None:
        self._trace_frame_leaf_assoc[trace_frame.id.local_id].add(
            (leaf.id.local_id, depth)
        )

    def get_trace_frame_leaf_ids(self, trace_frame: TraceFrame) -> Set[int]:
        ids: Set[int] = {
            id for (id, depth) in self._trace_frame_leaf_assoc[trace_frame.id.local_id]
        }
        return ids

    def add_issue_instance_trace_frame_assoc(
        self, instance: IssueInstance, trace_frame: TraceFrame
    ) -> None:
        self._issue_instance_trace_frame_assoc[instance.id.local_id].add(
            trace_frame.id.local_id
        )
        self._trace_frame_issue_instance_assoc[trace_frame.id.local_id].add(
            instance.id.local_id
        )

    def get_issue_instance_trace_frames(
        self, instance: IssueInstance
    ) -> List[TraceFrame]:
        if instance.id.local_id in self._issue_instance_trace_frame_assoc:
            return [
                self.get_trace_frame_from_id(id)
                for id in self._issue_instance_trace_frame_assoc[instance.id.local_id]
            ]
        else:
            return []

    def add_issue_instance_shared_text_assoc(
        self, instance: IssueInstance, shared_text: SharedText
    ) -> None:
        self._issue_instance_shared_text_assoc[instance.id.local_id].add(
            shared_text.id.local_id
        )
        self._shared_text_issue_instance_assoc[shared_text.id.local_id].add(
            instance.id.local_id
        )

    def get_issue_instance_shared_texts(
        self, instance_id: int, kind: SharedTextKind
    ) -> List[SharedText]:
        return [
            self._shared_texts[msg_id]
            for msg_id in self._issue_instance_shared_text_assoc[instance_id]
            if self._shared_texts[msg_id].kind == kind
        ]

    def update_bulk_saver(self, bulk_saver: BulkSaver) -> None:
        bulk_saver.add_all(list(self._issues.values()))
        bulk_saver.add_all(list(self._issue_instances.values()))
        bulk_saver.add_all(list(self._trace_frames.values()))
        bulk_saver.add_all(list(self._issue_instance_fix_info.values()))
        bulk_saver.add_all(list(self._trace_annotations.values()))
        bulk_saver.add_all(list(self._shared_texts.values()))

        self._save_issue_instance_trace_frame_assoc(bulk_saver)
        self._save_trace_frame_leaf_assoc(bulk_saver)
        self._save_issue_instance_shared_text_assoc(bulk_saver)

    def _save_issue_instance_trace_frame_assoc(self, bulk_saver: BulkSaver) -> None:
        for (
            trace_frame_id,
            instance_ids,
        ) in self._trace_frame_issue_instance_assoc.items():
            for instance_id in instance_ids:
                bulk_saver.add_issue_instance_trace_frame_assoc(
                    self._issue_instances[instance_id],
                    self._trace_frames[trace_frame_id],
                )

    def _save_trace_frame_leaf_assoc(self, bulk_saver: BulkSaver) -> None:
        for trace_frame_id, leaf_ids in self._trace_frame_leaf_assoc.items():
            for (leaf_id, depth) in leaf_ids:
                bulk_saver.add_trace_frame_leaf_assoc(
                    self._shared_texts[leaf_id],
                    self._trace_frames[trace_frame_id],
                    depth,
                )

    def _save_issue_instance_shared_text_assoc(self, bulk_saver: BulkSaver) -> None:
        for (
            shared_text_id,
            instance_ids,
        ) in self._shared_text_issue_instance_assoc.items():
            for instance_id in instance_ids:
                bulk_saver.add_issue_instance_shared_text_assoc(
                    self._issue_instances[instance_id],
                    self._shared_texts[shared_text_id],
                )
