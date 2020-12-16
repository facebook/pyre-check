# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import logging
from collections import defaultdict
from typing import DefaultDict, Dict, Iterable, List, NamedTuple, Optional, Set, Tuple

from .bulk_saver import BulkSaver
from .models import (
    DBID,
    SHARED_TEXT_LENGTH,
    Issue,
    IssueInstance,
    IssueInstanceFixInfo,
    SharedText,
    SharedTextKind,
    TraceFrame,
    TraceFrameAnnotation,
    TraceKind,
)


log: logging.Logger = logging.getLogger("sapp")


class LeafMapping(NamedTuple):
    caller_leaf: int
    callee_leaf: int
    transform: int


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

        # Create a mapping of (caller_id, caller_port) to the corresponding
        # trace frame's id.
        self._trace_frames_map: DefaultDict[
            TraceKind, DefaultDict[Tuple[int, str], Set[int]]
        ] = defaultdict(lambda: defaultdict(set))

        # Similar to _trace_frames_map, but maps the reverse direction
        # of the trace graph, i.e. (callee_id, callee_port) to the
        # trace_frame_id.
        self._trace_frames_rev_map: DefaultDict[
            TraceKind, DefaultDict[Tuple[int, str], Set[int]]
        ] = defaultdict(lambda: defaultdict(set))

        self._trace_frames: Dict[int, TraceFrame] = {}

        self._shared_texts: Dict[int, SharedText] = {}
        self._shared_text_lookup: (
            DefaultDict[SharedTextKind, Dict[str, int]]
        ) = defaultdict(dict)

        self._trace_frame_leaf_assoc: DefaultDict[
            int, Set[Tuple[int, Optional[int]]]
        ] = defaultdict(set)

        self._trace_frame_issue_instance_assoc: DefaultDict[
            int, Set[int]
        ] = defaultdict(set)
        self._issue_instance_trace_frame_assoc: DefaultDict[
            int, Set[int]
        ] = defaultdict(set)

        self._trace_frame_annotation_trace_frame_assoc: DefaultDict[
            int, Set[int]
        ] = defaultdict(set)
        self._trace_frame_trace_frame_annotation_assoc: DefaultDict[
            int, Set[int]
        ] = defaultdict(set)

        self._issue_instance_shared_text_assoc: DefaultDict[
            int, Set[int]
        ] = defaultdict(set)
        self._shared_text_issue_instance_assoc: DefaultDict[
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

    def get_text(self, shared_text_id: DBID) -> str:
        return self._shared_texts[shared_text_id.local_id].contents

    def get_shared_text_by_local_id(self, shared_text_id: int) -> SharedText:
        return self._shared_texts[shared_text_id]

    def get_shared_text(
        self, kind: SharedTextKind, content: str
    ) -> Optional[SharedText]:
        if kind in self._shared_text_lookup:
            contents = self._shared_text_lookup[kind]
            if content in contents and contents[content] in self._shared_texts:
                return self._shared_texts[contents[content]]
        return None

    def has_trace_frames_with_caller(
        self, kind: TraceKind, caller_id: DBID, caller_port: str
    ) -> bool:
        if self._trace_frames_map[kind]:
            key = (caller_id.local_id, caller_port)
            return key in self._trace_frames_map[kind]
        else:
            return False

    def has_postconditions_with_caller(self, caller_id: DBID, caller_port: str) -> bool:
        return self.has_trace_frames_with_caller(
            TraceKind.postcondition, caller_id, caller_port
        )

    def has_preconditions_with_caller(self, caller_id: DBID, caller_port: str) -> bool:
        return self.has_trace_frames_with_caller(
            TraceKind.precondition, caller_id, caller_port
        )

    def add_trace_annotation(self, annotation: TraceFrameAnnotation) -> None:
        self._trace_annotations[annotation.id.local_id] = annotation

    def get_condition_annotations(self, cond_id: int) -> List[TraceFrameAnnotation]:
        return [
            t
            for t in self._trace_annotations.values()
            if t.trace_frame_id.local_id == cond_id
        ]

    def get_annotation_trace_frames(self, ann_id: int) -> List[TraceFrame]:
        if ann_id in self._trace_frame_annotation_trace_frame_assoc:
            return [
                self.get_trace_frame_from_id(tf_id)
                for tf_id in self._trace_frame_annotation_trace_frame_assoc[ann_id]
            ]
        else:
            return []

    def add_trace_frame(self, trace_frame: TraceFrame) -> None:
        key = (trace_frame.caller_id.local_id, trace_frame.caller_port)
        rev_key = (trace_frame.callee_id.local_id, trace_frame.callee_port)
        # pyre-fixme[6]: Expected `TraceKind` for 1st param but got `str`.
        self._trace_frames_map[trace_frame.kind][key].add(trace_frame.id.local_id)
        # pyre-fixme[6]: Expected `TraceKind` for 1st param but got `str`.
        self._trace_frames_rev_map[trace_frame.kind][rev_key].add(
            trace_frame.id.local_id
        )
        self._trace_frames[trace_frame.id.local_id] = trace_frame

    def get_trace_frames_from_caller(
        self, kind: TraceKind, caller_id: DBID, caller_port: str
    ) -> List[TraceFrame]:
        key = (caller_id.local_id, caller_port)
        return [
            self._trace_frames[trace_frame_id]
            for trace_frame_id in self._trace_frames_map[kind][key]
        ]

    def get_trace_frame_from_id(self, id: int) -> TraceFrame:
        return self._trace_frames[id]

    def add_shared_text(self, shared_text: SharedText) -> None:
        assert (
            shared_text.id.local_id not in self._shared_texts
        ), "Shared text already exists"
        assert (
            shared_text.kind not in self._shared_text_lookup
            or shared_text.contents not in self._shared_text_lookup[shared_text.kind]
        ), "Shared text with same kind, contents exists"

        self._shared_texts[shared_text.id.local_id] = shared_text

        # Allow look up of SharedTexts by name and kind (to optimize
        # get_shared_text which is called when parsing each issue instance)
        self._shared_text_lookup[shared_text.kind][
            shared_text.contents
        ] = shared_text.id.local_id

    def get_or_add_shared_text(self, kind: SharedTextKind, name: str) -> SharedText:
        name = name[:SHARED_TEXT_LENGTH]
        shared_text = self.get_shared_text(kind, name)
        if shared_text is None:
            shared_text = SharedText.Record(id=DBID(), contents=name, kind=kind)
            self.add_shared_text(shared_text)
        return shared_text

    def add_trace_frame_leaf_assoc(
        self, trace_frame: TraceFrame, leaf: SharedText, depth: Optional[int]
    ) -> None:
        self._trace_frame_leaf_assoc[trace_frame.id.local_id].add(
            (leaf.id.local_id, depth)
        )

    def add_trace_frame_leaf_by_local_id_assoc(
        self, trace_frame: TraceFrame, leaf_id: int, depth: Optional[int]
    ) -> None:
        self._trace_frame_leaf_assoc[trace_frame.id.local_id].add((leaf_id, depth))

    def get_trace_frame_leaf_ids(self, trace_frame: TraceFrame) -> Set[int]:
        ids: Set[int] = {
            id for (id, depth) in self._trace_frame_leaf_assoc[trace_frame.id.local_id]
        }
        return ids

    def get_trace_frame_leaf_ids_by_kind(
        self, trace_frame: TraceFrame, kind: SharedTextKind
    ) -> Set[int]:
        return {
            id
            for (id, depth) in self._trace_frame_leaf_assoc[trace_frame.id.local_id]
            if self._shared_texts[id].kind == kind
        }

    def get_trace_frame_leaf_ids_with_depths(
        self, trace_frame: TraceFrame
    ) -> Set[Tuple[int, Optional[int]]]:
        return self._trace_frame_leaf_assoc[trace_frame.id.local_id]

    def add_issue_instance_trace_frame_assoc(
        self, instance: IssueInstance, trace_frame: TraceFrame
    ) -> None:
        self._issue_instance_trace_frame_assoc[instance.id.local_id].add(
            trace_frame.id.local_id
        )
        self._trace_frame_issue_instance_assoc[trace_frame.id.local_id].add(
            instance.id.local_id
        )

    def add_trace_frame_annotation_trace_frame_assoc(
        self, annotation: TraceFrameAnnotation, trace_frame: TraceFrame
    ) -> None:
        self._trace_frame_annotation_trace_frame_assoc[annotation.id.local_id].add(
            trace_frame.id.local_id
        )
        self._trace_frame_trace_frame_annotation_assoc[trace_frame.id.local_id].add(
            annotation.id.local_id
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

    def get_next_trace_frames(self, trace_frame: TraceFrame) -> Iterable[TraceFrame]:
        return self.get_trace_frames_from_caller(
            # pyre-fixme[6]: Expected `TraceKind` for 1st param but got `str`.
            trace_frame.kind,
            trace_frame.callee_id,
            trace_frame.callee_port,
        )

    def add_issue_instance_shared_text_assoc_id(
        self, instance: IssueInstance, shared_text_id: int
    ) -> None:
        self._issue_instance_shared_text_assoc[instance.id.local_id].add(shared_text_id)
        self._shared_text_issue_instance_assoc[shared_text_id].add(instance.id.local_id)

    def add_issue_instance_shared_text_assoc(
        self, instance: IssueInstance, shared_text: SharedText
    ) -> None:
        self.add_issue_instance_shared_text_assoc_id(instance, shared_text.id.local_id)

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
        self._save_trace_frame_annotation_trace_frame_assoc(bulk_saver)

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

    def _save_trace_frame_annotation_trace_frame_assoc(
        self, bulk_saver: BulkSaver
    ) -> None:
        for (
            trace_annotation_id,
            trace_frame_ids,
        ) in self._trace_frame_annotation_trace_frame_assoc.items():
            for trace_frame_id in trace_frame_ids:
                bulk_saver.add_trace_frame_annotation_trace_frame_assoc(
                    self._trace_annotations[trace_annotation_id],
                    self._trace_frames[trace_frame_id],
                )

    def _save_trace_frame_leaf_assoc(self, bulk_saver: BulkSaver) -> None:
        """Adds trace frame leaf assocs to bulk saver after filtering them:
        1. if frame is a leaf, include all kinds
        2. otherwise, find outgoing leaf kinds and intersect with union of incoming
           leaf kinds of all successor frames.
        3. include only kinds that map to one of these outgoing kinds.
        """
        for trace_frame_id, leaf_ids in self._trace_frame_leaf_assoc.items():
            frame = self._trace_frames[trace_frame_id]
            valid_frame_leaf_ids = self._compute_valid_frame_leaves(frame)
            for (leaf_id, depth) in leaf_ids:
                leaf_text = self._shared_texts[leaf_id]
                if (
                    leaf_text.kind is SharedTextKind.FEATURE
                    or leaf_id in valid_frame_leaf_ids
                    or self._is_opposite_leaf(frame, leaf_text)
                ):
                    bulk_saver.add_trace_frame_leaf_assoc(leaf_text, frame, depth)
                else:
                    # Logging all the leaf kinds that are omitted causes large logs.
                    pass

    def _is_opposite_leaf(self, frame: TraceFrame, leaf: SharedText) -> bool:
        """We may be propagating sources along sink traces or vice versa. These should
        not be filtered and are identified here."""
        return (
            frame.kind == TraceKind.PRECONDITION and leaf.kind == SharedTextKind.SOURCE
        ) or (
            frame.kind == TraceKind.POSTCONDITION and leaf.kind == SharedTextKind.SINK
        )

    def _compute_valid_frame_leaves(self, frame: TraceFrame) -> Set[int]:
        # pyre-fixme[16]: extra fields are not known to pyre
        leaf_mapping: Set[LeafMapping] = frame.leaf_mapping
        is_leaf_frame = self.is_leaf_port(frame.callee_port)
        if not is_leaf_frame:
            callee_frames = self.get_next_trace_frames(frame)
            callee_leaf_ids = {
                callee_map.caller_leaf
                for callee_frame in callee_frames
                for callee_map in callee_frame.leaf_mapping
            }
        else:
            callee_leaf_ids = set()
        return {
            leaf_map.transform
            for leaf_map in leaf_mapping
            if is_leaf_frame or (leaf_map.callee_leaf in callee_leaf_ids)
        }

    def is_leaf_port(self, port: str) -> bool:
        return (
            port == "leaf"
            or port == "source"
            or port == "sink"
            or port.startswith("anchor:")
            or port.startswith("producer:")
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

    def compute_next_leaf_kinds(
        self, leaves: Set[int], leaf_mapping: Set[LeafMapping]
    ) -> Set[int]:
        """Normally, we would just intersect leaves and frame leaves, but since frame
        leaves can indicate local transforms of the form T1:...:Tn@G1..Gm:S, we need
        to be more careful.

        We first need to identify which frame leaves match by substituting the @
        for :, in general that would be T1:..Tn:G1..Gm:S. Then given these
        matches, erase everything up to and including the @ sign. That will be
        the new leaf kind.  In general, that is G1..Gm:S.

        leaf_mapping is already normalized to (caller_leaf_id, callee_leaf_id),
        i.e. which callers map to which caller ids obtained by performing the
        substitutions.

        For non-transform kinds, the leaf mapping contains identical
        caller_leaf_id, callee_leaf_id.
        """

        next_kinds = set()
        for leaf_map in leaf_mapping:
            if leaf_map.caller_leaf in leaves:
                next_kinds.add(leaf_map.callee_leaf)
        return next_kinds

    def compute_prev_leaf_kinds(
        self, leaves: Set[int], leaf_mapping: Set[LeafMapping]
    ) -> Set[int]:
        """Same as next_leaf_kinds but when following from leaves to issues."""
        next_kinds = set()
        for leaf_map in leaf_mapping:
            if leaf_map.callee_leaf in leaves:
                next_kinds.add(leaf_map.caller_leaf)
        return next_kinds

    def get_transform_normalized_kind_id(self, leaf_kind: SharedText) -> int:
        assert (
            leaf_kind.kind == SharedTextKind.SINK
            or leaf_kind.kind == SharedTextKind.SOURCE
        )
        if "@" in leaf_kind.contents:
            normal_name = leaf_kind.contents.replace("@", ":", 1)
            normal_kind = self.get_or_add_shared_text(leaf_kind.kind, normal_name)
            return normal_kind.id.local_id
        else:
            return leaf_kind.id.local_id

    def get_transformed_kind_id(self, leaf_kind: SharedText) -> int:
        assert (
            leaf_kind.kind == SharedTextKind.SINK
            or leaf_kind.kind == SharedTextKind.SOURCE
        )
        if "@" in leaf_kind.contents:
            splits = leaf_kind.contents.split("@", 1)
            remaining_kind = self.get_or_add_shared_text(leaf_kind.kind, splits[1])
            return remaining_kind.id.local_id
        else:
            return leaf_kind.id.local_id

    def get_incoming_leaf_kinds_of_frame(self, trace_frame: TraceFrame) -> Set[int]:
        # pyre-fixme[16]: extra fields are not known to pyre
        leaf_mapping: Set[LeafMapping] = trace_frame.leaf_mapping
        assert leaf_mapping is not None
        return {leaf_map.callee_leaf for leaf_map in leaf_mapping}

    def get_outgoing_leaf_kinds_of_frame(self, trace_frame: TraceFrame) -> Set[int]:
        # pyre-fixme[16]: extra fields are not known to pyre
        leaf_mapping: Set[LeafMapping] = trace_frame.leaf_mapping
        assert leaf_mapping is not None
        return {leaf_map.caller_leaf for leaf_map in leaf_mapping}
