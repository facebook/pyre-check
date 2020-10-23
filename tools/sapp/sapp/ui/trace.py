# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any, Dict, List, NamedTuple, Optional, Set, Tuple, Union

import graphene
from graphql.execution.base import ResolveInfo
from sqlalchemy.orm import Session, aliased

from ..models import (
    DBID,
    IssueInstanceTraceFrameAssoc,
    SharedText,
    SharedTextKind,
    SourceLocation,
    TraceFrame,
    TraceFrameLeafAssoc,
    TraceKind,
)


# pyre-fixme[5]: Global expression must be annotated.
FilenameText = aliased(SharedText)
# pyre-fixme[5]: Global expression must be annotated.
CallableText = aliased(SharedText)
# pyre-fixme[5]: Global expression must be annotated.
CallerText = aliased(SharedText)
# pyre-fixme[5]: Global expression must be annotated.
CalleeText = aliased(SharedText)
# pyre-fixme[5]: Global expression must be annotated.
MessageText = aliased(SharedText)


class TraceFrameQueryResultType(graphene.ObjectType):
    class Meta:
        interfaces = (graphene.relay.Node,)

    frame_id = graphene.ID()
    caller = graphene.String()
    caller_port = graphene.String()
    callee = graphene.String()
    callee_port = graphene.String()
    caller_id = graphene.ID()
    callee_id = graphene.ID()
    callee_location = graphene.ID()
    kind = graphene.String()
    filename = graphene.String()
    trace_length = graphene.Int()

    def resolve_frame_id(self, info: ResolveInfo) -> DBID:
        # pyre-fixme[16]: `TraceFrameQueryResultType` has no attribute `id`.
        return self.id


LEAF_NAMES: Set[str] = {"source", "sink", "leaf"}


class TraceFrameQueryResult(NamedTuple):
    id: DBID
    caller: str
    caller_port: str
    callee: str
    callee_port: str
    caller_id: Optional[DBID] = None
    callee_id: Optional[DBID] = None
    callee_location: Optional[SourceLocation] = None
    kind: Optional[TraceKind] = None
    filename: Optional[str] = None
    trace_length: Optional[int] = None

    @staticmethod
    # pyre-fixme[2]: Parameter annotation cannot be `Any`.
    def from_record(record: Any) -> "TraceFrameQueryResult":
        return TraceFrameQueryResult(
            id=record.id,
            caller=record.caller,
            caller_port=record.caller_port,
            callee=record.callee,
            callee_port=record.callee_port,
            caller_id=record.caller_id,
            callee_id=record.callee_id,
            callee_location=record.callee_location,
            kind=record.kind,
            filename=record.filename,
            trace_length=getattr(record, "trace_length", None),
        )

    def is_leaf(self) -> bool:
        return self.callee_port in LEAF_NAMES


class TraceTuple(NamedTuple):
    trace_frame: TraceFrameQueryResult
    branches: int = 1
    missing: bool = False
    # Suppose we select a trace frame (A->B) and the generated trace is
    #   (A->B), (B->C), (C->D) with D as leaf.
    # When we display traces, we only use the callee, so this trace would look
    #   like B->C->D. If we also want to see A->, then we need to add a
    #   placeholder trace tuple. We do this by setting our trace tuples to
    #   [(A->B, placeholder=True), (A->B), (B->C), (C->D)]. When placeholder is
    #   True, that means we need to output the caller rather than the callee.
    placeholder: bool = False


class Query:
    def __init__(self, session: Session) -> None:
        self._session: Session = session

    def initial_trace_frames(
        self, issue_id: int, kind: TraceKind
    ) -> List[TraceFrameQueryResult]:
        return [
            TraceFrameQueryResult.from_record(result)
            for result in self._session.query(
                TraceFrame.id,
                TraceFrame.caller_id,
                CallerText.contents.label("caller"),
                TraceFrame.caller_port,
                TraceFrame.callee_id,
                CalleeText.contents.label("callee"),
                TraceFrame.callee_port,
                TraceFrame.callee_location,
                TraceFrame.kind,
                FilenameText.contents.label("filename"),
                TraceFrameLeafAssoc.trace_length,
            )
            .filter(TraceFrame.kind == kind)
            .join(
                IssueInstanceTraceFrameAssoc,
                IssueInstanceTraceFrameAssoc.trace_frame_id == TraceFrame.id,
            )
            .filter(IssueInstanceTraceFrameAssoc.issue_instance_id == issue_id)
            .join(CallerText, CallerText.id == TraceFrame.caller_id)
            .join(CalleeText, CalleeText.id == TraceFrame.callee_id)
            .join(FilenameText, FilenameText.id == TraceFrame.filename_id)
            .join(
                TraceFrameLeafAssoc, TraceFrameLeafAssoc.trace_frame_id == TraceFrame.id
            )
            .group_by(TraceFrame.id)
            .order_by(TraceFrameLeafAssoc.trace_length, TraceFrame.callee_location)
            .all()
        ]

    def navigate_trace_frames(
        self,
        leaf_dicts: Tuple[Dict[int, str], Dict[int, str], Dict[int, str]],
        current_run_id: DBID,
        sources: Set[str],
        sinks: Set[str],
        initial_trace_frames: List[TraceFrameQueryResult],
        index: int = 0,
    ) -> List[Tuple[TraceFrameQueryResult, int]]:
        if not initial_trace_frames:
            return []
        trace_frames = [(initial_trace_frames[index], len(initial_trace_frames))]
        visited_ids: Set[int] = {int(initial_trace_frames[index].id)}
        while not trace_frames[-1][0].is_leaf():
            trace_frame, branches = trace_frames[-1]
            if trace_frame.kind == TraceKind.POSTCONDITION:
                leaf_kind = sources
            elif trace_frame.kind == TraceKind.PRECONDITION:
                leaf_kind = sinks
            else:
                assert (
                    trace_frame.kind == TraceKind.POSTCONDITION
                    or trace_frame.kind == TraceKind.PRECONDITION
                )
            next_nodes = self.next_trace_frames(
                leaf_dicts, current_run_id, leaf_kind, trace_frame, visited_ids
            )

            if len(next_nodes) == 0:
                # Denote a missing frame by setting caller to None
                trace_frames.append(
                    (
                        TraceFrameQueryResult(
                            id=DBID(0),
                            callee=trace_frame.callee,
                            callee_port=trace_frame.callee_port,
                            caller="",
                            caller_port="",
                        ),
                        0,
                    )
                )
                return trace_frames

            visited_ids.add(int(next_nodes[0].id))
            trace_frames.append((next_nodes[0], len(next_nodes)))
        return trace_frames

    def next_trace_frames(
        self,
        leaf_dicts: Tuple[Dict[int, str], Dict[int, str], Dict[int, str]],
        current_run_id: DBID,
        leaf_kind: Set[str],
        trace_frame: TraceFrameQueryResult,
        visited_ids: Set[int],
        backwards: bool = False,
    ) -> List[TraceFrameQueryResult]:
        """Finds all trace frames that the given trace_frame flows to.

        When backwards=True, the result will include the parameter trace_frame,
        since we are filtering on the parameter's callee.
        """
        query = (
            self._session.query(
                TraceFrame.id,
                TraceFrame.caller_id,
                CallerText.contents.label("caller"),
                TraceFrame.caller_port,
                TraceFrame.callee_id,
                CalleeText.contents.label("callee"),
                TraceFrame.callee_port,
                TraceFrame.callee_location,
                TraceFrame.kind,
                FilenameText.contents.label("filename"),
                TraceFrameLeafAssoc.trace_length,
            )
            .filter(TraceFrame.run_id == current_run_id)
            .filter(TraceFrame.kind == trace_frame.kind)
            .join(CallerText, CallerText.id == TraceFrame.caller_id)
            .join(CalleeText, CalleeText.id == TraceFrame.callee_id)
            .join(FilenameText, FilenameText.id == TraceFrame.filename_id)
            .filter(
                TraceFrame.caller_id != TraceFrame.callee_id
            )  # skip recursive calls for now
        )
        if backwards:
            query = query.filter(TraceFrame.callee_id == trace_frame.caller_id).filter(
                TraceFrame.callee_port == trace_frame.caller_port
            )
        else:
            query = query.filter(TraceFrame.caller_id == trace_frame.callee_id).filter(
                TraceFrame.caller_port == trace_frame.callee_port
            )

        results = (
            query.join(
                TraceFrameLeafAssoc, TraceFrameLeafAssoc.trace_frame_id == TraceFrame.id
            )
            .group_by(TraceFrame.id)
            .order_by(TraceFrameLeafAssoc.trace_length, TraceFrame.callee_location)
        )

        filtered_results = []
        for frame in results:
            if int(frame.id) not in visited_ids and leaf_kind.intersection(
                set(
                    self.get_leaves_trace_frame(
                        leaf_dicts,
                        int(frame.id),
                        trace_kind_to_shared_text_kind(frame.kind),
                    )
                )
            ):
                filtered_results.append(frame)

        return [TraceFrameQueryResult.from_record(frame) for frame in filtered_results]

    def get_leaves_trace_frame(
        self,
        leaf_dicts: Tuple[Dict[int, str], Dict[int, str], Dict[int, str]],
        trace_frame_id: Union[int, DBID],
        kind: SharedTextKind,
    ) -> Set[str]:
        message_ids = [
            int(id)
            for id, in self._session.query(SharedText.id)
            .distinct(SharedText.id)
            .join(TraceFrameLeafAssoc, SharedText.id == TraceFrameLeafAssoc.leaf_id)
            .filter(TraceFrameLeafAssoc.trace_frame_id == trace_frame_id)
            .filter(SharedText.kind == kind)
        ]
        leaf_sources, leaf_sinks, features_dict = leaf_dicts
        return leaf_dict_lookups(
            leaf_sources, leaf_sinks, features_dict, message_ids, kind
        )


def trace_kind_to_shared_text_kind(trace_kind: Optional[TraceKind]) -> SharedTextKind:
    if trace_kind == TraceKind.POSTCONDITION:
        return SharedTextKind.SOURCE
    if trace_kind == TraceKind.PRECONDITION:
        return SharedTextKind.SINK

    raise AssertionError(f"{trace_kind} is invalid")


def leaf_dict_lookups(
    sources_dict: Dict[int, str],
    sinks_dict: Dict[int, str],
    features_dict: Dict[int, str],
    message_ids: List[int],
    kind: SharedTextKind,
) -> Set[str]:
    if kind == SharedTextKind.SOURCE:
        leaf_dict = sources_dict
    elif kind == SharedTextKind.SINK:
        leaf_dict = sinks_dict
    else:
        leaf_dict = features_dict
    return {leaf_dict[id] for id in message_ids if id in leaf_dict}
