# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Dict, Iterable, List, NamedTuple, Optional, Set, Tuple, Union

import graphene
from sqlalchemy.orm import Session, aliased

from .models import (
    DBID,
    IssueInstanceTraceFrameAssoc,
    SharedText,
    SharedTextKind,
    SourceLocation,
    TraceFrame,
    TraceFrameLeafAssoc,
    TraceKind,
)


FilenameText = aliased(SharedText)
CallableText = aliased(SharedText)
CallerText = aliased(SharedText)
CalleeText = aliased(SharedText)
MessageText = aliased(SharedText)


class TraceFrameQueryResultType(graphene.ObjectType):
    class Meta:
        interfaces = (graphene.relay.Node,)

    trace_id = graphene.ID()
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
    file_content = graphene.String()

    def resolve_trace_id(self, info):
        return self.id


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
    file_content: Optional[str] = None


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


class TraceOperator:
    LEAF_NAMES = {"source", "sink", "leaf"}

    @staticmethod
    def initial_trace_frames(
        session: Session, issue_id: int, kind
    ) -> List[TraceFrameQueryResult]:
        return [
            TraceFrameQueryResult(
                id=result.id,
                caller=result.caller,
                caller_port=result.caller_port,
                callee=result.callee,
                callee_port=result.callee_port,
                caller_id=result.caller_id,
                callee_id=result.callee_id,
                callee_location=result.callee_location,
                kind=result.kind,
                filename=result.filename,
                trace_length=result.trace_length,
            )
            for result in session.query(
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

    @staticmethod
    def navigate_trace_frames(
        leaf_dicts: Tuple[Dict[int, str], Dict[int, str], Dict[int, str]],
        session: Session,
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
        while not TraceOperator.is_leaf(trace_frames[-1][0]):
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
            next_nodes = TraceOperator.next_trace_frames(
                leaf_dicts, session, current_run_id, leaf_kind, trace_frame, visited_ids
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

    @staticmethod
    def is_leaf(trace_frame: TraceFrameQueryResult) -> bool:
        return trace_frame.callee_port in TraceOperator.LEAF_NAMES

    @staticmethod
    def next_trace_frames(
        leaf_dicts: Tuple[Dict[int, str], Dict[int, str], Dict[int, str]],
        session: Session,
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
            session.query(
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
                    TraceOperator.get_leaves_trace_frame(
                        leaf_dicts,
                        session,
                        int(frame.id),
                        TraceOperator.trace_kind_to_shared_text_kind(frame.kind),
                    )
                )
            ):
                filtered_results.append(frame)

        return [
            TraceFrameQueryResult(
                id=frame.id,
                caller=frame.caller,
                caller_port=frame.caller_port,
                callee=frame.callee,
                callee_port=frame.callee_port,
                caller_id=frame.caller_id,
                callee_location=frame.callee_location,
                kind=frame.kind,
                filename=frame.filename,
                trace_length=frame.trace_length,
            )
            for frame in filtered_results
        ]

    @staticmethod
    def get_leaves_trace_frame(
        leaf_dicts: Tuple[Dict[int, str], Dict[int, str], Dict[int, str]],
        session: Session,
        trace_frame_id: Union[int, DBID],
        kind: SharedTextKind,
    ) -> Set[str]:
        message_ids = [
            int(id)
            for id, in session.query(SharedText.id)
            .distinct(SharedText.id)
            .join(TraceFrameLeafAssoc, SharedText.id == TraceFrameLeafAssoc.leaf_id)
            .filter(TraceFrameLeafAssoc.trace_frame_id == trace_frame_id)
            .filter(SharedText.kind == kind)
        ]
        leaf_sources, leaf_sinks, features_dict = leaf_dicts
        return TraceOperator.leaf_dict_lookups(
            leaf_sources, leaf_sinks, features_dict, message_ids, kind
        )

    @staticmethod
    def trace_kind_to_shared_text_kind(
        trace_kind: Optional[TraceKind],
    ) -> SharedTextKind:
        if trace_kind == TraceKind.POSTCONDITION:
            return SharedTextKind.SOURCE
        if trace_kind == TraceKind.PRECONDITION:
            return SharedTextKind.SINK

        raise AssertionError(f"{trace_kind} is invalid")

    @staticmethod
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

    @staticmethod
    def create_trace_tuples(
        navigation: Iterable[Tuple[TraceFrameQueryResult, int]]
    ) -> List[TraceTuple]:
        return [
            TraceTuple(
                trace_frame=trace_frame,
                branches=branches,
                missing=trace_frame.caller == "",
            )
            for trace_frame, branches in navigation
        ]
