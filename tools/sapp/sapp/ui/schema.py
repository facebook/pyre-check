# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
from pathlib import Path
from typing import Any, Dict, List, NamedTuple, Optional, Tuple

import graphene
from graphene import relay
from graphene_sqlalchemy import get_session
from graphql.execution.base import ResolveInfo
from sqlalchemy.orm import Session
from sqlalchemy.sql import func

from ..models import (
    DBID,
    Run,
    RunStatus,
    SharedText,
    SharedTextKind,
    TraceFrame,
    TraceKind,
)
from . import filters as filters_module, issues, trace, typeahead
from .issues import IssueQueryResult, IssueQueryResultType
from .trace import LeafLookup, TraceFrameQueryResult, TraceFrameQueryResultType


class IssueConnection(relay.Connection):
    class Meta:
        node = IssueQueryResultType


class TraceFrameConnection(relay.Connection):
    class Meta:
        node = TraceFrameQueryResultType


class FileType(graphene.ObjectType):
    class Meta:
        interfaces = (relay.Node,)

    path = graphene.String()
    contents = graphene.String()


class File(NamedTuple):
    path: str
    contents: str


class FileConnection(relay.Connection):
    class Meta:
        node = FileType


class CodeConnection(relay.Connection):
    class Meta:
        node = typeahead.CodeType


class PathConnection(relay.Connection):
    class Meta:
        node = typeahead.PathType


class CallableConnection(relay.Connection):
    class Meta:
        node = typeahead.CallableType


class FeatureConnection(relay.Connection):
    class Meta:
        node = typeahead.Feature


class FilterConnection(relay.Connection):
    class Meta:
        node = filters_module.Filter


class FeatureCondition(graphene.InputObjectType):
    mode = graphene.String()
    features = graphene.List(graphene.String)


class Query(graphene.ObjectType):
    # pyre-fixme[4]: Attribute must be annotated.
    node = relay.Node.Field()
    issues = relay.ConnectionField(
        IssueConnection,
        codes=graphene.List(graphene.Int, default_value=["%"]),
        paths=graphene.List(graphene.String, default_value=["%"]),
        callables=graphene.List(graphene.String, default_value=["%"]),
        features=graphene.List(FeatureCondition),
        min_trace_length_to_sinks=graphene.Int(),
        max_trace_length_to_sinks=graphene.Int(),
        min_trace_length_to_sources=graphene.Int(),
        max_trace_length_to_sources=graphene.Int(),
        issue_id=graphene.Int(),
    )

    trace = relay.ConnectionField(TraceFrameConnection, issue_id=graphene.ID())
    initial_trace_frames = relay.ConnectionField(
        TraceFrameConnection, issue_id=graphene.Int(), kind=graphene.String()
    )
    next_trace_frames = relay.ConnectionField(
        TraceFrameConnection,
        issue_id=graphene.Int(),
        frame_id=graphene.Int(),
        kind=graphene.String(),
    )

    # Typeahead data.
    codes = relay.ConnectionField(CodeConnection)
    paths = relay.ConnectionField(PathConnection)
    callables = relay.ConnectionField(CallableConnection)
    features = relay.ConnectionField(FeatureConnection)

    file = relay.ConnectionField(FileConnection, path=graphene.String())

    filters = relay.ConnectionField(FilterConnection)

    def resolve_issues(
        self,
        info: ResolveInfo,
        codes: List[int],
        paths: List[str],
        callables: List[str],
        features: Optional[List[FeatureCondition]] = None,
        min_trace_length_to_sinks: Optional[int] = None,
        max_trace_length_to_sinks: Optional[int] = None,
        min_trace_length_to_sources: Optional[int] = None,
        max_trace_length_to_sources: Optional[int] = None,
        issue_id: Optional[int] = None,
        **kwargs: Any,
    ) -> List[IssueQueryResult]:
        session = get_session(info.context)
        run_id = Query.latest_run_id(session)

        builder = (
            issues.Query(session, run_id)
            .where_codes_is_any_of(codes)
            .where_callables_is_any_of(callables)
            .where_path_is_any_of(paths)
            .where_trace_length_to_sinks(
                min_trace_length_to_sinks, max_trace_length_to_sinks
            )
            .where_trace_length_to_sources(
                min_trace_length_to_sources, max_trace_length_to_sources
            )
            .where_issue_id_is(issue_id)
        )

        for feature in features or []:
            if feature.mode == "any of":
                # pyre-ignore[6]: graphene too dynamic.
                builder = builder.where_any_features(feature.features)
            if feature.mode == "all of":
                # pyre-ignore[6]: graphene too dynamic.
                builder = builder.where_all_features(feature.features)
            if feature.mode == "none of":
                # pyre-ignore[6]: graphene too dynamic.
                builder = builder.where_exclude_features(feature.features)

        return builder.get()

    def resolve_initial_trace_frames(
        self, info: ResolveInfo, issue_id: int, kind: str
    ) -> List[TraceFrameQueryResult]:
        session = info.context.get("session")
        return trace.Query(session).initial_trace_frames(
            issue_id, TraceKind.create_from_string(kind)
        )

    def resolve_next_trace_frames(
        self, info: ResolveInfo, issue_id: int, frame_id: int, kind: str
    ) -> List[TraceFrameQueryResult]:
        session = info.context.get("session")

        run_id = DBID(Query.latest_run_id(session))

        trace_kind = TraceKind.create_from_string(kind)
        if trace_kind == TraceKind.POSTCONDITION:
            leaf_kind = issues.Query(session, run_id).get_leaves_issue_instance(
                session, issue_id, SharedTextKind.SOURCE
            )
        elif trace_kind == TraceKind.PRECONDITION:
            leaf_kind = issues.Query(session, run_id).get_leaves_issue_instance(
                session, issue_id, SharedTextKind.SINK
            )

        trace_frame = session.query(TraceFrame).get(frame_id)
        if trace_frame is None:
            raise ValueError(f"`{frame_id}` is not a valid trace frame id")

        return trace.Query(session).next_trace_frames(
            trace.LeafLookup.create(session),
            run_id,
            leaf_kind,
            trace_frame,
            visited_ids=set(),
        )

    def resolve_codes(self, info: ResolveInfo) -> List[typeahead.Code]:
        session = info.context["session"]
        return typeahead.all_codes(session)

    def resolve_paths(self, info: ResolveInfo) -> List[typeahead.Path]:
        session = info.context["session"]
        return typeahead.all_paths(session)

    def resolve_callables(self, info: ResolveInfo) -> List[typeahead.Callable]:
        session = info.context["session"]
        return typeahead.all_callables(session)

    def resolve_features(self, info: ResolveInfo) -> List[typeahead.Feature]:
        session = info.context["session"]
        return typeahead.all_features(session)

    def resolve_file(self, info: ResolveInfo, path: str, **kwargs: Any) -> List[File]:
        if ".." in path:
            raise FileNotFoundError("Attempted directory traversal")

        source_directory = Path(info.context.get("source_directory") or os.getcwd())
        contents = (source_directory / path).read_text()
        return [File(path=path, contents=contents)]

    def resolve_filters(self, info: ResolveInfo) -> List[filters_module.Filter]:
        session = info.context["session"]
        return filters_module.all_filters(session)

    @staticmethod
    def latest_run_id(session: Session) -> DBID:
        return DBID(
            (
                session.query(func.max(Run.id))
                .filter(Run.status == RunStatus.FINISHED)
                .scalar()
            )
        )


class SaveFilterMutation(relay.ClientIDMutation):
    node = graphene.Field(filters_module.Filter)

    class Input:
        name = graphene.String(required=True)
        description = graphene.String()
        json = graphene.String()

    def mutate_and_get_payload(
        self, info: ResolveInfo, **kwargs: Any
    ) -> "SaveFilterMutation":
        session = info.context.get("session")
        filter = filters_module.Filter(**kwargs)
        filters_module.save_filter(session, filter)
        return SaveFilterMutation(node=filter)


class DeleteFilterMutation(relay.ClientIDMutation):
    class Input:
        name = graphene.String(required=True)

    def mutate_and_get_payload(
        self, info: ResolveInfo, **kwargs: Any
    ) -> "DeleteFilterMutation":
        session = info.context.get("session")
        filters_module.delete_filter(session, kwargs["name"])
        return DeleteFilterMutation()


class Mutation(graphene.ObjectType):
    # pyre-fixme[4]: Attribute must be annotated.
    save_filter = SaveFilterMutation.Field()
    # pyre-fixme[4]: Attribute must be annotated.
    delete_filter = DeleteFilterMutation.Field()


schema = graphene.Schema(query=Query, mutation=Mutation, auto_camelcase=False)
