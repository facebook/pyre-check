# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
from typing import Dict, List, Optional, Tuple

import graphene
from graphene import relay
from graphene_sqlalchemy import get_session
from graphql.execution.base import ResolveInfo
from sqlalchemy.orm import Session, aliased
from sqlalchemy.sql import func

from .interactive import IssueQueryResult, IssueQueryResultType
from .models import (
    DBID,
    Issue,
    IssueInstance,
    Run,
    RunStatus,
    SharedText,
    SharedTextKind,
    TraceKind,
)
from .query_builder import IssueQueryBuilder
from .trace_operator import (
    TraceFrameQueryResult,
    TraceFrameQueryResultType,
    TraceOperator,
)


FilenameText = aliased(SharedText)
CallableText = aliased(SharedText)
CallerText = aliased(SharedText)
CalleeText = aliased(SharedText)
MessageText = aliased(SharedText)


class IssueConnection(relay.Connection):
    class Meta:
        node = IssueQueryResultType


class TraceFrameConnection(relay.Connection):
    class Meta:
        node = TraceFrameQueryResultType


class Query(graphene.ObjectType):
    node = relay.Node.Field()
    issues = relay.ConnectionField(
        IssueConnection,
        codes=graphene.List(graphene.Int, default_value=["%"]),
        callables=graphene.List(graphene.String, default_value=["%"]),
        file_names=graphene.List(graphene.String, default_value=["%"]),
        min_trace_length_to_sinks=graphene.Int(),
        max_trace_length_to_sinks=graphene.Int(),
        min_trace_length_to_sources=graphene.Int(),
        max_trace_length_to_sources=graphene.Int(),
    )
    trace = relay.ConnectionField(TraceFrameConnection, issue_id=graphene.ID())

    def resolve_issues(
        self,
        info: ResolveInfo,
        codes: List[int],
        callables: List[str],
        file_names: List[str],
        min_trace_length_to_sinks: Optional[int] = None,
        max_trace_length_to_sinks: Optional[int] = None,
        min_trace_length_to_sources: Optional[int] = None,
        max_trace_length_to_sources: Optional[int] = None,
        **args
    ) -> List[IssueQueryResult]:
        session = get_session(info.context)
        run_id = Query.latest_run_id(session)

        builder = (
            IssueQueryBuilder(run_id)
            .with_session(session)
            .where_codes_is_any_of(codes)
            .where_callables_is_any_of(callables)
            .where_file_names_is_any_of(file_names)
            .where_trace_length_to_sinks(
                min_trace_length_to_sinks, max_trace_length_to_sinks
            )
            .where_trace_length_to_sources(
                min_trace_length_to_sources, max_trace_length_to_sources
            )
        )

        return builder.get()

    def resolve_trace(
        self, info: ResolveInfo, issue_id: DBID, **args
    ) -> List[TraceFrameQueryResult]:
        session = info.context.get("session")

        run_id = DBID(Query.latest_run_id(session))

        issue = (
            IssueQueryBuilder(run_id)
            .get_session_query(session)
            .filter(IssueInstance.id == issue_id)
            .join(Issue, IssueInstance.issue_id == Issue.id)
            .first()
        )

        leaf_kinds = Query.all_leaf_kinds(session)

        builder = IssueQueryBuilder(run_id)
        sources = builder.get_leaves_issue_instance(
            session, issue.id, SharedTextKind.SOURCE
        )
        sinks = builder.get_leaves_issue_instance(
            session, issue.id, SharedTextKind.SINK
        )

        postcondition_navigation = TraceOperator.navigate_trace_frames(
            leaf_kinds,
            session,
            run_id,
            sources,
            sinks,
            TraceOperator.initial_trace_frames(
                session, issue.id, TraceKind.POSTCONDITION
            ),
        )
        precondition_navigation = TraceOperator.navigate_trace_frames(
            leaf_kinds,
            session,
            run_id,
            sources,
            sinks,
            TraceOperator.initial_trace_frames(
                session, issue.id, TraceKind.PRECONDITION
            ),
        )

        trace_frames = (
            [frame_tuple[0] for frame_tuple in reversed(postcondition_navigation)]
            + [
                TraceFrameQueryResult(
                    id=DBID(0),
                    caller="",
                    caller_port="",
                    callee=issue.callable,
                    callee_port="root",
                    filename=issue.filename,
                    callee_location=issue.location,
                )
            ]
            + [frame_tuple[0] for frame_tuple in precondition_navigation]
        )

        return [
            frame._replace(file_content=Query.file_content(frame.filename))
            for frame in trace_frames
            if frame.filename
        ]

    @staticmethod
    def all_leaf_kinds(
        session: Session,
    ) -> Tuple[Dict[int, str], Dict[int, str], Dict[int, str]]:
        return (
            {
                int(id): contents
                for id, contents in session.query(
                    SharedText.id, SharedText.contents
                ).filter(SharedText.kind == SharedTextKind.SOURCE)
            },
            {
                int(id): contents
                for id, contents in session.query(
                    SharedText.id, SharedText.contents
                ).filter(SharedText.kind == SharedTextKind.SINK)
            },
            {
                int(id): contents
                for id, contents in session.query(
                    SharedText.id, SharedText.contents
                ).filter(SharedText.kind == SharedTextKind.FEATURE)
            },
        )

    @staticmethod
    def latest_run_id(session: Session) -> DBID:
        return DBID(
            (
                session.query(func.max(Run.id))
                .filter(Run.status == RunStatus.FINISHED)
                .scalar()
            )
        )

    @staticmethod
    def file_content(filename: str) -> str:
        repository_directory = os.getcwd()
        file_path = os.path.join(repository_directory, filename)
        try:
            with open(file_path, "r") as file:
                return "".join(file.readlines())
        except FileNotFoundError:
            return "File not found"


schema = graphene.Schema(query=Query, auto_camelcase=False)
