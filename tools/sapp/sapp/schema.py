# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

from typing import List

import graphene
from graphene import relay
from graphene_sqlalchemy import get_session
from graphql.execution.base import ResolveInfo
from sqlalchemy.sql import func

from .interactive import IssueQueryResult, IssueQueryResultType
from .models import Run, RunStatus
from .query_builder import IssueQueryBuilder


class IssueConnection(relay.Connection):
    class Meta:
        node = IssueQueryResultType


class Query(graphene.ObjectType):
    node = relay.Node.Field()
    issues = relay.ConnectionField(
        IssueConnection,
        codes=graphene.List(graphene.Int, default_value=["%"]),
        callables=graphene.List(graphene.String, default_value=["%"]),
        file_names=graphene.List(graphene.String, default_value=["%"]),
        trace_length_to_sinks=graphene.List(graphene.String, default_value=["%"]),
        trace_length_to_sources=graphene.List(graphene.String, default_value=["%"]),
    )

    def resolve_issues(
        self,
        info: ResolveInfo,
        codes: List[int],
        callables: List[str],
        file_names: List[str],
        trace_length_to_sinks: List[str],
        trace_length_to_sources: List[str],
        **args
    ) -> List[IssueQueryResult]:
        session = get_session(info.context)
        latest_run_id = (
            session.query(func.max(Run.id))
            .filter(Run.status == RunStatus.FINISHED)
            .scalar()
        )

        builder = (
            IssueQueryBuilder(latest_run_id)
            .with_session(session)
            .where_codes_is_any_of(codes)
            .where_callables_is_any_of(callables)
            .where_file_names_is_any_of(file_names)
        )

        return builder.get()


schema = graphene.Schema(query=Query, auto_camelcase=False)
