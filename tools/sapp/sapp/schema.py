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
    issues = relay.ConnectionField(IssueConnection)

    def resolve_issues(self, info: ResolveInfo, **args) -> List[IssueQueryResult]:
        session = get_session(info.context)
        latest_run_id = (
            session.query(func.max(Run.id))
            .filter(Run.status == RunStatus.FINISHED)
            .scalar()
        )
        builder = IssueQueryBuilder(latest_run_id).with_session(session)

        return builder.get()


schema = graphene.Schema(query=Query, auto_camelcase=False)
