# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

from typing import TYPE_CHECKING, List

import graphene
from graphene import relay

from .models import Issue, IssueInstance, SharedText


if TYPE_CHECKING:
    from graphql.execution.base import ResolveInfo


if TYPE_CHECKING:
    from graphql.execution.base import ResolveInfo


SharedTextType = SharedText.generateSQLAlchemyObject()
IssueInstanceType = IssueInstance.generateSQLAlchemyObject()
IssueType = Issue.generateSQLAlchemyObject()


class IssueConnection(relay.Connection):
    class Meta:
        node = IssueInstanceType


class Query(graphene.ObjectType):
    node = relay.Node.Field()
    issue_instances = relay.ConnectionField(IssueConnection)

    def resolve_issue_instances(
        self, info: "ResolveInfo", **args
    ) -> List[IssueInstance]:
        issue_instances_query = IssueInstanceType.get_query(info)
        return issue_instances_query.all()


schema = graphene.Schema(query=Query)
