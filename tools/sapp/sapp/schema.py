# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

import graphene

from .models import IssueInstance


class Query(graphene.ObjectType):
    issue_instances = graphene.List(IssueInstance.generateSQLAlchemyObject())

    def resolve_issue_instances(self, info):
        query = IssueInstance.generateSQLAlchemyObject().get_query(info)
        return query.all()


schema = graphene.Schema(query=Query)
