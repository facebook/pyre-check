# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

import graphene

from .models import IssueInstance, SharedText


SharedTextType = SharedText.generateSQLAlchemyObject()
IssueInstanceType = IssueInstance.generateSQLAlchemyObject()


class Query(graphene.ObjectType):
    issue_instances = graphene.List(IssueInstanceType)

    def resolve_issue_instances(self, info):
        query = IssueInstanceType.get_query(info)
        return query.all()


schema = graphene.Schema(query=Query)
