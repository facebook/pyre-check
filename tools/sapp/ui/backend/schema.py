# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

import graphene
from graphene import relay
from graphene_sqlalchemy import SQLAlchemyConnectionField, SQLAlchemyObjectType

from .models import Issue as IssueModel, Run as RunModel


class Run(SQLAlchemyObjectType):
    class Meta:
        model = RunModel
        interfaces = (relay.Node,)


class RunConnection(relay.Connection):
    class Meta:
        node = Run


class Issue(SQLAlchemyObjectType):
    class Meta:
        model = IssueModel
        interfaces = (relay.Node,)


class IssueConnection(relay.Connection):
    class Meta:
        node = Issue


class IssuesConnection(SQLAlchemyConnectionField):
    @classmethod
    def get_query(cls, model, info, **args):
        if "run_id" in args:
            return model.query.filter_by(run=args["run_id"])
        return model.query


class Query(graphene.ObjectType):
    node = relay.Node.Field()

    run = relay.Node.Field(Run)
    issue = relay.Node.Field(Issue)

    all_runs = SQLAlchemyConnectionField(RunConnection)
    all_issues = IssuesConnection(
        IssueConnection, sort=None, args={"run_id": graphene.Argument(graphene.Int)}
    )


schema = graphene.Schema(query=Query)
