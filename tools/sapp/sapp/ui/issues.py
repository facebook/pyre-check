# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

from typing import Any, List, NamedTuple, Optional, Set

import graphene
from graphql.execution.base import ResolveInfo
from sqlalchemy import func
from sqlalchemy.orm import Session, aliased

from ..models import (
    DBID,
    Issue,
    IssueInstance,
    IssueInstanceSharedTextAssoc,
    SharedText,
    SharedTextKind,
    SourceLocation,
)
from . import filters, run


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
# pyre-fixme[5]: Global expression must be annotated.
FeatureText = aliased(SharedText)


# pyre-ignore[13]: unitialized class attribute
class IssueQueryResultType(graphene.ObjectType):
    concatenated_features: str

    class Meta:
        interfaces = (graphene.relay.Node,)

    issue_id = graphene.ID()
    issue_instance_id = graphene.ID()

    code = graphene.Int()
    message = graphene.String()

    callable = graphene.String()

    filename = graphene.String()
    location = graphene.String()

    sources = graphene.List(graphene.String)
    source_names = graphene.List(graphene.String)
    sinks = graphene.List(graphene.String)
    sink_names = graphene.List(graphene.String)
    features = graphene.List(graphene.String)

    is_new_issue = graphene.Boolean()

    min_trace_length_to_sources = graphene.Int()
    min_trace_length_to_sinks = graphene.Int()

    def resolve_sources(self, info: ResolveInfo) -> List[str]:
        # pyre-ignore[6]: graphene too dynamic.
        return list(sources(info.context["session"], self.issue_instance_id))

    def resolve_source_names(self, info: ResolveInfo) -> List[str]:
        # pyre-ignore[6]: graphene too dynamic.
        return list(source_names(info.context["session"], self.issue_instance_id))

    def resolve_sinks(self, info: ResolveInfo) -> List[str]:
        # pyre-ignore[6]: graphene too dynamic.
        return list(sinks(info.context["session"], self.issue_instance_id))

    def resolve_sink_names(self, info: ResolveInfo) -> List[str]:
        # pyre-ignore[6]: graphene too dynamic.
        return list(sink_names(info.context["session"], self.issue_instance_id))

    def resolve_features(self, info: ResolveInfo) -> List[str]:
        # pyre-ignore[6]: graphene too dynamic.
        return sorted(self.features)


class IssueQueryResult(NamedTuple):
    issue_id: DBID
    issue_instance_id: DBID

    code: int
    message: str

    callable: str

    filename: str
    location: SourceLocation

    is_new_issue: bool

    min_trace_length_to_sources: int
    min_trace_length_to_sinks: int

    features: Set[str]

    @staticmethod
    # pyre-fixme[2]: Parameter annotation cannot be `Any`.
    def from_record(record: Any) -> IssueQueryResult:
        return IssueQueryResult(
            issue_id=record.issue_id,
            issue_instance_id=record.issue_instance_id,
            code=record.code,
            message=record.message,
            callable=record.callable,
            filename=record.filename,
            location=record.location,
            is_new_issue=record.is_new_issue,
            min_trace_length_to_sources=record.min_trace_length_to_sources,
            min_trace_length_to_sinks=record.min_trace_length_to_sinks,
            features=set(record.concatenated_features.split(","))
            if record.concatenated_features
            else set(),
        )


class Instance:
    def __init__(self, session: Session, run_id: Optional[DBID] = None) -> None:
        self._session: Session = session
        self._predicates: List[filters.Predicate] = []
        self._run_id: DBID = run_id or run.latest(session)

    def get(self) -> List[IssueQueryResult]:
        features = (
            self._session.query(
                # pyre-ignore: SQAlchemy sadness.
                IssueInstance.id.label("id"),
                func.group_concat(FeatureText.contents.distinct()).label(
                    "concatenated_features"
                ),
            )
            .join(
                IssueInstanceSharedTextAssoc,
                IssueInstanceSharedTextAssoc.issue_instance_id == IssueInstance.id,
                isouter=True,
            )
            .join(
                FeatureText,
                FeatureText.id == IssueInstanceSharedTextAssoc.shared_text_id,
                isouter=True,
            )
            .filter(FeatureText.kind == SharedTextKind.FEATURE)
            .group_by(IssueInstance)
            .subquery()
        )
        query = (
            self._session.query(
                IssueInstance.id.label("issue_instance_id"),
                FilenameText.contents.label("filename"),
                IssueInstance.location,
                # pyre-ignore[16]: SQLAlchemy
                Issue.id.label("issue_id"),
                Issue.code,
                CallableText.contents.label("callable"),
                MessageText.contents.label("message"),
                IssueInstance.is_new_issue,
                IssueInstance.min_trace_length_to_sources,
                IssueInstance.min_trace_length_to_sinks,
                features.c.concatenated_features,
            )
            .filter(IssueInstance.run_id == self._run_id)
            .join(FilenameText, FilenameText.id == IssueInstance.filename_id)
            .join(CallableText, CallableText.id == IssueInstance.callable_id)
            .join(features, IssueInstance.id == features.c.id, isouter=True)
        )

        for predicate in self._predicates:
            if isinstance(predicate, filters.QueryPredicate):
                query = predicate.apply(query)

        issues = [
            IssueQueryResult.from_record(record)
            for record in query.join(Issue, IssueInstance.issue_id == Issue.id).join(
                MessageText, MessageText.id == IssueInstance.message_id
            )
        ]

        issue_predicates = [
            predicate
            for predicate in self._predicates
            if isinstance(predicate, filters.IssuePredicate)
        ]
        if len(issue_predicates) > 0:
            for issue_predicate in issue_predicates:
                issues = issue_predicate.apply(issues)

        return issues

    def where(self, *predicates: filters.Predicate) -> "Instance":
        self._predicates.extend(predicates)
        return self

    def where_issue_instance_id_is(self, issue_id: Optional[int]) -> "Instance":
        if issue_id is not None:
            self._predicates.append(filters.Equals(IssueInstance.id, issue_id))
        return self

    def where_is_new_issue(self, is_new_issue: Optional[bool]) -> "Instance":
        if is_new_issue:
            self._predicates.append(filters.Equals(IssueInstance.is_new_issue, True))
        return self

    def where_codes_is_any_of(self, codes: List[int]) -> "Instance":
        return self.where(filters.Like(Issue.code, codes))

    def where_callables_is_any_of(self, callables: List[str]) -> "Instance":
        return self.where(filters.Like(CallableText.contents, callables))

    def where_path_is_any_of(self, paths: List[str]) -> "Instance":
        return self.where(filters.Like(FilenameText.contents, paths))

    def where_trace_length_to_sinks(
        self, minimum: Optional[int] = None, maximum: Optional[int] = None
    ) -> "Instance":
        return self.where(
            filters.InRange(
                IssueInstance.min_trace_length_to_sinks, lower=minimum, upper=maximum
            )
        )

    def where_trace_length_to_sources(
        self, minimum: Optional[int] = None, maximum: Optional[int] = None
    ) -> "Instance":
        return self.where(
            filters.InRange(
                IssueInstance.min_trace_length_to_sources, lower=minimum, upper=maximum
            )
        )

    def where_any_features(self, features: List[str]) -> "Instance":
        return self.where(filters.HasAny(set(features)))

    def where_all_features(self, features: List[str]) -> "Instance":
        return self.where(filters.HasAll(set(features)))

    def where_exclude_features(self, features: List[str]) -> "Instance":
        return self.where(filters.HasNone(set(features)))


def sources(session: Session, issue_id: DBID) -> Set[str]:
    return _get_leaves(session, issue_id, SharedTextKind.SOURCE)


def source_names(session: Session, issue_id: DBID) -> Set[str]:
    return _get_leaves(session, issue_id, SharedTextKind.SOURCE_DETAIL)


def sinks(session: Session, issue_id: DBID) -> Set[str]:
    return _get_leaves(session, issue_id, SharedTextKind.SINK)


def sink_names(session: Session, issue_id: DBID) -> Set[str]:
    return _get_leaves(session, issue_id, SharedTextKind.SINK_DETAIL)


def features(session: Session, issue_id: DBID) -> Set[str]:
    return _get_leaves(session, issue_id, SharedTextKind.FEATURE)


def _get_leaves(
    session: Session, issue_instance_id: DBID, kind: SharedTextKind
) -> Set[str]:
    message_ids = [
        int(id)
        for id, in session.query(SharedText.id)
        .distinct(SharedText.id)
        .join(
            IssueInstanceSharedTextAssoc,
            SharedText.id == IssueInstanceSharedTextAssoc.shared_text_id,
        )
        .filter(IssueInstanceSharedTextAssoc.issue_instance_id == issue_instance_id)
        .filter(SharedText.kind == kind)
    ]

    leaf_lookup = {
        int(id): contents
        for id, contents in session.query(SharedText.id, SharedText.contents).filter(
            SharedText.kind == kind
        )
    }
    return {leaf_lookup[id] for id in message_ids if id in leaf_lookup}
