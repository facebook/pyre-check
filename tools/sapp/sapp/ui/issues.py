# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

from abc import ABC, abstractmethod
from collections import defaultdict
from enum import Enum
from typing import (
    Dict,
    Generic,
    List,
    NamedTuple,
    Optional,
    Sequence,
    Set,
    TypeVar,
    Union,
)

import graphene
from graphql.execution.base import ResolveInfo
from sqlalchemy import Column
from sqlalchemy.orm import Session, aliased
from sqlalchemy.orm.query import Query as RawQuery
from sqlalchemy.sql.expression import or_
from typing_extensions import Final

from ..models import (
    DBID,
    Issue,
    IssueInstance,
    IssueInstanceSharedTextAssoc,
    SharedText,
    SharedTextKind,
    SourceLocation,
)


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


def _query(info: ResolveInfo) -> "Query":
    # TODO(T71492980): queries here are run-independent and should be factored out.
    return Query(info.context["session"], 1)


# pyre-ignore[13]: unitialized class attribute
class IssueQueryResultType(graphene.ObjectType):
    id: DBID

    class Meta:
        interfaces = (graphene.relay.Node,)

    issue_id = graphene.ID()

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

    min_trace_length_to_sources = graphene.Int()
    min_trace_length_to_sinks = graphene.Int()

    def resolve_issue_id(self, info: ResolveInfo) -> DBID:
        return self.id

    def resolve_sources(self, info: ResolveInfo) -> List[str]:
        return list(_query(info).sources(self.id))

    def resolve_source_names(self, info: ResolveInfo) -> List[str]:
        return list(_query(info).source_names(self.id))

    def resolve_sinks(self, info: ResolveInfo) -> List[str]:
        return list(_query(info).sinks(self.id))

    def resolve_sink_names(self, info: ResolveInfo) -> List[str]:
        return list(_query(info).sink_names(self.id))

    def resolve_features(self, info: ResolveInfo) -> List[str]:
        return list(_query(info).features(self.id))


class IssueQueryResult(NamedTuple):
    id: DBID

    code: int
    message: str

    callable: str

    filename: str
    location: SourceLocation

    min_trace_length_to_sources: int
    min_trace_length_to_sinks: int


class FilterEnum(Enum):
    codes = "codes"
    callables = "callables"
    file_names = "file_names"
    trace_length_to_sources = "trace_length_to_sources"
    trace_length_to_sinks = "trace_length_to_sinks"
    any_features = "any_features"
    all_features = "all_features"
    exclude_features = "exclude_features"
    issue_id = "issue_id"


_Q = TypeVar("_Q")
_T = TypeVar("_T")


class Predicate(ABC):
    pass


class QueryPredicate(Predicate):
    @abstractmethod
    def apply(self, query: RawQuery[_Q]) -> RawQuery[_Q]:
        ...


class InRange(Generic[_T], QueryPredicate):
    def __init__(
        self,
        column: Union[Column[_T], DBID],
        *,
        lower: Optional[_T] = None,
        upper: Optional[_T] = None,
    ) -> None:
        self._column = column
        self._lower: Final[Optional[_T]] = lower
        self._upper: Final[Optional[_T]] = upper

    def apply(self, query: RawQuery[_Q]) -> RawQuery[_Q]:
        if self._lower is not None:
            query = query.filter(self._column >= self._lower)
        if self._upper is not None:
            query = query.filter(self._column <= self._upper)
        return query


class Equals(Generic[_T], QueryPredicate):
    def __init__(self, column: Union[Column[_T], DBID], to: _T) -> None:
        self._column = column
        self._to: Final[Optional[_T]] = to

    def apply(self, query: RawQuery[_Q]) -> RawQuery[_Q]:
        return query.filter(self._column == self._to)


class IsNull(Generic[_T], QueryPredicate):
    def __init__(self, column: Union[Column[_T], DBID]) -> None:
        self._column = column

    def apply(self, query: RawQuery[_Q]) -> RawQuery[_Q]:
        return query.filter(self._column is None)


class Like(Generic[_T], QueryPredicate):
    def __init__(self, column: Union[Column[_T], DBID], items: Sequence[_T]) -> None:
        self._column = column
        self._items = items

    def apply(self, query: RawQuery[_Q]) -> RawQuery[_Q]:
        # pyre-ignore: SQLAlchemy too dynamic.
        return query.filter(or_(*[self._column.like(item) for item in self._items]))


class IssuePredicate(Predicate):
    # TODO(T71492980): migrate to query filters to remove bottleneck.
    @abstractmethod
    def apply(
        self, issues: List[IssueQueryResult], feature_map: Dict[int, Set[str]]
    ) -> List[IssueQueryResult]:
        ...


class HasAll(IssuePredicate):
    def __init__(self, features: Set[str]) -> None:
        self._features = features

    def apply(
        self, issues: List[IssueQueryResult], feature_map: Dict[int, Set[str]]
    ) -> List[IssueQueryResult]:
        return [
            issue
            for issue in issues
            if feature_map[int(issue.id)] & self._features == self._features
        ]


class HasAny(IssuePredicate):
    def __init__(self, features: Set[str]) -> None:
        self._features = features

    def apply(
        self, issues: List[IssueQueryResult], feature_map: Dict[int, Set[str]]
    ) -> List[IssueQueryResult]:
        return [
            issue
            for issue in issues
            if len(feature_map[int(issue.id)] & self._features) > 0
        ]


class HasNone(IssuePredicate):
    def __init__(self, features: Set[str]) -> None:
        self._features = features

    def apply(
        self, issues: List[IssueQueryResult], feature_map: Dict[int, Set[str]]
    ) -> List[IssueQueryResult]:
        return [
            issue
            for issue in issues
            if len(feature_map[int(issue.id)] & self._features) == 0
        ]


class Query:
    # pyre-fixme[3]: Return type must be annotated.
    def __init__(self, session: Session, current_run_id: Union[DBID, int]):
        self._session: Session = session
        self._predicates: List[Predicate] = []
        self.current_run_id = current_run_id
        self.breadcrumb_filters: Dict[FilterEnum, List[str]] = defaultdict(list)

    @property
    # pyre-fixme[3]: Return type must be annotated.
    def session(self):
        if self._session is None:
            raise Exception("No current session found for query!")
        return self._session

    def get(self) -> List[IssueQueryResult]:
        query = self.get_raw_query()

        for predicate in self._predicates:
            if isinstance(predicate, QueryPredicate):
                query = predicate.apply(query)

        issues = list(
            query.join(Issue, IssueInstance.issue_id == Issue.id).join(
                MessageText, MessageText.id == IssueInstance.message_id
            )
        )

        issue_predicates = [
            predicate
            for predicate in self._predicates
            if isinstance(predicate, IssuePredicate)
        ]
        if len(issue_predicates) > 0:
            # We only do this when specified because the operation is expensive
            feature_map = {
                int(issue.id): self.get_leaves_issue_instance(
                    self._session, int(issue.id), SharedTextKind.FEATURE
                )
                for issue in issues
            }
            for issue_predicate in issue_predicates:
                issues = issue_predicate.apply(issues, feature_map)

        return issues

    def where(self, *predicates: Predicate) -> "Query":
        self._predicates.extend(predicates)
        return self

    def where_issue_id_is(self, issue_id: Optional[int]) -> "Query":
        if issue_id is not None:
            self._predicates.append(Equals(IssueInstance.id, issue_id))
        return self

    def where_codes_is_any_of(self, codes: List[int]) -> "Query":
        return self.where(Like(Issue.code, codes))

    def where_callables_is_any_of(self, callables: List[str]) -> "Query":
        return self.where(Like(CallableText.contents, callables))

    def where_file_names_is_any_of(self, file_names: List[str]) -> "Query":
        return self.where(Like(FilenameText.contents, file_names))

    def where_trace_length_to_sinks(
        self, minimum: Optional[int] = None, maximum: Optional[int] = None
    ) -> "Query":
        return self.where(
            InRange(
                IssueInstance.min_trace_length_to_sinks, lower=minimum, upper=maximum
            )
        )

    def where_trace_length_to_sources(
        self, minimum: Optional[int] = None, maximum: Optional[int] = None
    ) -> "Query":
        return self.where(
            InRange(
                IssueInstance.min_trace_length_to_sources, lower=minimum, upper=maximum
            )
        )

    def where_any_features(self, features: List[str]) -> "Query":
        return self.where(HasAny(set(features)))

    def where_all_features(self, features: List[str]) -> "Query":
        return self.where(HasAll(set(features)))

    def where_exclude_features(self, features: List[str]) -> "Query":
        return self.where(HasNone(set(features)))

    def sources(self, issue_id: DBID) -> Set[str]:
        return self.get_leaves_issue_instance(
            self._session, int(issue_id), SharedTextKind.SOURCE
        )

    def source_names(self, issue_id: DBID) -> Set[str]:
        return self.get_leaves_issue_instance(
            self._session, int(issue_id), SharedTextKind.SOURCE_DETAIL
        )

    def sinks(self, issue_id: DBID) -> Set[str]:
        return self.get_leaves_issue_instance(
            self._session, int(issue_id), SharedTextKind.SINK
        )

    def sink_names(self, issue_id: DBID) -> Set[str]:
        return self.get_leaves_issue_instance(
            self._session, int(issue_id), SharedTextKind.SINK_DETAIL
        )

    def features(self, issue_id: DBID) -> Set[str]:
        return self.get_leaves_issue_instance(
            self._session, int(issue_id), SharedTextKind.FEATURE
        )

    # pyre-fixme[24]: Generic type `RawQuery` expects 1 type parameter.
    def get_raw_query(self) -> RawQuery:
        return (
            self._session.query(
                IssueInstance.id,
                FilenameText.contents.label("filename"),
                IssueInstance.location,
                Issue.code,
                CallableText.contents.label("callable"),
                MessageText.contents.label("message"),
                IssueInstance.min_trace_length_to_sources,
                IssueInstance.min_trace_length_to_sinks,
            )
            .filter(IssueInstance.run_id == self.current_run_id)
            .join(FilenameText, FilenameText.id == IssueInstance.filename_id)
            .join(CallableText, CallableText.id == IssueInstance.callable_id)
        )

    def get_leaves_issue_instance(
        self, session: Session, issue_instance_id: int, kind: SharedTextKind
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
        return self._leaf_dict_lookups(message_ids, kind, session)

    def _leaf_dict_lookups(
        self, message_ids: List[int], kind: SharedTextKind, session: Session
    ) -> Set[str]:
        leaf_dict = {
            int(id): contents
            for id, contents in session.query(
                SharedText.id, SharedText.contents
            ).filter(SharedText.kind == kind)
        }
        return {leaf_dict[id] for id in message_ids if id in leaf_dict}
