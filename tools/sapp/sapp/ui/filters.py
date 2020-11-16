# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import logging
from abc import ABC, abstractmethod
from typing import TYPE_CHECKING, Generic, List, Optional, Sequence, Set, TypeVar, Union

import graphene
from sqlalchemy import Column, Integer, String
from sqlalchemy.orm import Session
from sqlalchemy.orm.query import Query
from sqlalchemy.sql.expression import or_
from typing_extensions import Final

from ..models import DBID, Base


if TYPE_CHECKING:
    from .issues import IssueQueryResult  # noqa


LOG: logging.Logger = logging.getLogger(__name__)


_Q = TypeVar("_Q")
_T = TypeVar("_T")


class Predicate(ABC):
    pass


class QueryPredicate(Predicate):
    @abstractmethod
    def apply(self, query: Query[_Q]) -> Query[_Q]:
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

    def apply(self, query: Query[_Q]) -> Query[_Q]:
        if self._lower is not None:
            query = query.filter(self._column >= self._lower)
        if self._upper is not None:
            query = query.filter(self._column <= self._upper)
        return query


class Equals(Generic[_T], QueryPredicate):
    def __init__(self, column: Union[Column[_T], DBID], to: _T) -> None:
        self._column = column
        self._to: Final[Optional[_T]] = to

    def apply(self, query: Query[_Q]) -> Query[_Q]:
        return query.filter(self._column == self._to)


class IsNull(Generic[_T], QueryPredicate):
    def __init__(self, column: Union[Column[_T], DBID]) -> None:
        self._column = column

    def apply(self, query: Query[_Q]) -> Query[_Q]:
        return query.filter(self._column is None)


class Like(Generic[_T], QueryPredicate):
    def __init__(self, column: Union[Column[_T], DBID], items: Sequence[_T]) -> None:
        self._column = column
        self._items = items

    def apply(self, query: Query[_Q]) -> Query[_Q]:
        # pyre-ignore: SQLAlchemy too dynamic.
        return query.filter(or_(*[self._column.like(item) for item in self._items]))


class IssuePredicate(Predicate):
    @abstractmethod
    def apply(self, issues: List[IssueQueryResult]) -> List[IssueQueryResult]:
        ...


class HasAll(IssuePredicate):
    def __init__(self, features: Set[str]) -> None:
        self._features = features

    def apply(self, issues: List[IssueQueryResult]) -> List[IssueQueryResult]:
        return [
            issue
            for issue in issues
            if issue.features & self._features == self._features
        ]


class HasAny(IssuePredicate):
    def __init__(self, features: Set[str]) -> None:
        self._features = features

    def apply(self, issues: List[IssueQueryResult]) -> List[IssueQueryResult]:
        return [issue for issue in issues if len(issue.features & self._features) > 0]


class HasNone(IssuePredicate):
    def __init__(self, features: Set[str]) -> None:
        self._features = features

    def apply(self, issues: List[IssueQueryResult]) -> List[IssueQueryResult]:
        return [issue for issue in issues if len(issue.features & self._features) == 0]


class Filter(graphene.ObjectType):
    name = graphene.String(required=True)
    description = graphene.String()
    json = graphene.String()

    @staticmethod
    def from_record(record: FilterRecord) -> Filter:
        return Filter(
            name=record.name, description=record.description, json=record.json
        )


class FilterRecord(Base):
    __tablename__ = "filters"

    name: Column[str] = Column(
        String(length=255), nullable=False, unique=True, primary_key=True
    )
    description: Column[Optional[str]] = Column(String(length=1024), nullable=True)

    json: Column[str] = Column(
        String(length=1024), nullable=False, doc="JSON representation of the filter"
    )

    @staticmethod
    def from_filter(filter: Filter) -> FilterRecord:
        return FilterRecord(
            # pyre-ignore[6]: graphene too dynamic.
            name=filter.name,
            description=filter.description,
            json=filter.json,
        )


def all_filters(session: Session) -> List[Filter]:
    return [Filter.from_record(record) for record in session.query(FilterRecord).all()]


def save_filter(session: Session, filter: Filter) -> None:
    LOG.debug(f"Storing {filter}")
    session.add(FilterRecord.from_filter(filter))
    session.commit()


class EmptyDeletionError(Exception):
    pass


def delete_filter(session: Session, name: str) -> None:
    LOG.debug(f"Deleting {name}")
    deleted_rows = (
        session.query(FilterRecord).filter(FilterRecord.name == name).delete()
    )
    if deleted_rows == 0:
        raise EmptyDeletionError(f'No filter with `name` "{name}" exists.')
    session.commit()
