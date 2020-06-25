from __future__ import annotations

from enum import Enum
from typing import Any, Dict, List

from sqlalchemy.orm import Session, aliased
from sqlalchemy.orm.query import Query
from sqlalchemy.sql.expression import or_

from .db import DB
from .models import Issue, IssueInstance, SharedText


FilenameText = aliased(SharedText)
CallableText = aliased(SharedText)
CallerText = aliased(SharedText)
CalleeText = aliased(SharedText)
MessageText = aliased(SharedText)


class Filter(Enum):
    codes = "codes"


class IssueQueryBuilder:
    def __init__(self, database: DB, current_run_id: int):
        self.db = database
        self.current_run_id = current_run_id
        self.issue_filters: Dict[Filter, Any] = {}

    def get(self) -> Query:
        with self.db.make_session() as session:
            query = self._get_session_query(session)
            for filter_type, filter_condition in self.issue_filters.items():
                if filter_type == Filter.codes:
                    query = query.filter(
                        or_(*[Issue.code.like(item) for item in filter_condition])
                    )
            return query.join(Issue, IssueInstance.issue_id == Issue.id).join(
                MessageText, MessageText.id == IssueInstance.message_id
            )

    def where_codes(self, codes: List[int]) -> IssueQueryBuilder:
        if Filter.codes in self.issue_filters:
            self.issue_filters[Filter.codes] = self.issue_filters[Filter.codes] + codes
        else:
            self.issue_filters[Filter.codes] = codes
        return self

    def _get_session_query(self, session: Session) -> Query:
        return (
            session.query(
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
