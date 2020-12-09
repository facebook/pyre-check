# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from __future__ import annotations

import enum
import logging
from datetime import datetime
from decimal import Decimal
from itertools import islice
from typing import Any, Dict, Optional, List, Set, Type

from graphene_sqlalchemy.converter import (
    convert_column_to_int_or_id,
    convert_column_to_string,
    convert_sqlalchemy_type,
)
from sqlalchemy import (
    Boolean,
    Column,
    DateTime,
    Enum,
    Float,
    Index,
    Integer,
    String,
    func,
    types,
)
from sqlalchemy.dialects import mysql, sqlite
from sqlalchemy.dialects.mysql import BIGINT
from sqlalchemy.exc import NoSuchTableError
from sqlalchemy.ext.associationproxy import association_proxy
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import Session, relationship

from .db import DB
from .db_support import (
    DBID,
    BIGDBIDType,
    DBIDType,
    MutableRecordMixin,
    PrepareMixin,
    PrimaryKeyBase,
    PrimaryKeyGeneratorBase,
    RecordMixin,
)
from .decorators import classproperty
from .errors import AIException


log: logging.Logger = logging.getLogger("sapp")


Base = declarative_base()
INNODB_MAX_INDEX_LENGTH = 767
HANDLE_LENGTH = 255
MESSAGE_LENGTH = 4096
SHARED_TEXT_LENGTH = 4096

"""Models used to represent DB entries

An Issue is a particular problem found. It can exist across multiple commits.  A
Run is a single run of Zoncolan over a specific commit. It may find new Issues,
or existing Issues.  Each run is tied to Issues through IssueInstances.
IssueInstances have per run information, like source location, while Issues have
attributes like the status of an issue.
"""


class SourceLocation(object):
    """The location in a source file that an error occurred in

    If end_column is defined then we have a range, otherwise it defaults to
    begin_column and we have a single point.
    """

    def __init__(self, line_no, begin_column, end_column=None) -> None:
        self.line_no = line_no
        self.begin_column = begin_column
        self.end_column = end_column or self.begin_column

    def __eq__(self, other):
        return (
            self.line_no == other.line_no
            and self.begin_column == other.begin_column
            and self.end_column == other.end_column
        )

    def __str__(self):
        return SourceLocation.to_string(self)

    @staticmethod
    def from_string(location_string) -> SourceLocation:
        location_points = location_string.split("|")
        assert len(location_points) == 3, "Invalid location string %s" % location_string
        return SourceLocation(*location_points)

    @staticmethod
    def to_string(location) -> str:
        return "|".join(
            map(str, [location.line_no, location.begin_column, location.end_column])
        )


class CaseSensitiveStringType(types.TypeDecorator):
    impl = types.String

    def load_dialect_impl(self, dialect):
        if dialect.name == "mysql":
            return dialect.type_descriptor(
                mysql.VARCHAR(length=255, collation="latin1_general_cs")
            )
        elif dialect.name == "sqlite":
            return dialect.type_descriptor(
                sqlite.VARCHAR(length=255, collation="binary")
            )
        else:
            raise AIException("%s not supported" % dialect.name)


class SourceLocationType(types.TypeDecorator):
    """Defines a new type of SQLAlchemy to store source locations.

    In python land we use SourceLocation, but when stored in the databae we just
    split the fields with |
    """

    impl = types.String

    def __init__(self) -> None:
        super(SourceLocationType, self).__init__(length=255)

    def process_bind_param(self, value, dialect):
        """
        SQLAlchemy uses this to convert a SourceLocation object into a string.
        """
        if value is None:
            return None
        return SourceLocation.to_string(value)

    def process_result_value(self, value, dialect) -> Optional[SourceLocation]:
        """
        SQLAlchemy uses this to convert a string into a SourceLocation object.
        We separate the fields by a |
        """
        if value is None:
            return None

        p = value.split("|")

        if len(p) == 0:
            return None
        return SourceLocation(*map(int, p))


class SourceLocationsType(types.TypeDecorator):
    """Defines a type to store multiple source locations in a single string"""

    impl = types.String

    def __init__(self) -> None:
        super(SourceLocationsType, self).__init__(length=4096)

    def process_bind_param(self, value, dialect) -> Optional[str]:
        if value is None:
            return None
        return ",".join([SourceLocation.to_string(location) for location in value])

    def process_result_value(self, value, dialect):
        if value is None or value == "":
            return []
        assert isinstance(value, str), "Invalid SourceLocationsType %s" % str(value)
        locations = value.split(",")
        return [SourceLocation.from_string(location) for location in locations]


# See Issue.merge for information about replace_assocs


class IssueDBID(DBID):
    __slots__ = ["replace_assocs"]

    def __init__(self, id=None) -> None:
        super().__init__(id)
        self.replace_assocs = False


class IssueDBIDType(DBIDType):
    def process_result_value(self, value, dialect) -> IssueDBID:
        return IssueDBID(value)


class IssueBIGDBIDType(BIGDBIDType):
    def process_result_value(self, value, dialect) -> IssueDBID:
        return IssueDBID(value)


class IssueInstanceTraceFrameAssoc(Base, PrepareMixin, RecordMixin):  # noqa

    __tablename__ = "issue_instance_trace_frame_assoc"

    issue_instance_id = Column(
        "issue_instance_id", BIGDBIDType, primary_key=True, nullable=False
    )

    trace_frame_id = Column(
        "trace_frame_id", BIGDBIDType, primary_key=True, nullable=False, index=True
    )

    issue_instance = relationship(
        "IssueInstance",
        primaryjoin=(
            "IssueInstanceTraceFrameAssoc.issue_instance_id == "
            "foreign(IssueInstance.id)"
        ),
        uselist=False,
    )

    trace_frame = relationship(
        "TraceFrame",
        primaryjoin=(
            "IssueInstanceTraceFrameAssoc.trace_frame_id == " "foreign(TraceFrame.id)"
        ),
        uselist=False,
    )

    @classmethod
    def merge(cls, session, items):
        return cls._merge_assocs(
            session, items, cls.issue_instance_id, cls.trace_frame_id
        )


class SharedTextKind(enum.Enum):
    # Do NOT reorder the enums. Depending on the type of database, existing
    # DBs may have these enums represented internally as ints based on the
    # order shown here, and changing it here messes up existing data. This
    # also means that new enums should be added AT THE END of the list.
    feature = enum.auto()
    message = enum.auto()
    source = enum.auto()
    sink = enum.auto()
    callable = enum.auto()
    filename = enum.auto()
    source_detail = enum.auto()
    sink_detail = enum.auto()

    @classproperty
    def FEATURE(cls):  # noqa
        return cls.feature

    @classproperty
    def MESSAGE(cls):  # noqa
        return cls.message

    @classproperty
    def SOURCE(cls):  # noqa
        return cls.source

    @classproperty
    def SINK(cls):  # noqa
        return cls.sink

    @classproperty
    def CALLABLE(cls):  # noqa
        return cls.callable

    @classproperty
    def FILENAME(cls):  # noqa
        return cls.filename

    @classproperty
    def SOURCE_DETAIL(cls):  # noqa
        return cls.source_detail

    @classproperty
    def SINK_DETAIL(cls):  # noqa
        return cls.sink_detail

    @classmethod
    def from_string(cls, string: str) -> Optional[SharedTextKind]:
        return cls.__members__.get(string)


class SharedText(Base, PrepareMixin, RecordMixin):  # noqa
    """Any string-ish type that can be shared as a property of some other
    object. (e.g. features, sources, sinks). The table name 'messages' is due
    to legacy reasons."""

    __tablename__ = "messages"

    __table_args__ = (Index("ix_messages_handle", "contents", "kind"),)

    # pyre-fixme[8]: Attribute has type `DBID`; used as `Column[typing.Any]`.
    id: DBID = Column(BIGDBIDType, primary_key=True)

    # pyre-fixme[8]: Attribute has type `str`; used as `Column[str]`.
    contents: str = Column(
        String(length=SHARED_TEXT_LENGTH), nullable=False, index=True
    )

    # pyre-fixme[8]: Attribute has type `SharedTextKind`; used as `Column[str]`.
    kind: SharedTextKind = Column(
        Enum(SharedTextKind), server_default="feature", nullable=False, index=True
    )

    issue_instances = association_proxy("shared_text_issue_instance", "issue_instance")

    shared_text_issue_instance = relationship(
        "IssueInstanceSharedTextAssoc",
        primaryjoin=(
            "SharedText.id == foreign(IssueInstanceSharedTextAssoc.shared_text_id)"
        ),
    )

    trace_frames = association_proxy("shared_text_trace_frame", "trace_frames")

    shared_text_trace_frame = relationship(
        "TraceFrameLeafAssoc",
        primaryjoin=("SharedText.id == foreign(TraceFrameLeafAssoc.leaf_id)"),
    )

    @classmethod
    def merge(cls, session, items):
        return cls._merge_by_keys(
            session,
            items,
            lambda item: "%s:%s" % (item.contents, item.kind),
            cls.contents,
            cls.kind,
        )


class IssueInstanceSharedTextAssoc(Base, PrepareMixin, RecordMixin):  # noqa
    """Assoc table between issue instances and its properties that are
    representable by a string. The DB table name and column names are due to
    legacy reasons and warrant some explanation:
    - 'Features' used to be the only shared text of the assoc, now, the assoc
      also accounts for 'Sources' and 'Sinks' and possibly more.
    - 'messages' table used to be only for 'messages', now, it contains
      features, sources and sinks and possibly more.
    - It is expensive to rename the DB tables, so renaming only happened in
      the model. This is why it looks like we have 3 different terms for the
      same thing: 'messages', 'shared_text', 'features'.

    When in doubt, trust the property and method names used in the model and
    refer to the relationship joins for how objects relate to each other.
    """

    __tablename__ = "issue_instance_feature_assoc"

    issue_instance_id = Column(
        "issue_instance_id", BIGDBIDType, primary_key=True, nullable=False
    )

    shared_text_id = Column("feature_id", BIGDBIDType, primary_key=True, nullable=False)

    issue_instance = relationship(
        "IssueInstance",
        primaryjoin=(
            "IssueInstanceSharedTextAssoc.issue_instance_id =="
            "foreign(IssueInstance.id)"
        ),
        uselist=False,
    )

    shared_text = relationship(
        "SharedText",
        primaryjoin=(
            "IssueInstanceSharedTextAssoc.shared_text_id == " "foreign(SharedText.id)"
        ),
        uselist=False,
    )

    @classmethod
    def merge(cls, session, items):
        return cls._merge_assocs(
            session, items, cls.issue_instance_id, cls.shared_text_id
        )


class TraceKind(enum.Enum):
    # Do NOT reorder the enums. Depending on the type of database, existing
    # DBs may have these enums represented internally as ints based on the
    # order shown here, and changing it here messes up existing data. This
    # also means that new enums should be added AT THE END of the list.
    precondition = enum.auto()
    postcondition = enum.auto()

    @classproperty
    def PRECONDITION(cls):  # noqa
        return cls.precondition

    @classproperty
    def POSTCONDITION(cls):  # noqa
        return cls.postcondition

    @classmethod
    def create_from_string(cls, value: str) -> TraceKind:  # noqa
        if value == "precondition":
            return cls.precondition
        if value == "postcondition":
            return cls.postcondition
        raise ValueError(f"`{value}` is not a valid `TraceKind`")


class IssueInstance(Base, PrepareMixin, MutableRecordMixin):  # noqa
    """A particularly instance of an issue found in a run"""

    __tablename__ = "issue_instances"

    # pyre-fixme[8]: Attribute has type `DBID`; used as `Column[typing.Any]`.
    id: DBID = Column(BIGDBIDType, primary_key=True)

    location = Column(
        SourceLocationType,
        nullable=False,
        doc="Location (possibly a range) of the issue",
    )

    filename_id = Column(BIGDBIDType, nullable=False, server_default="0", default=0)

    filename = relationship(
        "SharedText",
        primaryjoin="foreign(SharedText.id) == IssueInstance.filename_id",
        uselist=False,
    )

    callable_id = Column(BIGDBIDType, nullable=False, server_default="0", default=0)

    callable = relationship(
        "SharedText",
        primaryjoin="foreign(SharedText.id) == IssueInstance.callable_id",
        uselist=False,
    )

    is_new_issue: Column[Optional[bool]] = Column(
        Boolean,
        index=True,
        default=False,
        doc="True if the issue did not exist before this instance",
    )

    run_id = Column(BIGDBIDType, nullable=False, index=True)

    issue_id = Column(BIGDBIDType, nullable=False, index=True)

    issue = relationship(
        "Issue",
        primaryjoin="foreign(Issue.id) == IssueInstance.issue_id",
        uselist=False,
    )

    fix_info_id = Column(BIGDBIDType, nullable=True)

    fix_info = relationship(
        "IssueInstanceFixInfo",
        primaryjoin=(
            "foreign(IssueInstanceFixInfo.id) == " "IssueInstance.fix_info_id"
        ),
        uselist=False,
    )

    message_id = Column(BIGDBIDType, nullable=True)

    message = relationship(
        "SharedText",
        primaryjoin="foreign(SharedText.id) == IssueInstance.message_id",
        uselist=False,
    )

    trace_frames = association_proxy("issue_instance_trace_frame", "trace_frame")

    issue_instance_trace_frame = relationship(
        "IssueInstanceTraceFrameAssoc",
        primaryjoin=(
            "IssueInstance.id == "
            "foreign(IssueInstanceTraceFrameAssoc.issue_instance_id)"
        ),
    )

    shared_texts = association_proxy("issue_instance_shared_text", "shared_text")

    issue_instance_shared_text = relationship(
        "IssueInstanceSharedTextAssoc",
        primaryjoin=(
            "IssueInstance.id == "
            "foreign(IssueInstanceSharedTextAssoc.issue_instance_id)"
        ),
    )

    min_trace_length_to_sources: Column[Optional[int]] = Column(
        Integer, nullable=True, doc="The minimum trace length to sources"
    )

    min_trace_length_to_sinks: Column[Optional[int]] = Column(
        Integer, nullable=True, doc="The minimum trace length to sinks"
    )

    rank: Column[Optional[int]] = Column(
        Integer,
        server_default="0",
        doc="The higher the rank, the higher the priority for this issue",
    )

    callable_count: Column[Optional[int]] = Column(
        Integer,
        server_default="0",
        doc="Number of issues in this callable for this run",
    )

    min_trace_length_to_entrypoints: Column[Optional[int]] = Column(
        Integer, nullable=True, doc="The minimum trace length to entrypoints"
    )

    def get_shared_texts_by_kind(self, kind: SharedTextKind) -> List[SharedText]:
        return [text for text in self.shared_texts if text.kind == kind]

    def get_trace_frames_by_kind(self, kind: TraceKind):
        return [frame for frame in self.trace_frames if frame.kind == kind]

    @classmethod
    def merge(cls, session, items):
        for i in items:
            # If the issue is new, then the instance has to be new. But note
            # that we still may need RunDiffer, because issues that disappeared
            # for a while and then came back are also marked new.
            i.is_new_issue = i.issue_id.is_new
            yield i


class IssueStatus(enum.Enum):
    """Issues are born uncategorized. Humans can
    set it to FALSE_POSITIVE or VALID_BUG upon review."""

    # Do NOT reorder the enums. Depending on the type of database, existing
    # DBs may have these enums represented internally as ints based on the
    # order shown here, and changing it here messes up existing data. This
    # also means that new enums should be added AT THE END of the list.
    """An issue that hasn't been marked as a bug or FP"""
    uncategorized = enum.auto()

    """Not a security bug, but a bad practice. Still needs fixing."""
    bad_practice = enum.auto()

    """False positive from analysis"""
    false_positive = enum.auto()

    """Reviewed and seen to be a valid bug that needs fixing"""
    valid_bug = enum.auto()

    """I don't care about this particular issue,
    but still want to see issues of this kind."""
    do_not_care = enum.auto()

    @classproperty
    def UNCATEGORIZED(cls):  # noqa
        return cls.uncategorized

    @classproperty
    def BAD_PRACTICE(cls):  # noqa
        return cls.bad_practice

    @classproperty
    def FALSE_POSITIVE(cls):  # noqa
        return cls.false_positive

    @classproperty
    def VALID_BUG(cls):  # noqa
        return cls.valid_bug

    @classproperty
    def DO_NOT_CARE(cls):  # noqa
        return cls.do_not_care


class Issue(Base, PrepareMixin, MutableRecordMixin):  # noqa
    """An issue coming from the static analysis.

    An issue can persist across multiple runs, even if it moves around in the
    code.
    """

    __tablename__ = "issues"

    # pyre-fixme[8]: Attribute has type `IssueDBID`; used as `Column[typing.Any]`.
    id: IssueDBID = Column(IssueBIGDBIDType, primary_key=True, nullable=False)

    handle: Column[str] = Column(
        String(length=HANDLE_LENGTH),
        nullable=False,
        unique=True,
        doc="This handle should uniquely identify an issue across runs on "
        + "different code revisions",
    )

    code: Column[int] = Column(
        Integer, doc="Code identifiying the issue type", nullable=False, index=True
    )

    instances = relationship(
        "IssueInstance", primaryjoin="Issue.id == foreign(IssueInstance.issue_id)"
    )

    first_seen: Column[datetime] = Column(
        DateTime,
        doc="time of the first run that found this issue",
        nullable=False,
        index=True,
    )

    status: Column[str] = Column(
        Enum(IssueStatus),
        doc="Shows the issue status from the latest run",
        server_default="uncategorized",
        nullable=False,
        index=True,
    )

    task_number: Column[Optional[int]] = Column(
        Integer, doc="Task number (not fbid) that is tracking this issue"
    )

    triage_history_fbid: Column[Optional[int]] = Column(
        BIGINT(unsigned=True),
        nullable=True,
        doc="FBID for EntZoncolanIssueTriageHistory",
    )

    feedback_fbid: Column[Optional[int]] = Column(
        BIGINT(unsigned=True), nullable=True, doc="FBID for EntZoncolanFeedback"
    )

    json: Column[Optional[str]] = Column(
        types.TEXT, doc="Raw JSON of original issue", nullable=True
    )

    @classmethod
    def _take(cls, n, iterable):
        "Return first n items of the iterable as a list"
        return list(islice(iterable, n))

    @classmethod
    def merge(cls, session, issues):
        return cls._merge_by_key(session, issues, cls.handle)


class RunStatus(enum.Enum):
    # Do NOT reorder the enums. Depending on the type of database, existing
    # DBs may have these enums represented internally as ints based on the
    # order shown here, and changing it here messes up existing data. This
    # also means that new enums should be added AT THE END of the list.
    finished = enum.auto()
    incomplete = enum.auto()
    skipped = enum.auto()
    failed = enum.auto()

    @classproperty
    def FINISHED(cls):  # noqa
        return cls.finished

    @classproperty
    def INCOMPLETE(cls):  # noqa
        return cls.incomplete

    @classproperty
    def SKIPPED(cls):  # noqa
        return cls.skipped

    @classproperty
    def FAILED(cls):  # noqa
        return cls.failed


CURRENT_DB_VERSION = 1


class Run(Base):  # noqa
    """A particular run of the static analyzer.

    Each time output is parsed from the static analyzer we generate a new run. A
    run has multiple IssueInstances."""

    __tablename__ = "runs"

    id = Column(BIGDBIDType, primary_key=True)

    job_id: Column[Optional[str]] = Column(String(length=255), index=True)

    date: Column[datetime] = Column(
        DateTime, doc="The date/time the analysis was run", nullable=False
    )

    commit_hash: Column[Optional[str]] = Column(
        String(length=255),
        doc="The commit hash of the codebase",
        nullable=True,
        index=True,
    )

    revision_id: Column[Optional[int]] = Column(
        Integer, doc="Differential revision (DXXXXXX)", nullable=True, index=True
    )

    differential_id: Column[Optional[int]] = Column(
        Integer,
        doc="Differential diff (instance of revision)",
        nullable=True,
        index=True,
    )

    hh_version: Column[Optional[str]] = Column(
        String(length=255), doc="The output of hh_server --version"
    )

    branch: Column[Optional[str]] = Column(
        String(length=255),
        doc="Branch the commit is based on",
        nullable=True,
        index=True,
    )

    issue_instances = relationship(
        "IssueInstance",
        primaryjoin="Run.id == foreign(IssueInstance.run_id)",
        backref="run",
    )

    status: Column[str] = Column(
        Enum(RunStatus), server_default="finished", nullable=False, index=True
    )

    status_description: Column[Optional[str]] = Column(
        String(length=255), doc="The reason why a run didn't finish", nullable=True
    )

    kind: Column[Optional[str]] = Column(
        String(length=255),
        doc=(
            "Specify different kinds of runs, e.g. MASTER vs. TEST., GKFORXXX, etc. "
            "in the same DB"
        ),
        nullable=True,
        index=True,
    )

    repository: Column[Optional[str]] = Column(
        String(length=255),
        doc=("The repository that static analysis was run on."),
        nullable=True,
    )

    db_version: Column[int] = Column(
        Integer,
        doc="Tracks under which DB version this was written (for migrations)",
        nullable=False,
        default=CURRENT_DB_VERSION,
        server_default="0",
    )

    def get_summary(self, **kwargs) -> RunSummary:
        session = Session.object_session(self)

        return RunSummary(
            commit_hash=self.commit_hash,
            differential_id=self.differential_id,
            id=self.id.resolved(),
            job_id=self.job_id,
            num_new_issues=self._get_num_new_issue_instances(session),
            num_total_issues=self._get_num_total_issues(session),
            alarm_counts=self._get_alarm_counts(session),
        )

    def new_issue_instances(self):
        session = Session.object_session(self)
        return (
            session.query(IssueInstance)
            .filter(IssueInstance.run_id == self.id)
            .filter(IssueInstance.is_new_issue.is_(True))
            .all()
        )

    def _get_num_new_issue_instances(self, session):
        return (
            session.query(IssueInstance)
            .filter(IssueInstance.run_id == self.id)
            .filter(IssueInstance.is_new_issue.is_(True))
            .count()
        )

    def _get_num_total_issues(self, session):
        return (
            session.query(IssueInstance).filter(IssueInstance.run_id == self.id).count()
        )

    def _get_alarm_counts(self, session):
        return dict(
            session.query(Issue.code, func.count(Issue.code))
            .filter(IssueInstance.run_id == self.id)
            .outerjoin(IssueInstance.issue)
            .group_by(Issue.code)
            .all()
        )


class MetaRun(Base):  # noqa
    """An identifier that represents multiple runs which should be grouped semantically.

    Meta-runs and runs have a many-to-many relationship, and the purpose of a meta-run
    is to allow querying & displaying results for all related runs without having to
    browse each of them separately."""

    __tablename__ = "metaruns"

    id = Column(BIGDBIDType, primary_key=True, autoincrement=False)

    # This is the moral equivalent of job_id, but named in a more intuitive manner.
    # Allows determining the latest meta run for each custom run separately.
    custom_run_name: Column[Optional[str]] = Column(String(length=255), nullable=True)

    date: Column[datetime] = Column(
        DateTime, doc="The date/time the meta-run was generated", nullable=False
    )

    # We want to be able to filter meta-runs by completion. Towards that end, we plan on
    # using the information of number of total runs vs. the number of runs written in
    # the database.
    expected_run_count: Column[Optional[int]] = Column(Integer, nullable=True)

    kind: Column[Optional[str]] = Column(
        String(length=255),
        doc=(
            "Specify different kinds of runs, e.g. MASTER vs. TEST., GKFORXXX, etc. "
            "in the same DB"
        ),
        nullable=True,
        index=True,
    )

    db_version: Column[int] = Column(
        Integer,
        doc="Tracks under which DB version this was written (for migrations)",
        nullable=False,
        default=CURRENT_DB_VERSION,
    )


class RunSummary:
    def __init__(
        self,
        commit_hash,
        differential_id,
        id,
        job_id,
        num_new_issues,
        num_total_issues,
        num_missing_preconditions: int = -1,
        num_missing_postconditions: int = -1,
        alarm_counts=None,
    ) -> None:
        self.commit_hash = commit_hash
        self.differential_id = differential_id
        self.id = id
        self.job_id = job_id
        self.num_new_issues = num_new_issues
        self.num_total_issues = num_total_issues
        self.num_missing_preconditions = num_missing_preconditions
        self.num_missing_postconditions = num_missing_postconditions
        self.alarm_counts = alarm_counts or {}

    def todict(self) -> Dict[str, Any]:
        return self.__dict__

    @classmethod
    def fromdict(cls, d):
        return cls(**d)


class MetaRunToRunAssoc(Base, PrepareMixin, RecordMixin):  # noqa
    """The responsibility of filling out the meta-run to run assoc is on the child jobs
    of a larger run.
    """

    __tablename__ = "metarun_run_assoc"

    meta_run_id = Column(BIGDBIDType, nullable=False, primary_key=True)
    run_id = Column(BIGDBIDType, nullable=False, primary_key=True)
    meta_run = relationship(
        "MetaRun",
        primaryjoin=("MetaRunToRunAssoc.meta_run_id == foreign(MetaRun.id)"),
        uselist=False,
    )
    run = relationship(
        "Run",
        primaryjoin=("MetaRunToRunAssoc.run_id == foreign(Run.id)"),
        uselist=False,
    )

    @classmethod
    def merge(cls, session, items):
        return cls._merge_assocs(session, items, cls.meta_run_id, cls.run_id)


class TraceFrameLeafAssoc(Base, PrepareMixin, RecordMixin):  # noqa

    __tablename__ = "trace_frame_message_assoc"

    trace_frame_id = Column(BIGDBIDType, nullable=False, primary_key=True)

    leaf_id = Column("message_id", BIGDBIDType, nullable=False, primary_key=True)

    # The minimum trace length unfortunately can be off and actually lead to
    # loops. This is a known problem and any code generating traces should
    # additionally have cycle detection.
    trace_length: Column[Optional[int]] = Column(
        Integer, doc="minimum trace length to the given leaf", nullable=True
    )

    trace_frame = relationship(
        "TraceFrame",
        primaryjoin=("TraceFrameLeafAssoc.trace_frame_id == " "foreign(TraceFrame.id)"),
        uselist=False,
    )

    leaves = relationship(
        "SharedText",
        primaryjoin="TraceFrameLeafAssoc.leaf_id == foreign(SharedText.id)",
        uselist=False,
    )

    @classmethod
    def merge(cls, session, items):
        return cls._merge_assocs(session, items, cls.trace_frame_id, cls.leaf_id)


class IssueInstanceFixInfo(Base, PrepareMixin, RecordMixin):  # noqa
    __tablename__ = "issue_instance_fix_info"

    # pyre-fixme[8]: Attribute has type `DBID`; used as `Column[typing.Any]`.
    id: DBID = Column(BIGDBIDType, nullable=False, primary_key=True)

    fix_info: Column[str] = Column(
        String(length=INNODB_MAX_INDEX_LENGTH), nullable=False
    )

    issue_instance = relationship(
        "IssueInstance",
        primaryjoin=(
            "foreign(IssueInstance.fix_info_id) == " "IssueInstanceFixInfo.id"
        ),
        uselist=False,
    )


class TraceFrame(Base, PrepareMixin, RecordMixin):  # noqa

    __tablename__ = "trace_frames"

    __table_args__ = (
        Index("ix_traceframe_run_caller_port", "run_id", "caller_id", "caller_port"),
        Index("ix_traceframe_run_callee_port", "run_id", "callee_id", "callee_port"),
    )

    # pyre-fixme[8]: Attribute has type `DBID`; used as `Column[typing.Any]`.
    id: DBID = Column(BIGDBIDType, nullable=False, primary_key=True)

    kind: Column[str] = Column(Enum(TraceKind), nullable=False, index=False)

    caller_id = Column(BIGDBIDType, nullable=False, server_default="0", default=0)

    caller = relationship(
        "SharedText",
        primaryjoin="foreign(SharedText.id) == TraceFrame.caller_id",
        uselist=False,
    )

    # pyre-fixme[8]: Attribute has type `str`; used as `Column[str]`.
    caller_port: str = Column(
        String(length=INNODB_MAX_INDEX_LENGTH),
        nullable=False,
        server_default="",
        doc="The caller port of this call edge",
    )

    callee_id = Column(BIGDBIDType, nullable=False, server_default="0", default=0)

    callee = relationship(
        "SharedText",
        primaryjoin="foreign(SharedText.id) == TraceFrame.callee_id",
        uselist=False,
    )

    callee_location = Column(
        SourceLocationType,
        nullable=False,
        doc="The location of the callee in the source code (line|start|end)",
    )

    # pyre-fixme[8]: Attribute has type `str`; used as `Column[str]`.
    callee_port: str = Column(
        String(length=INNODB_MAX_INDEX_LENGTH),
        nullable=False,
        server_default="",
        doc="The callee port of this call edge'",
    )

    filename_id = Column(BIGDBIDType, nullable=False, server_default="0", default=0)

    run_id = Column("run_id", BIGDBIDType, nullable=False, index=False)

    type_interval_lower: Column[Optional[int]] = Column(
        Integer, nullable=True, doc="Class interval lower-bound (inclusive)"
    )

    type_interval_upper: Column[Optional[int]] = Column(
        Integer, nullable=True, doc="Class interval upper-bound (inclusive)"
    )

    migrated_id = Column(
        BIGDBIDType,
        nullable=True,
        doc=(
            "ID of the corresponding pre/postcondition. Temporary column used "
            "for migrating existing pre/postconditions into trace frames. "
            "Will be removed once migration is completed. Use None if not "
            "in data migration mode."
        ),
    )

    preserves_type_context: Column[bool] = Column(
        Boolean,
        default=False,
        server_default="0",
        nullable=False,
        doc="Whether the call preserves the calling type context",
    )

    titos = Column(
        SourceLocationsType,
        doc="Locations of TITOs aka abductions for the trace frame",
        nullable=False,
        server_default="",
    )

    annotations = relationship(
        "TraceFrameAnnotation",
        primaryjoin=(
            "TraceFrame.id == " "foreign(TraceFrameAnnotation.trace_frame_id)"
        ),
        uselist=True,
    )

    leaves = association_proxy("leaf_assoc", "leaves")

    leaf_assoc = relationship(
        "TraceFrameLeafAssoc",
        primaryjoin=("TraceFrame.id == " "foreign(TraceFrameLeafAssoc.trace_frame_id)"),
        uselist=True,
    )

    issue_instances = association_proxy("trace_frame_issue_instance", "issue_instance")

    trace_frame_issue_instance = relationship(
        "IssueInstanceTraceFrameAssoc",
        primaryjoin=(
            "TraceFrame.id == " "foreign(IssueInstanceTraceFrameAssoc.trace_frame_id)"
        ),
    )


# Extra bits of information we can show on a TraceFrame.
# This may be a message description, or it may be the start of another series
# of traces leading to some other leaf. TraceFrameAnnotationTraceFrameAssoc
# contains the first hop towards that leaf..
class TraceFrameAnnotation(Base, PrepareMixin, RecordMixin):  # noqa

    __tablename__ = "trace_frame_annotations"

    # pyre-fixme[8]: Attribute has type `DBID`; used as `Column[typing.Any]`.
    id: DBID = Column(BIGDBIDType, nullable=False, primary_key=True)

    location = Column(
        SourceLocationType, nullable=False, doc="The location for the message"
    )

    kind: Column[Optional[str]] = Column(String(length=255), nullable=True, index=True)

    # pyre-fixme[8]: Attribute has type `str`; used as `Column[str]`.
    message: str = Column(
        String(length=4096),
        doc="Message describing info about the trace",
        nullable=False,
    )

    leaf_id = Column(BIGDBIDType, nullable=True)
    leaf = relationship(
        "SharedText",
        primaryjoin="foreign(SharedText.id) == TraceFrameAnnotation.leaf_id",
        uselist=False,
    )

    # pyre-fixme[8]: Attribute has type `Optional[str]`; used as `Column[str]`.
    link: Optional[str] = Column(
        String(length=4096),
        doc="An optional URL linking the message to more info (Quandary)",
        nullable=True,
    )

    # pyre-fixme[8]: Attribute has type `Optional[str]`; used as `Column[str]`.
    trace_key: Optional[str] = Column(
        String(length=INNODB_MAX_INDEX_LENGTH),
        nullable=True,
        doc="Link to possible pre/post traces (caller_condition).",
    )

    # pyre-fixme[8]: Attribute has type `DBID`; used as `Column[typing.Any]`.
    trace_frame_id: DBID = Column(BIGDBIDType, nullable=False, index=True)
    trace_frame = relationship(
        "TraceFrame",
        primaryjoin=(
            "TraceFrame.id == " "foreign(TraceFrameAnnotation.trace_frame_id)"
        ),
        uselist=True,
    )

    child_trace_frames = association_proxy(
        "trace_frame_annotation_trace_frame", "trace_frame"
    )
    trace_frame_annotation_trace_frame = relationship(
        "TraceFrameAnnotationTraceFrameAssoc",
        primaryjoin=(
            "TraceFrameAnnotation.id == "
            "foreign(TraceFrameAnnotationTraceFrameAssoc.trace_frame_annotation_id)"
        ),
    )


# A TraceFrameAnnotation may indicate more traces branching out from a trace
# frame towards a different leaf/trace kind. In that case, this assoc describes
# the first hop trace frame from the annotation. It is similar to
# IssueInstanceTraceFrameAssoc, which indicates the first hop trace frame from
# the issue instance.
class TraceFrameAnnotationTraceFrameAssoc(Base, PrepareMixin, RecordMixin):  # noqa

    __tablename__ = "trace_frame_annotation_trace_frame_assoc"

    trace_frame_annotation_id = Column(
        "trace_frame_annotation_id", BIGDBIDType, primary_key=True, nullable=False
    )

    trace_frame_id = Column(
        "trace_frame_id", BIGDBIDType, primary_key=True, nullable=False, index=True
    )

    trace_frame_annotation = relationship(
        "TraceFrameAnnotation",
        primaryjoin=(
            "TraceFrameAnnotationTraceFrameAssoc.trace_frame_annotation_id == "
            "foreign(TraceFrameAnnotation.id)"
        ),
        uselist=False,
    )

    trace_frame = relationship(
        "TraceFrame",
        primaryjoin=(
            "TraceFrameAnnotationTraceFrameAssoc.trace_frame_id == "
            "foreign(TraceFrame.id)"
        ),
        uselist=False,
    )

    @classmethod
    def merge(cls, session, items):
        return cls._merge_assocs(
            session, items, cls.trace_frame_annotation_id, cls.trace_frame_id
        )


class WarningMessage(Base):  # noqa
    __tablename__ = "warning_messages"

    code: Column[int] = Column(Integer, autoincrement=False, primary_key=True)

    message: Column[str] = Column(String(length=4096), nullable=False)


class WarningCodeCategory(enum.Enum):
    # Do NOT reorder the enums. Depending on the type of database, existing
    # DBs may have these enums represented internally as ints based on the
    # order shown here, and changing it here messes up existing data. This
    # also means that new enums should be added AT THE END of the list.
    bug = enum.auto()
    code_smell = enum.auto()

    @classproperty
    def BUG(cls):  # noqa
        return cls.bug

    @classproperty
    def CODE_SMELL(cls):  # noqa
        return cls.code_smell


class WarningCodeProperties(Base):  # noqa
    """Contains properties describing each warning code"""

    __tablename__ = "warning_code_properties"

    code: Column[int] = Column(
        Integer,
        autoincrement=False,
        nullable=False,
        primary_key=True,
        doc="Code identifiying the issue type",
    )

    category: Column[Optional[str]] = Column(
        Enum(WarningCodeCategory),
        nullable=True,
        index=False,
        # pyre-fixme[6]: Expected `str` for 4th param but got `Tuple[str]`.
        doc=(
            "The category of problems that issues in with this warning code "
            "can result in ",
        ),
    )

    new_issue_rate: Column[Optional[Decimal]] = Column(
        Float,
        nullable=True,
        index=False,
        doc="Average number of new issues per day (computed column)",
    )

    bug_count: Column[Optional[int]] = Column(
        Integer,
        nullable=True,
        index=False,
        doc="Number of issues in this category (computed column)",
    )

    avg_trace_len: Column[Optional[Decimal]] = Column(
        Float, nullable=True, index=False, doc="Deprecated. See avg_fwd/bwd_trace_len"
    )

    avg_fwd_trace_len: Column[Optional[Decimal]] = Column(
        Float,
        nullable=True,
        index=False,
        # pyre-fixme[6]: Expected `str` for 4th param but got `Tuple[str]`.
        doc=(
            "Average (min) length of forward traces for the given warning code "
            "(computed column)",
        ),
    )

    avg_bwd_trace_len: Column[Optional[Decimal]] = Column(
        Float,
        nullable=True,
        index=False,
        # pyre-fixme[6]: Expected `str` for 4th param but got `Tuple[str]`.
        doc=(
            "Average (min) length of backward traces for the given warning "
            "code (computed column)",
        ),
    )

    snr: Column[Optional[Decimal]] = Column(
        Float,
        nullable=True,
        index=False,
        doc=(
            "Signal to noise ratio based on triaged issues (computed column). "
            "Ratio of (valid + bad practice) to (false positive + don't care)"
        ),
    )

    is_snr_significant: Column[Optional[bool]] = Column(
        Boolean,
        nullable=True,
        index=False,
        doc=(
            "True if we are confident about the snr (computed column). "
            "Depends on percentage of triaged issues and number of issues."
        ),
    )

    discoverable: Column[Optional[bool]] = Column(
        Boolean,
        nullable=True,
        index=False,
        doc="True if an attacker can discover the issue",
    )

    health_score: Column[Optional[Decimal]] = Column(
        Float,
        nullable=True,
        index=False,
        doc=(
            "Scoring for the health of the warning code, between 0 and 1, "
            "based on the values in the other columns (computed column)"
        ),
    )

    notes: Column[Optional[str]] = Column(
        String(length=4096),
        nullable=True,
        index=False,
        doc="Free form field for note-taking",
    )


class PrimaryKey(Base, PrimaryKeyBase):
    pass


class PrimaryKeyGenerator(PrimaryKeyGeneratorBase):

    PRIMARY_KEY: Type = PrimaryKey

    QUERY_CLASSES: Set[Type] = {
        Issue,
        IssueInstance,
        IssueInstanceFixInfo,
        SharedText,
        Run,
        TraceFrame,
        TraceFrameAnnotation,
    }


def create(db: DB) -> None:
    try:
        Base.metadata.create_all(db.engine)
    except NoSuchTableError:
        pass


convert_sqlalchemy_type.register(SourceLocationType)(convert_column_to_string)
convert_sqlalchemy_type.register(BIGDBIDType)(convert_column_to_int_or_id)
