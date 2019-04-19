#!/usr/bin/env python3

import enum
import logging
from collections import namedtuple
from itertools import islice, tee
from typing import Any, Dict, List, Optional, Set, Tuple, Type

from munch import Munch
from sqlalchemy import (
    Boolean,
    Column,
    DateTime,
    Enum,
    Float,
    Index,
    Integer,
    MetaData,
    String,
    Table,
    and_,
    exc,
    func,
    inspect,
    or_,
    types,
)
from sqlalchemy.dialects import mysql, sqlite
from sqlalchemy.dialects.mysql import BIGINT, INTEGER
from sqlalchemy.ext.associationproxy import association_proxy
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import Session, relationship

from .errors import AIException
from .iterutil import split_every


log = logging.getLogger("sapp")


# For use on enums to alias upper case value.
#
# FLAKE8 does not understand that this is a static property
# flake8: noqa B902
class classproperty(property):
    def __get__(self, cls, owner):
        return classmethod(self.fget).__get__(None, owner)()


Base = declarative_base()
INNODB_MAX_INDEX_LENGTH = 767
HANDLE_LENGTH = 255
MESSAGE_LENGTH = 4096
SHARED_TEXT_LENGTH = 4096

"""Number of variables that can safely be set on a single DB call"""
BATCH_SIZE = 450

"""Models used to represent DB entries

An Issue is a particular problem found. It can exist across multiple commits.  A
Run is a single run of Zoncolan over a specific commit. It may find new Issues,
or existing Issues.  Each run is tied to Issues through IssueInstances.
IssueInstances have per run information, like source location, while Issues have
attributes like the status of an issue.
"""

"""Tables that should be removed from existing databases.

These are tables that are NO LONGER in use, and will be removed. Don't add
tables here until all code that was using them has been updated.
"""
PurgeMetadata = MetaData()
for t in [
    "active_jobs",
    "deletion_history",
    "herald_task_history",
    "precondition_incremental_lookup",
    "run_precondition_assoc",
    "run_postcondition_assoc",
]:
    Table(t, PurgeMetadata)


class PrepareMixin(object):
    @classmethod
    def prepare(cls, session, pkgen, items):
        """This is called immediately before the items are written to the
        database. pkgen is passed in to allow last-minute resolving of ids.
        """
        for item in cls.merge(session, items):
            if hasattr(item, "id"):
                item.id.resolve(id=pkgen.get(cls), is_new=True)
            yield cls.to_dict(item)

    @classmethod
    def merge(cls, session, items):
        """Models should override this to perform a merge"""
        return items

    @classmethod
    def _merge_by_key(cls, session, items, attr):
        return cls._merge_by_keys(
            session, items, lambda item: getattr(item, attr.key), attr
        )

    @classmethod
    def _merge_by_keys(cls, session, items, hash_item, *attrs):
        """An object can have multiple attributes as its key. This merges the
        items to be added with existing items in the database based on their
        key(s).

        session: Session object for querying the DB.
        items: Iterator of items to be added to the DB.
        hash_item: Function that takes as in put the item to be added and
                   returns a hash of it.
        attrs: List of attributes of the object/class that represent the
               object's key.

        Returns the next item (in items) that is not already in the DB.
        """
        # Note: items is an iterator, not an iterable, 'tee' is a must.
        items_iter1, items_iter2 = tee(items)

        keys = {}  # map of hash -> keys of the item
        for i in items_iter1:
            # An item's key is a map of 'attr -> item[attr]' where attr is
            # usually a column name.
            # For 'SharedText', its key would look like: {
            #   "kind": "feature",
            #   "contents": "via tito",
            # }
            item_hash = hash_item(i)
            keys[item_hash] = {attr.key: getattr(i, attr.key) for attr in attrs}

        # Find existing items.
        existing_ids = {}  # map of item_hash -> existing ID
        cls_attrs = [getattr(cls, attr.key) for attr in attrs]
        for fetch_keys in split_every(BATCH_SIZE, keys.values()):
            filters = []
            for fetch_key in fetch_keys:
                # Sub-filters for checking if item with fetch_key is in the DB
                # Example: [
                #   SharedText.kind.__eq__("feature"),
                #   SharedText.contents.__eq__("via tito"),
                # ]
                subfilter = [
                    getattr(cls, attr).__eq__(val) for attr, val in fetch_key.items()
                ]
                filters.append(and_(*subfilter))
            existing_items = (
                session.query(cls.id, *cls_attrs).filter(or_(*(filters))).all()
            )
            for existing_item in existing_items:
                item_hash = hash_item(existing_item)
                existing_ids[item_hash] = existing_item.id

        # Now see if we can merge
        new_items = {}
        for i in items_iter2:
            item_hash = hash_item(i)
            if item_hash in existing_ids:
                # The key is already in the DB
                i.id.resolve(existing_ids[item_hash], is_new=False)
            elif item_hash in new_items:
                # The key is already in the list of new items
                i.id.resolve(new_items[item_hash].id, is_new=False)
            else:
                # The key is new
                new_items[item_hash] = i
                yield i

    @classmethod
    def _merge_assocs(cls, session, items, id1, id2):
        new_items = {}
        for i in items:
            r1 = getattr(i, id1.key)
            r2 = getattr(i, id2.key)
            key = (r1.resolved(), r2.resolved())
            if key not in new_items:
                new_items[key] = i
                yield i


# The record mixin class is more efficient than the MutableRecordMixin, so it
# should be preferred. But the performance isn't from the mutability, it's
# because we use namedtuples, which creates a new class on demand, which uses
# __slots__, which is more efficient. Both of these mixins can be replaced when
# we have dynamically created classes with the slots set. But until then,
# prefer RecordMixin unless you need to change fields after creation.
class RecordMixin(object):
    _record = None

    @classmethod
    def Record(cls, extra_fields=None, **kwargs):
        if not cls._record:
            if not extra_fields:
                extra_fields = []
            mapper = inspect(cls)
            keys = [c.key for c in mapper.column_attrs] + ["model"] + extra_fields
            cls._record = namedtuple(cls.__name__ + "Record", keys)

        return cls._record(model=cls, **kwargs)

    @classmethod
    def to_dict(cls, obj):
        return obj._asdict()


class MutableRecordMixin(object):
    @classmethod
    def Record(cls, **kwargs):
        return Munch(model=cls, **kwargs)

    @classmethod
    def to_dict(cls, obj):
        return obj.toDict()


class SourceLocation(object):
    """The location in a source file that an error occurred in

    If end_column is defined then we have a range, otherwise it defaults to
    begin_column and we have a single point.
    """

    def __init__(self, line_no, begin_column, end_column=None):
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
    def from_string(location_string):
        location_points = location_string.split("|")
        assert len(location_points) == 3, "Invalid location string %s" % location_string
        return SourceLocation(*location_points)

    @staticmethod
    def to_string(location):
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

    def __init__(self):
        super(SourceLocationType, self).__init__(length=255)

    def process_bind_param(self, value, dialect):
        """
        SQLAlchemy uses this to convert a SourceLocation object into a string.
        """
        if value is None:
            return None
        return SourceLocation.to_string(value)

    def process_result_value(self, value, dialect):
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

    def __init__(self):
        super(SourceLocationsType, self).__init__(length=4096)

    def process_bind_param(self, value, dialect):
        if value is None:
            return None
        return ",".join([SourceLocation.to_string(l) for l in value])

    def process_result_value(self, value, dialect):
        if value is None or value == "":
            return []
        assert isinstance(value, str), "Invalid SourceLocationsType %s" % str(value)
        locations = value.split(",")
        return [SourceLocation.from_string(location) for location in locations]


# The following three DBID classes require some explanation. Normally models
# will reference each other by their id. But we do bulk insertion at the end
# of our processing, which means the id isn't set until later. Having a DBID
# object allows these models to reference each other before that point. When
# we are ready to insert into the database, PrimaryKeyGenerator will give it
# an ID. Any other models referencing that DBID object will now be able to use
# the real id.


class DBID(object):
    __slots__ = ["_id", "is_new", "local_id"]

    # Temporary IDs that are local per run (local_id) are assigned for each
    # DBID object on creation. This acts as a key for the object in map-like
    # structures of DB objects without having to define a hashing function for
    # each of them. next_id tracks the next available int to act as an id.
    next_id: int = 0

    def __init__(self, id=None):
        self.resolve(id)
        self.local_id: int = DBID.next_id
        DBID.next_id += 1

    def resolve(self, id, is_new=True):
        self._check_type(id)
        self._id = id
        self.is_new = is_new
        return self

    def resolved(self):
        id = self._id

        # We allow one level of a DBID pointing to another DBID
        if isinstance(id, DBID):
            id = id.resolved()

        return id

    def _check_type(self, id):
        if not isinstance(id, (int, type(None), DBID)):
            raise TypeError(
                "id expected to be type '{}' but was type '{}'".format(int, type(id))
            )

    # Allow DBIDs to be added and compared as ints
    def __int__(self):
        return self.resolved()

    def __str__(self):
        return str(self.resolved())

    def __add__(self, other):
        return int(self) + int(other)

    def __lt__(self, other):
        return int(self) < int(other)

    def __gt__(self, other):
        return int(self) > int(other)

    def __ge__(self, other):
        return int(self) >= int(other)

    def __le__(self, other):
        return int(self) <= int(other)

    def __repr__(self):
        return "<{}(id={}) object at 0x{:x}>".format(
            self.__class__.__name__, self._id, id(self)
        )


class DBIDType(types.TypeDecorator):
    impl = types.Integer

    def process_bind_param(self, value, dialect):
        # If it is a DBID wrapper, then write the contained value. Otherwise it
        # may be resolved already, or None.
        if isinstance(value, DBID):
            return value.resolved()
        else:
            return value

    def process_result_value(self, value, dialect):
        return DBID(value)

    def load_dialect_impl(self, dialect):
        if dialect.name == "mysql":
            return dialect.type_descriptor(mysql.INTEGER(unsigned=True))
        return self.impl


class BIGDBIDType(DBIDType):
    impl = types.BigInteger

    def load_dialect_impl(self, dialect):
        if dialect.name == "mysql":
            return dialect.type_descriptor(mysql.BIGINT(unsigned=True))
        return self.impl


# See Issue.merge for information about replace_assocs


class IssueDBID(DBID):
    __slots__ = ["replace_assocs"]

    def __init__(self, id=None):
        super().__init__(id)
        self.replace_assocs = False


class IssueDBIDType(DBIDType):
    def process_result_value(self, value, dialect):
        return IssueDBID(value)


class IssueBIGDBIDType(BIGDBIDType):
    def process_result_value(self, value, dialect):
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
    feature = enum.auto()
    message = enum.auto()
    source = enum.auto()
    sink = enum.auto()
    callable = enum.auto()
    filename = enum.auto()

    @classproperty
    def FEATURE(cls):
        return cls.feature

    @classproperty
    def MESSAGE(cls):
        return cls.message

    @classproperty
    def SOURCE(cls):
        return cls.source

    @classproperty
    def SINK(cls):
        return cls.sink

    @classproperty
    def CALLABLE(cls):
        return cls.callable

    @classproperty
    def FILENAME(cls):
        return cls.filename


class SharedText(Base, PrepareMixin, RecordMixin):  # noqa
    """Any string-ish type that can be shared as a property of some other
    object. (e.g. features, sources, sinks). The table name 'messages' is due
    to legacy reasons."""

    __tablename__ = "messages"

    __table_args__ = (Index("ix_messages_handle", "contents", "kind"),)

    id: DBID = Column(BIGDBIDType, primary_key=True)

    contents: str = Column(
        String(length=SHARED_TEXT_LENGTH), nullable=False, index=True
    )

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
    precondition = enum.auto()
    postcondition = enum.auto()

    @classproperty
    def PRECONDITION(cls):
        return cls.precondition

    @classproperty
    def POSTCONDITION(cls):
        return cls.postcondition


class IssueInstance(Base, PrepareMixin, MutableRecordMixin):  # noqa
    """A particularly instance of an issue found in a run"""

    __tablename__ = "issue_instances"

    id: DBID = Column(BIGDBIDType, primary_key=True)

    location = Column(
        SourceLocationType,
        nullable=False,
        doc="Location (possibly a range) of the issue",
    )

    filename = Column(
        String(length=767),
        doc="Filename containing the issue",
        nullable=True,
        index=True,
    )

    filename_id = Column(BIGDBIDType, nullable=False, server_default="0", default=0)

    callable_id = Column(BIGDBIDType, nullable=False, server_default="0", default=0)

    is_new_issue = Column(
        Boolean,
        index=True,
        default=False,
        doc="True if the issue did not exist before this instance",
    )

    run_id = Column(BIGDBIDType, nullable=False, index=True)

    issue_id = Column(BIGDBIDType, nullable=False, index=True)

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

    min_trace_length_to_sources = Column(
        Integer, nullable=True, doc="The minimum trace length to sources"
    )

    min_trace_length_to_sinks = Column(
        Integer, nullable=True, doc="The minimum trace length to sinks"
    )

    rank = Column(
        Integer,
        server_default="0",
        doc="The higher the rank, the higher the priority for this issue",
    )

    callable_count = Column(
        Integer,
        server_default="0",
        doc="Number of issues in this callable for this run",
    )

    def get_shared_texts_by_kind(self, kind: SharedTextKind):
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
    def UNCATEGORIZED(cls):
        return cls.uncategorized

    @classproperty
    def BAD_PRACTICE(cls):
        return cls.bad_practice

    @classproperty
    def FALSE_POSITIVE(cls):
        return cls.false_positive

    @classproperty
    def VALID_BUG(cls):
        return cls.valid_bug

    @classproperty
    def DO_NOT_CARE(cls):
        return cls.do_not_care


class Issue(Base, PrepareMixin, MutableRecordMixin):  # noqa
    """An issue coming from the static analysis.

    An issue can persist across multiple runs, even if it moves around in the
    code.
    """

    __tablename__ = "issues"

    id: IssueDBID = Column(IssueBIGDBIDType, primary_key=True, nullable=False)

    handle = Column(
        String(length=HANDLE_LENGTH),
        nullable=False,
        unique=True,
        doc="This handle should uniquely identify an issue across runs on "
        + "different code revisions",
    )

    code = Column(
        Integer, doc="Code identifiying the issue type", nullable=False, index=True
    )

    callable = Column(
        String(length=INNODB_MAX_INDEX_LENGTH),
        doc="Callable containing the issue",
        nullable=False,
        index=True,
    )

    instances = relationship(
        "IssueInstance",
        primaryjoin="Issue.id == foreign(IssueInstance.issue_id)",
        backref="issue",
    )

    first_seen = Column(
        DateTime,
        doc="time of the first run that found this issue",
        nullable=False,
        index=True,
    )

    status = Column(
        Enum(IssueStatus),
        doc="Shows the issue status from the latest run",
        server_default="uncategorized",
        nullable=False,
        index=True,
    )

    task_number = Column(
        Integer, doc="Task number (not fbid) that is tracking this issue"
    )

    triage_history_fbid = Column(
        BIGINT(unsigned=True),
        nullable=True,
        doc="FBID for EntZoncolanIssueTriageHistory",
    )

    feedback_fbid = Column(
        BIGINT(unsigned=True), nullable=True, doc="FBID for EntZoncolanFeedback"
    )

    json = Column(types.TEXT, doc="Raw JSON of original issue", nullable=True)

    @classmethod
    def _take(cls, n, iterable):
        "Return first n items of the iterable as a list"
        return list(islice(iterable, n))

    @classmethod
    def merge(cls, session, issues):
        return cls._merge_by_key(session, issues, cls.handle)


class RunStatus(enum.Enum):
    finished = enum.auto()
    incomplete = enum.auto()
    skipped = enum.auto()
    failed = enum.auto()

    @classproperty
    def FINISHED(cls):
        return cls.finished

    @classproperty
    def INCOMPLETE(cls):
        return cls.incomplete

    @classproperty
    def SKIPPED(cls):
        return cls.skipped

    @classproperty
    def FAILED(cls):
        return cls.failed


CURRENT_DB_VERSION = 1


class Run(Base):  # noqa
    """A particular run of the static analyzer.

    Each time output is parsed from the static analyzer we generate a new run. A
    run has multiple IssueInstances."""

    __tablename__ = "runs"

    id = Column(BIGDBIDType, primary_key=True)

    job_id = Column(String(length=255), unique=True)

    date = Column(DateTime, doc="The date/time the analysis was run", nullable=False)

    commit_hash = Column(
        String(length=255),
        doc="The commit hash of the codebase",
        nullable=True,
        index=True,
    )

    revision_id = Column(
        Integer, doc="Differential revision (DXXXXXX)", nullable=True, index=True
    )

    differential_id = Column(
        Integer,
        doc="Differential diff (instance of revision)",
        nullable=True,
        index=True,
    )

    hh_version = Column(String(length=255), doc="The output of hh_server --version")

    branch = Column(
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

    status = Column(
        Enum(RunStatus), server_default="finished", nullable=False, index=True
    )

    status_description = Column(
        String(length=255), doc="The reason why a run didn't finish", nullable=True
    )

    kind = Column(
        String(length=255),
        doc=(
            "Specify different kinds of runs, e.g. MASTER vs. TEST., GKFORXXX, etc. "
            "in the same DB"
        ),
        nullable=True,
        index=True,
    )

    repository = Column(
        String(length=255),
        doc=("The repository that static analysis was run on."),
        nullable=True,
    )

    db_version = Column(
        Integer,
        doc="Tracks under which DB version this was written (for migrations)",
        nullable=False,
        default=CURRENT_DB_VERSION,
        server_default="0",
    )

    def get_summary(self, **kwargs):
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


class RunSummary:
    def __init__(
        self,
        commit_hash,
        differential_id,
        id,
        job_id,
        num_new_issues,
        num_total_issues,
        num_missing_preconditions=-1,
        num_missing_postconditions=-1,
        alarm_counts=None,
    ):
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


class TraceFrameLeafAssoc(Base, PrepareMixin, RecordMixin):  # noqa

    __tablename__ = "trace_frame_message_assoc"

    trace_frame_id = Column(BIGDBIDType, nullable=False, primary_key=True)

    leaf_id = Column("message_id", BIGDBIDType, nullable=False, primary_key=True)

    # The minimum trace length unfortunately can be off and actually lead to
    # loops. This is a known problem and any code generating traces should
    # additionally have cycle detection.
    trace_length = Column(
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

    id: DBID = Column(BIGDBIDType, nullable=False, primary_key=True)

    fix_info = Column(String(length=INNODB_MAX_INDEX_LENGTH), nullable=False)

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
    )

    id: DBID = Column(BIGDBIDType, nullable=False, primary_key=True)

    kind = Column(Enum(TraceKind), nullable=False, index=False)

    caller: str = Column(
        String(length=INNODB_MAX_INDEX_LENGTH),
        nullable=False,
        doc="The function/method that produces the tainted trace",
    )

    caller_id = Column(BIGDBIDType, nullable=False, server_default="0", default=0)

    caller_port: str = Column(
        String(length=INNODB_MAX_INDEX_LENGTH),
        nullable=False,
        server_default="",
        doc="The caller port of this call edge",
    )

    callee: str = Column(
        String(length=INNODB_MAX_INDEX_LENGTH),
        nullable=False,
        doc="The function/method within the caller that produces the tainted trace.",
    )

    callee_id = Column(BIGDBIDType, nullable=False, server_default="0", default=0)

    callee_location = Column(
        SourceLocationType,
        nullable=False,
        doc="The location of the callee in the source code (line|start|end)",
    )

    callee_port: str = Column(
        String(length=INNODB_MAX_INDEX_LENGTH),
        nullable=False,
        server_default="",
        doc="The callee port of this call edge'",
    )

    filename = Column(
        String(length=4096), doc="Filename containing the call", nullable=False
    )

    filename_id = Column(BIGDBIDType, nullable=False, server_default="0", default=0)

    run_id = Column("run_id", BIGDBIDType, nullable=False, index=False)

    type_interval_lower = Column(
        Integer, nullable=True, doc="Class interval lower-bound (inclusive)"
    )

    type_interval_upper = Column(
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

    preserves_type_context = Column(
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
class TraceFrameAnnotation(Base, PrepareMixin, RecordMixin):  # noqa

    __tablename__ = "trace_frame_annotations"

    id: DBID = Column(BIGDBIDType, nullable=False, primary_key=True)

    location = Column(
        SourceLocationType, nullable=False, doc="The location for the message"
    )

    message: str = Column(
        String(length=4096),
        doc="Message describing info about the trace",
        nullable=False,
    )

    link: Optional[str] = Column(
        String(length=4096),
        doc="An optional URL linking the message to more info (Quandary)",
        nullable=True,
    )

    trace_key: Optional[str] = Column(
        String(length=INNODB_MAX_INDEX_LENGTH),
        nullable=True,
        doc="Link to possible pre/post traces (caller_condition).",
    )

    trace_frame_id: DBID = Column(BIGDBIDType, nullable=False, index=True)
    trace_frame = relationship(
        "TraceFrame",
        primaryjoin=(
            "TraceFrame.id == " "foreign(TraceFrameAnnotation.trace_frame_id)"
        ),
        uselist=True,
    )


class WarningMessage(Base):  # noqa
    __tablename__ = "warning_messages"

    code = Column(Integer, autoincrement=False, primary_key=True)

    message = Column(String(length=4096), nullable=False)


class WarningCodeCategory(enum.Enum):
    bug = enum.auto()
    code_smell = enum.auto()

    @classproperty
    def BUG(cls):
        return cls.bug

    @classproperty
    def CODE_SMELL(cls):
        return cls.code_smell


class WarningCodeProperties(Base):  # noqa
    """Contains properties describing each warning code"""

    __tablename__ = "warning_code_properties"

    code = Column(
        Integer,
        autoincrement=False,
        nullable=False,
        primary_key=True,
        doc="Code identifiying the issue type",
    )

    category = Column(
        Enum(WarningCodeCategory),
        nullable=True,
        index=False,
        doc=(
            "The category of problems that issues in with this warning code "
            "can result in ",
        ),
    )

    new_issue_rate = Column(
        Float,
        nullable=True,
        index=False,
        doc="Average number of new issues per day (computed column)",
    )

    bug_count = Column(
        Integer,
        nullable=True,
        index=False,
        doc="Number of issues in this category (computed column)",
    )

    avg_trace_len = Column(
        Float, nullable=True, index=False, doc="Deprecated. See avg_fwd/bwd_trace_len"
    )

    avg_fwd_trace_len = Column(
        Float,
        nullable=True,
        index=False,
        doc=(
            "Average (min) length of forward traces for the given warning code "
            "(computed column)",
        ),
    )

    avg_bwd_trace_len = Column(
        Float,
        nullable=True,
        index=False,
        doc=(
            "Average (min) length of backward traces for the given warning "
            "code (computed column)",
        ),
    )

    snr = Column(
        Float,
        nullable=True,
        index=False,
        doc=(
            "Signal to noise ratio based on triaged issues (computed column). "
            "Ratio of (valid + bad practice) to (false positive + don't care)"
        ),
    )

    is_snr_significant = Column(
        Boolean,
        nullable=True,
        index=False,
        doc=(
            "True if we are confident about the snr (computed column). "
            "Depends on percentage of triaged issues and number of issues."
        ),
    )

    discoverable = Column(
        Boolean,
        nullable=True,
        index=False,
        doc="True if an attacker can discover the issue",
    )

    health_score = Column(
        Float,
        nullable=True,
        index=False,
        doc=(
            "Scoring for the health of the warning code, between 0 and 1, "
            "based on the values in the other columns (computed column)"
        ),
    )

    notes = Column(
        String(length=4096),
        nullable=True,
        index=False,
        doc="Free form field for note-taking",
    )


class PrimaryKey(Base, PrepareMixin, RecordMixin):  # noqa

    __tablename__ = "primary_keys"

    table_name: str = Column(
        String(length=100),
        doc="Name of the table that this row stores the next available primary key for",
        nullable=False,
        primary_key=True,
    )

    current_id: int = Column(
        BIGINT(unsigned=True).with_variant(BIGINT, "sqlite"),
        doc="The current/latest id used in the table.",
        nullable=False,
        primary_key=False,
    )


class PrimaryKeyGenerator:
    """Keep track of DB objects' primary keys by ourselves rather than relying
    on SQLAlchemy, so we can supply them as arguments when creating association
    objects, such as TraceFrameLeafAssoc"""

    QUERY_CLASSES: Set[Type] = {
        Issue,
        IssueInstance,
        IssueInstanceFixInfo,
        SharedText,
        Run,
        TraceFrame,
        TraceFrameAnnotation,
    }

    # Map from class name to an ID range (next_id, max_reserved_id)
    pks: Dict[str, Tuple[int, int]] = {}

    def reserve(
        self,
        session: Session,
        saving_classes: List[Type],
        item_counts: Optional[Dict[str, int]] = None,
        use_lock: bool = False,
    ) -> "PrimaryKeyGenerator":
        """
        session - Session for DB operations.
        saving_classes - class objects that need to be saved e.g. Issue, Run
        item_counts - map from class name to the number of items, for preallocating
        id ranges
        """
        query_classes = {cls for cls in saving_classes if cls in self.QUERY_CLASSES}
        for cls in query_classes:
            if item_counts and cls.__name__ in item_counts:
                count = item_counts[cls.__name__]
            else:
                count = 1
            self._reserve_id_range(session, cls, count, use_lock)

        return self

    def _lock_pk_with_retries(
        self, session: Session, cls: Type
    ) -> Optional[PrimaryKey]:
        cls_pk: Optional[PrimaryKey] = None
        retries: int = 3
        while retries > 0:
            try:
                cls_pk = (
                    session.query(PrimaryKey)
                    .filter(PrimaryKey.table_name == cls.__name__)
                    .with_for_update()
                    .first()
                )
                # if we're here, the record has been locked, or there is no record
                retries = 0
            except exc.OperationalError as ex:
                # Failed to get exclusive lock on the record, so we retry
                retries -= 1
                # Re-raise the exception if our retries are exhausted
                if retries == 0:
                    raise ex
        return cls_pk

    def _reserve_id_range(
        self, session: Session, cls: Type, count: int, use_lock: bool
    ) -> None:
        cls_pk = self._lock_pk_with_retries(session, cls)
        if use_lock or not cls_pk:
            # If cls_pk is None, then we query the data table for the max ID
            # and use that as the current_id in the primary_keys table. This
            # should only occur once (the except with a rollback means any
            # additional attempt will fail to add a row, and use the "current"
            # id value)
            row = session.query(cls.id).order_by(cls.id.desc()).first()
            try:
                if not cls_pk:
                    session.execute(
                        "INSERT INTO primary_keys(table_name, current_id) \
                        VALUES (:table_name, :current_id)",
                        {
                            "table_name": cls.__name__,
                            "current_id": (row.id) if row else 0,
                        },
                    )
                else:
                    cls_pk.current_id = row.id if row else 0
                session.commit()
            except exc.SQLAlchemyError:
                session.rollback()
            cls_pk = self._lock_pk_with_retries(session, cls)

        if cls_pk:
            next_id = cls_pk.current_id + 1
            cls_pk.current_id = cls_pk.current_id + count
            pk_entry: Tuple[int, int] = (next_id, cls_pk.current_id)
            session.commit()
            self.pks[cls.__name__] = pk_entry

    def get(self, cls):
        assert cls in self.QUERY_CLASSES, (
            "%s primary key should be generated by SQLAlchemy" % cls.__name__
        )
        assert cls.__name__ in self.pks, (
            "%s primary key needs to be initialized before use" % cls.__name__
        )
        (pk, max_pk) = self.pks[cls.__name__]
        assert pk <= max_pk, "%s reserved primary key range exhausted" % cls.__name__
        self.pks[cls.__name__] = (pk + 1, max_pk)
        return pk


def create(engine):
    Base.metadata.create_all(engine)
