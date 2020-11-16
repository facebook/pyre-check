# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import logging
from collections import namedtuple
from itertools import tee
from typing import Dict, List, Optional, Set, Tuple, Type

from munch import Munch
from sqlalchemy import Column, String, and_, exc, inspect, or_, types
from sqlalchemy.dialects import mysql
from sqlalchemy.dialects.mysql import BIGINT
from sqlalchemy.orm import Session

from .iterutil import split_every


log: logging.Logger = logging.getLogger("sapp")


"""Number of variables that can safely be set on a single DB call"""
BATCH_SIZE = 450


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

    # pyre-fixme[2]: Parameter must be annotated.
    def __init__(self, id=None) -> None:
        self.resolve(id)
        self.local_id: int = DBID.next_id
        DBID.next_id += 1

    # pyre-fixme[3]: Return type must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    def resolve(self, id, is_new=True):
        self._check_type(id)
        self._id = id
        self.is_new = is_new
        return self

    # pyre-fixme[3]: Return type must be annotated.
    def resolved(self):
        id = self._id

        # We allow one level of a DBID pointing to another DBID
        if isinstance(id, DBID):
            id = id.resolved()

        return id

    # pyre-fixme[2]: Parameter must be annotated.
    def _check_type(self, id) -> None:
        if not isinstance(id, (int, type(None), DBID)):
            raise TypeError(
                "id expected to be type '{}' but was type '{}'".format(int, type(id))
            )

    # Allow DBIDs to be added and compared as ints
    # pyre-fixme[3]: Return type must be annotated.
    def __int__(self):
        return self.resolved()

    # pyre-fixme[3]: Return type must be annotated.
    def __str__(self):
        return str(self.resolved())

    # pyre-fixme[2]: Parameter must be annotated.
    def __add__(self, other) -> int:
        return int(self) + int(other)

    # pyre-fixme[2]: Parameter must be annotated.
    def __lt__(self, other) -> bool:
        return int(self) < int(other)

    # pyre-fixme[2]: Parameter must be annotated.
    def __gt__(self, other) -> bool:
        return int(self) > int(other)

    # pyre-fixme[2]: Parameter must be annotated.
    def __ge__(self, other) -> bool:
        return int(self) >= int(other)

    # pyre-fixme[2]: Parameter must be annotated.
    def __le__(self, other) -> bool:
        return int(self) <= int(other)

    def __repr__(self) -> str:
        return "<{}(id={}) object at 0x{:x}>".format(
            self.__class__.__name__, self._id, id(self)
        )


class DBIDType(types.TypeDecorator):
    impl = types.Integer

    # pyre-fixme[3]: Return type must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    def process_bind_param(self, value, dialect):
        # If it is a DBID wrapper, then write the contained value. Otherwise it
        # may be resolved already, or None.
        if isinstance(value, DBID):
            return value.resolved()
        else:
            return value

    # pyre-fixme[2]: parameter must be annotated.
    # pyre-fixme[2]: parameter must be annotated.
    def process_result_value(self, value, dialect) -> DBID:
        return DBID(value)

    # pyre-fixme[3]: Return type must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    def load_dialect_impl(self, dialect):
        if dialect.name == "mysql":
            return dialect.type_descriptor(mysql.INTEGER(unsigned=True))
        return self.impl


class BIGDBIDType(DBIDType):
    impl = types.BigInteger

    # pyre-fixme[3]: Return type must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    def load_dialect_impl(self, dialect):
        if dialect.name == "mysql":
            return dialect.type_descriptor(mysql.BIGINT(unsigned=True))
        return self.impl


class PrepareMixin(object):
    @classmethod
    # pyre-fixme[3]: Return type must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    def prepare(cls, session, pkgen, items):
        """This is called immediately before the items are written to the
        database. pkgen is passed in to allow last-minute resolving of ids.
        """
        for item in cls.merge(session, items):
            if hasattr(item, "id"):
                item.id.resolve(id=pkgen.get(cls), is_new=True)
            # pyre-fixme[16]: `PrepareMixin` has no attribute `to_dict`.
            yield cls.to_dict(item)

    @classmethod
    # pyre-fixme[3]: Return type must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    def merge(cls, session, items):
        """Models should override this to perform a merge"""
        return items

    @classmethod
    # pyre-fixme[3]: Return type must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    def _merge_by_key(cls, session, items, attr):
        return cls._merge_by_keys(
            session, items, lambda item: getattr(item, attr.key), attr
        )

    @classmethod
    # pyre-fixme[3]: Return type must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
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
                # pyre-fixme[16]: `PrepareMixin` has no attribute `id`.
                session.query(cls.id, *cls_attrs)
                .filter(or_(*(filters)))
                .all()
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
    # pyre-fixme[3]: Return type must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
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
    # pyre-fixme[4]: Attribute must be annotated.
    _record = None

    @classmethod
    # pyre-fixme[3]: Return type must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    def Record(cls, extra_fields=None, **kwargs):
        if not cls._record:
            if not extra_fields:
                extra_fields = []
            mapper = inspect(cls)
            keys = [c.key for c in mapper.column_attrs] + ["model"] + extra_fields
            cls._record = namedtuple(cls.__name__ + "Record", keys)

        return cls._record(model=cls, **kwargs)

    @classmethod
    # pyre-fixme[3]: Return type must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    def to_dict(cls, obj):
        return obj._asdict()


class MutableRecordMixin(object):
    @classmethod
    # pyre-fixme[2]: Parameter must be annotated.
    def Record(cls, **kwargs) -> Munch:
        return Munch(model=cls, **kwargs)

    @classmethod
    # pyre-fixme[3]: Return type must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    def to_dict(cls, obj):
        return obj.toDict()


class PrimaryKeyBase(PrepareMixin, RecordMixin):  # noqa
    """Subclass this and include your declarative_base mixin"""

    __tablename__ = "primary_keys"

    # pyre-fixme[8]: Attribute has type `str`; used as `Column[str]`.
    table_name: str = Column(
        String(length=100),
        doc="Name of the table that this row stores the next available primary key for",
        nullable=False,
        primary_key=True,
    )

    # pyre-fixme[8]: Attribute has type `int`; used as
    #  `Column[Variable[sqlalchemy.sql.type_api._U]]`.
    current_id: int = Column(
        BIGINT(unsigned=True).with_variant(BIGINT, "sqlite"),
        doc="The current/latest id used in the table.",
        nullable=False,
        primary_key=False,
    )


class PrimaryKeyGeneratorBase:  # pyre-ignore[13]
    """Keep track of DB objects' primary keys by ourselves rather than relying
    on SQLAlchemy, so we can supply them as arguments when creating association
    objects. Subclass to define PRIMARY_KEY class and QUERY_CLASSES."""

    # pyre-fixme[24]: Generic type `type` expects 1 type parameter, use
    #  `typing.Type` to avoid runtime subscripting errors.
    PRIMARY_KEY: Type
    # pyre-fixme[24]: Generic type `type` expects 1 type parameter, use
    #  `typing.Type` to avoid runtime subscripting errors.
    QUERY_CLASSES: Set[Type]

    # Map from class name to an ID range (next_id, max_reserved_id)
    pks: Dict[str, Tuple[int, int]] = {}

    def reserve(
        self,
        session: Session,
        # pyre-fixme[24]: Generic type `type` expects 1 type parameter, use
        #  `typing.Type` to avoid runtime subscripting errors.
        saving_classes: List[Type],
        item_counts: Optional[Dict[str, int]] = None,
        use_lock: bool = False,
    ) -> "PrimaryKeyGeneratorBase":
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

    # pyre-fixme[24]: Generic type `type` expects 1 type parameter, use
    #  `typing.Type` to avoid runtime subscripting errors.
    # pyre-fixme[24]: Generic type `type` expects 1 type parameter, use
    #  `typing.Type` to avoid runtime subscripting errors.
    def _lock_pk_with_retries(self, session: Session, cls: Type) -> Optional[Type]:
        # pyre-fixme[24]: Generic type `type` expects 1 type parameter, use
        #  `typing.Type` to avoid runtime subscripting errors.
        cls_pk: Optional[Type] = None
        retries: int = 3
        while retries > 0:
            try:
                cls_pk = (
                    session.query(self.PRIMARY_KEY)
                    .filter(self.PRIMARY_KEY.table_name == cls.__name__)
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
        self,
        session: Session,
        # pyre-fixme[24]: Generic type `type` expects 1 type parameter, use
        #  `typing.Type` to avoid runtime subscripting errors.
        cls: Type,
        count: int,
        use_lock: bool,
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

    # pyre-fixme[3]: Return type must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
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
