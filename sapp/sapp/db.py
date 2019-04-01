#!/usr/bin/env python3
"""
This file defines the underlying db used by SAPP library.
"""

import logging
from contextlib import contextmanager
from typing import Iterator

import sqlalchemy
from sqlalchemy.exc import OperationalError
from sqlalchemy.orm import Session, scoped_session, sessionmaker
from sqlalchemy.pool import AssertionPool

from . import errors, models
from .decorators import retryable


log = logging.getLogger("sapp")


class DBType(sqlalchemy.Enum):
    XDB = "xdb"  # not yet implemented
    SQLITE = "sqlite"
    MEMORY = "memory"


class DB(object):
    """Interact with the database type requested"""

    """File-based DB when using SQLITE"""
    DEFAULT_DB_FILE = "sapp.db"

    def __init__(
        self, dbtype, dbname=None, debug=False, read_only=False, assertions=False
    ):
        self.dbtype = dbtype
        self.dbname = dbname or self.DEFAULT_DB_FILE
        self.debug = debug
        self.read_only = read_only
        self.assertions = assertions

        self.poolclass = assertions and AssertionPool or None

        if dbtype == DBType.MEMORY:
            self.engine = sqlalchemy.create_engine(
                sqlalchemy.engine.url.URL("sqlite", database=":memory:"),
                echo=debug,
                poolclass=self.poolclass,
            )
        elif dbtype == DBType.SQLITE:
            self.engine = sqlalchemy.create_engine(
                sqlalchemy.engine.url.URL("sqlite", database=self.dbname),
                echo=debug,
                poolclass=self.poolclass,
            )
        elif dbtype == DBType.XDB:
            self._create_xdb_engine()
        else:
            raise errors.AIException("Invalid db type: " + dbtype)

        try:
            models.create(self.engine)
        except sqlalchemy.exc.NoSuchTableError:
            pass

    def _create_xdb_engine(self):
        raise NotImplementedError

    @contextmanager
    def make_session(self, *args, **kwargs) -> Iterator[Session]:
        session = self.make_session_object(*args, **kwargs)
        try:
            yield session
        finally:
            self.close_session(session)

    @retryable(num_tries=2, retryable_exs=[OperationalError])
    def make_session_object(self, *args, **kwargs):
        # use scoped_session so sessionmaker generates the same session in
        # different threads. This is useful for UTs.
        session = scoped_session(sessionmaker(bind=self.engine))(*args, **kwargs)
        ping_db(session)
        if self.dbtype == DBType.XDB:
            # Make sure SQL doesn't quit on us after 10s. Sometimes merging data takes
            # longer.
            session.execute("SET SESSION wait_timeout = %d" % 30)

        return session

    @retryable(num_tries=2, retryable_exs=[OperationalError])
    def close_session(self, session):
        session.close()


def ping_db(session):
    session.execute("SELECT 1")
