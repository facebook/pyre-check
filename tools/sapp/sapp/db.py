# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

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

from . import errors
from .decorators import retryable


# pyre-fixme[5]: Global expression must be annotated.
log = logging.getLogger("sapp")


class DBType(sqlalchemy.Enum):
    XDB = "xdb"  # not yet implemented
    SQLITE = "sqlite"
    MEMORY = "memory"


class DB(object):
    """Interact with the database type requested"""

    """File-based DB when using SQLITE"""
    DEFAULT_DB_FILE = "sapp.db"

    # pyre-fixme[3]: Return type must be annotated.
    def __init__(
        self,
        # pyre-fixme[2]: Parameter must be annotated.
        dbtype,
        # pyre-fixme[2]: Parameter must be annotated.
        dbname=None,
        # pyre-fixme[2]: Parameter must be annotated.
        debug=False,
        # pyre-fixme[2]: Parameter must be annotated.
        read_only=False,
        # pyre-fixme[2]: Parameter must be annotated.
        assertions=False,
    ):
        # pyre-fixme[4]: Attribute must be annotated.
        self.dbtype = dbtype
        # pyre-fixme[4]: Attribute must be annotated.
        self.dbname = dbname or self.DEFAULT_DB_FILE
        # pyre-fixme[4]: Attribute must be annotated.
        self.debug = debug
        # pyre-fixme[4]: Attribute must be annotated.
        self.read_only = read_only
        # pyre-fixme[4]: Attribute must be annotated.
        self.assertions = assertions

        # pyre-fixme[4]: Attribute must be annotated.
        self.poolclass = assertions and AssertionPool or None

        if dbtype == DBType.MEMORY:
            # pyre-fixme[4]: Attribute must be annotated.
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

    # pyre-fixme[3]: Return type must be annotated.
    def _create_xdb_engine(self):
        raise NotImplementedError

    @contextmanager
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    def make_session(self, *args, **kwargs) -> Iterator[Session]:
        session = self.make_session_object(*args, **kwargs)
        try:
            yield session
        finally:
            self.close_session(session)

    @retryable(num_tries=2, retryable_exs=[OperationalError])
    # pyre-fixme[3]: Return type must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
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
    # pyre-fixme[3]: Return type must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    def close_session(self, session):
        session.close()


# pyre-fixme[3]: Return type must be annotated.
# pyre-fixme[2]: Parameter must be annotated.
def ping_db(session):
    session.execute("SELECT 1")
