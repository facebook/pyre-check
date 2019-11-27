# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

from sqlalchemy import Column, ForeignKey, Integer, String, create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import scoped_session, sessionmaker


engine = create_engine("sqlite:///database.sqlite3", convert_unicode=True)
session = scoped_session(sessionmaker(autocommit=False, autoflush=False, bind=engine))

Base = declarative_base()
Base.query = session.query_property()


class Run(Base):
    __tablename__ = "runs"

    run_id = Column(Integer, primary_key=True)


class Issue(Base):
    __tablename__ = "issues"

    issue_id = Column(Integer, primary_key=True)
    source = Column(String)
    sink = Column(String)

    run = Column(Integer, ForeignKey("runs.run_id"))
