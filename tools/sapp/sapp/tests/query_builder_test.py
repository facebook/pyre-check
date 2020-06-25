import sys
from io import StringIO
from unittest import TestCase

from sqlalchemy.sql import func

from ..db import DB, DBType
from ..models import Run, RunStatus, create as create_models
from ..query_builder import IssueQueryBuilder
from .fake_object_generator import FakeObjectGenerator


class QueryBuilderTest(TestCase):
    def setUp(self) -> None:
        self.db = DB(DBType.MEMORY)
        create_models(self.db)
        self.fakes = FakeObjectGenerator()
        run = self.fakes.run()

        issue1 = self.fakes.issue(code=6016)
        self.fakes.instance(
            issue_id=issue1.id,
            callable="module.sub.function1",
            filename="module/sub.py",
            min_trace_length_to_sources=1,
            min_trace_length_to_sinks=1,
        )
        self.fakes.save_all(self.db)

        issue2 = self.fakes.issue(code=6017)
        self.fakes.instance(
            issue_id=issue2.id,
            callable="module.sub.function2",
            filename="module/sub.py",
            min_trace_length_to_sources=2,
            min_trace_length_to_sinks=2,
        )
        self.fakes.save_all(self.db)

        issue3 = self.fakes.issue(code=6018)
        self.fakes.instance(
            issue_id=issue3.id,
            callable="module.function3",
            filename="module/__init__.py",
            min_trace_length_to_sources=3,
            min_trace_length_to_sinks=3,
        )
        self.fakes.save_all(self.db)

        with self.db.make_session() as session:
            session.add(run)
            session.commit()

    def testWhereCode(self) -> None:
        with self.db.make_session() as session:
            latest_run_id = (
                session.query(func.max(Run.id))
                .filter(Run.status == RunStatus.FINISHED)
                .scalar()
            )
        builder = IssueQueryBuilder(self.db, latest_run_id)
        issue_ids = {int(issue.id) for issue in builder.where_codes([6016]).get()}
        self.assertIn(1, issue_ids)
        self.assertNotIn(2, issue_ids)
        self.assertNotIn(3, issue_ids)

        builder = IssueQueryBuilder(self.db, latest_run_id)
        issue_ids = {int(issue.id) for issue in builder.where_codes([6017, 6018]).get()}
        self.assertNotIn(1, issue_ids)
        self.assertIn(2, issue_ids)
        self.assertIn(3, issue_ids)

        builder = IssueQueryBuilder(self.db, latest_run_id)
        issue_ids = {int(issue.id) for issue in builder.where_codes([1234]).get()}
        self.assertNotIn(1, issue_ids)
        self.assertNotIn(2, issue_ids)
        self.assertNotIn(3, issue_ids)
