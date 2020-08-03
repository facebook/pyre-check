from unittest import TestCase

from sqlalchemy.sql import func

from ..db import DB, DBType
from ..models import (
    DBID,
    IssueInstanceSharedTextAssoc,
    Run,
    RunStatus,
    create as create_models,
)
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
            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id) for issue in builder.where_codes_is_any_of([6016]).get()
            }
            self.assertIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)
            self.assertNotIn(3, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_codes_is_any_of([6017, 6018]).get()
            }
            self.assertNotIn(1, issue_ids)
            self.assertIn(2, issue_ids)
            self.assertIn(3, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id) for issue in builder.where_codes_is_any_of([1234]).get()
            }
            self.assertNotIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)
            self.assertNotIn(3, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_codes_is_any_of([6017])
                .where_codes_is_any_of([6018])
                .get()
            }
            self.assertNotIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)
            self.assertNotIn(3, issue_ids)

    def testWhereCallables(self) -> None:
        with self.db.make_session() as session:
            latest_run_id = (
                session.query(func.max(Run.id))
                .filter(Run.status == RunStatus.FINISHED)
                .scalar()
            )
            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_callables_is_any_of(["%sub%"]).get()
            }
            self.assertIn(1, issue_ids)
            self.assertIn(2, issue_ids)
            self.assertNotIn(3, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_callables_is_any_of(["1234"]).get()
            }
            self.assertNotIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)
            self.assertNotIn(3, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_callables_is_any_of(["%function3"]).get()
            }
            self.assertNotIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)
            self.assertIn(3, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_callables_is_any_of(["%function3"])
                .where_callables_is_any_of(["%sub%"])
                .get()
            }
            self.assertNotIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)
            self.assertNotIn(3, issue_ids)

    def testWhereFileNames(self) -> None:
        with self.db.make_session() as session:
            latest_run_id = (
                session.query(func.max(Run.id))
                .filter(Run.status == RunStatus.FINISHED)
                .scalar()
            )
            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_file_names_is_any_of(["1234"]).get()
            }
            self.assertNotIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)
            self.assertNotIn(3, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_file_names_is_any_of(["module/s%"]).get()
            }
            self.assertIn(1, issue_ids)
            self.assertIn(2, issue_ids)
            self.assertNotIn(3, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_file_names_is_any_of(["%__init__.py"]).get()
            }
            self.assertNotIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)
            self.assertIn(3, issue_ids)

    def testWhereTraceLength(self) -> None:
        with self.db.make_session() as session:
            latest_run_id = (
                session.query(func.max(Run.id))
                .filter(Run.status == RunStatus.FINISHED)
                .scalar()
            )

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_trace_length_to_sinks(1, 1).get()
            }
            self.assertIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)
            self.assertNotIn(3, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_trace_length_to_sources(1, 1).get()
            }
            self.assertIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)
            self.assertNotIn(3, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_trace_length_to_sources(0, 1).get()
            }
            self.assertIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)
            self.assertNotIn(3, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_trace_length_to_sinks(0, 1).get()
            }
            self.assertIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)
            self.assertNotIn(3, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_trace_length_to_sinks(0, 2).get()
            }
            self.assertIn(1, issue_ids)
            self.assertIn(2, issue_ids)
            self.assertNotIn(3, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_trace_length_to_sinks(0, 2).get()
            }
            self.assertIn(1, issue_ids)
            self.assertIn(2, issue_ids)
            self.assertNotIn(3, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_trace_length_to_sources(0, 1)
                .where_trace_length_to_sinks(0, 1)
                .get()
            }
            self.assertIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)
            self.assertNotIn(3, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_trace_length_to_sources(0, 1)
                .where_trace_length_to_sinks(0, 2)
                .get()
            }
            self.assertIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)
            self.assertNotIn(3, issue_ids)

    def testWhereAnyFeatures(self) -> None:
        self.fakes.instance()
        feature1 = self.fakes.feature("via:feature1")
        feature2 = self.fakes.feature("via:feature2")
        self.fakes.feature("via:feature3")

        self.fakes.save_all(self.db)

        with self.db.make_session() as session:
            session.add(
                IssueInstanceSharedTextAssoc(  # pyre-ignore
                    shared_text_id=feature1.id, issue_instance_id=1
                )
            )
            session.add(
                IssueInstanceSharedTextAssoc(  # pyre-ignore
                    shared_text_id=feature2.id, issue_instance_id=1
                )
            )
            session.commit()
            latest_run_id = (
                session.query(func.max(Run.id))
                .filter(Run.status == RunStatus.FINISHED)
                .scalar()
            )
            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_any_features(["via:feature1"]).get()
            }
            self.assertIn(1, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_any_features(
                    ["via:feature1", "via:feature2"]
                ).get()
            }
            self.assertIn(1, issue_ids)
            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_any_features(
                    ["via:feature1", "via:feature3"]
                ).get()
            }
            self.assertIn(1, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_any_features(["via:feature3"]).get()
            }
            self.assertNotIn(1, issue_ids)

    def testAssertAllFeatures(self) -> None:
        self.fakes.instance()
        feature1 = self.fakes.feature("via:feature1")
        feature2 = self.fakes.feature("via:feature2")
        self.fakes.feature("via:feature3")

        self.fakes.save_all(self.db)

        with self.db.make_session() as session:
            session.add(
                IssueInstanceSharedTextAssoc(  # pyre-ignore
                    shared_text_id=feature1.id, issue_instance_id=1
                )
            )
            session.add(
                IssueInstanceSharedTextAssoc(  # pyre-ignore
                    shared_text_id=feature2.id, issue_instance_id=1
                )
            )
            session.commit()
            latest_run_id = (
                session.query(func.max(Run.id))
                .filter(Run.status == RunStatus.FINISHED)
                .scalar()
            )
            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_all_features(["via:feature1"]).get()
            }
            self.assertIn(1, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_all_features(
                    ["via:feature1", "via:feature2"]
                ).get()
            }
            self.assertIn(1, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_all_features(["via:feature3"]).get()
            }
            self.assertNotIn(1, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_all_features(
                    ["via:feature1", "via:feature3"]
                ).get()
            }
            self.assertNotIn(1, issue_ids)

    def testAssertExcludeFeatures(self) -> None:
        feature1 = self.fakes.feature("via:feature1")
        feature2 = self.fakes.feature("via:feature2")
        self.fakes.feature("via:feature3")
        feature4 = self.fakes.feature("via:feature4")

        self.fakes.save_all(self.db)

        with self.db.make_session() as session:
            session.add(
                IssueInstanceSharedTextAssoc(  # pyre-ignore
                    shared_text_id=feature1.id, issue_instance_id=1
                )
            )
            session.add(
                IssueInstanceSharedTextAssoc(  # pyre-ignore
                    shared_text_id=feature2.id, issue_instance_id=1
                )
            )
            session.add(
                IssueInstanceSharedTextAssoc(  # pyre-ignore
                    shared_text_id=feature1.id, issue_instance_id=2
                )
            )
            session.add(
                IssueInstanceSharedTextAssoc(  # pyre-ignore
                    shared_text_id=feature4.id, issue_instance_id=2
                )
            )
            session.commit()
            latest_run_id = (
                session.query(func.max(Run.id))
                .filter(Run.status == RunStatus.FINISHED)
                .scalar()
            )

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id) for issue in builder.where_exclude_features([]).get()
            }
            self.assertIn(1, issue_ids)
            self.assertIn(2, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_exclude_features(["via:feature1"]).get()
            }
            self.assertNotIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_exclude_features(["via:feature2"]).get()
            }
            self.assertNotIn(1, issue_ids)
            self.assertIn(2, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_exclude_features(["via:feature3"]).get()
            }
            self.assertIn(1, issue_ids)
            self.assertIn(2, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_exclude_features(
                    ["via:feature1", "via:feature2"]
                ).get()
            }
            self.assertNotIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_exclude_features(
                    ["via:feature1", "via:feature4"]
                ).get()
            }
            self.assertNotIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_exclude_features(
                    ["via:feature2", "via:feature4"]
                ).get()
            }
            self.assertNotIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)

            builder = IssueQueryBuilder(latest_run_id)
            builder = builder.with_session(session)
            issue_ids = {
                int(issue.id)
                for issue in builder.where_exclude_features(
                    ["via:feature1", "via:feature3"]
                ).get()
            }
            self.assertNotIn(1, issue_ids)
            self.assertNotIn(2, issue_ids)
