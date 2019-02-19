#!/usr/bin/env python3

import sys
from datetime import datetime
from io import StringIO
from unittest import TestCase
from unittest.mock import patch

from sapp.db import DB
from sapp.interactive import Interactive
from sapp.models import Issue, IssueInstance, Run, RunStatus, SharedText, SourceLocation


class InteractiveTest(TestCase):
    def setUp(self):
        self.db = DB("memory")
        self.interactive = Interactive("memory", "")
        self.interactive.db = self.db  # we need the tool to refer to the same db
        self.stdout = StringIO()
        self.stderr = StringIO()
        sys.stdout = self.stdout  # redirect output
        sys.stderr = self.stderr  # redirect output

    def tearDown(self):
        sys.stdout = sys.__stdout__  # reset redirect
        sys.stderr = sys.__stderr__  # reset redirect

    def testListIssuesBasic(self):
        issues = [
            Issue(
                id=1,
                handle="1",
                first_seen=datetime.now(),
                code=1000,
                callable="module.function1",
            ),
            Issue(
                id=2,
                handle="2",
                first_seen=datetime.now(),
                code=1001,
                callable="module.function2",
            ),
        ]

        message = SharedText(id=1, contents="message1")
        run = Run(id=1, date=datetime.now())

        issue_instances = [
            IssueInstance(
                id=1,
                run_id=1,
                message_id=1,
                filename="module.py",
                location=SourceLocation(1, 2, 3),
                issue_id=1,
            )
        ]

        with self.db.make_session() as session:
            session.add(issues[0])
            session.add(issues[1])
            session.add(message)
            session.add(run)
            session.add(issue_instances[0])
            session.commit()

        self.interactive.start_repl()
        self.interactive.issues()
        output = self.stdout.getvalue().strip()

        self.assertIn("Issue 1", output)
        self.assertIn("Code: 1000", output)
        self.assertIn("Message: message1", output)
        self.assertIn("Callable: module.function1", output)
        self.assertIn("Location: module.py:1|2|3", output)
        self.assertNotIn("module.function2", output)

    def testListIssuesFromLatestRun(self):
        issues = [
            Issue(
                id=1,
                handle="1",
                first_seen=datetime.now(),
                code=1000,
                callable="module.function1",
            )
        ]

        message = SharedText(id=1, contents="message1")
        runs = [
            Run(id=1, date=datetime.now(), status=RunStatus.FINISHED),
            Run(id=2, date=datetime.now(), status=RunStatus.FINISHED),
        ]

        issue_instances = [
            IssueInstance(
                id=1,
                run_id=1,
                message_id=1,
                filename="module.py",
                location=SourceLocation(1, 2, 3),
                issue_id=1,
            ),
            IssueInstance(
                id=2,
                run_id=2,
                message_id=1,
                filename="module.py",
                location=SourceLocation(1, 2, 3),
                issue_id=1,
            ),
        ]

        with self.db.make_session() as session:
            session.add(issues[0])
            session.add(message)
            session.add(runs[0])
            session.add(runs[1])
            session.add(issue_instances[0])
            session.add(issue_instances[1])
            session.commit()

        self.interactive.start_repl()
        self.interactive.issues()
        output = self.stdout.getvalue().strip()

        self.assertNotIn("Issue 1", output)
        self.assertIn("Issue 2", output)

    def testNoRunsFound(self):
        with self.assertRaises(SystemExit):
            self.interactive.start_repl()
        stderr = self.stderr.getvalue().strip()
        self.assertIn("No runs found.", stderr)

    def testListRuns(self):
        runs = [
            Run(id=1, date=datetime.now(), status=RunStatus.FINISHED),
            Run(id=2, date=datetime.now(), status=RunStatus.INCOMPLETE),
            Run(id=3, date=datetime.now(), status=RunStatus.FINISHED),
        ]

        with self.db.make_session() as session:
            session.add(runs[0])
            session.add(runs[1])
            session.add(runs[2])
            session.commit()

        self.interactive.start_repl()
        self.interactive.runs()
        output = self.stdout.getvalue().strip()

        self.assertIn("Run 1", output)
        self.assertNotIn("Run 2", output)
        self.assertIn("Run 3", output)

    def testSetRun(self):
        runs = [
            Run(id=1, date=datetime.now(), status=RunStatus.FINISHED),
            Run(id=2, date=datetime.now(), status=RunStatus.FINISHED),
        ]
        issue = Issue(
            id=1,
            handle="1",
            first_seen=datetime.now(),
            code=1000,
            callable="module.function1",
        )
        issue_instances = [
            IssueInstance(
                id=1,
                run_id=1,
                message_id=1,
                filename="module.py",
                location=SourceLocation(1, 2, 3),
                issue_id=1,
            ),
            IssueInstance(
                id=2,
                run_id=2,
                message_id=1,
                filename="module.py",
                location=SourceLocation(1, 2, 3),
                issue_id=1,
            ),
        ]

        with self.db.make_session() as session:
            session.add(runs[0])
            session.add(runs[1])
            session.add(issue)
            session.add(issue_instances[0])
            session.add(issue_instances[1])
            session.commit()

        self.interactive.start_repl()
        self.interactive.set_run(1)
        self.interactive.issues()
        output = self.stdout.getvalue().strip()

        self.assertIn("Issue 1", output)
        self.assertNotIn("Issue 2", output)

    def testSetRunNonExistent(self):
        runs = [
            Run(id=1, date=datetime.now(), status=RunStatus.FINISHED),
            Run(id=2, date=datetime.now(), status=RunStatus.INCOMPLETE),
        ]

        with self.db.make_session() as session:
            session.add(runs[0])
            session.add(runs[1])
            session.commit()

        self.interactive.start_repl()
        self.interactive.set_run(2)
        self.interactive.set_run(3)
        stderr = self.stderr.getvalue().strip()

        self.assertIn("Run 2 doesn't exist", stderr)
        self.assertIn("Run 3 doesn't exist", stderr)

    def testSetIssue(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        issue = Issue(
            id=1,
            handle="1",
            first_seen=datetime.now(),
            code=1000,
            callable="module.function1",
        )
        issue_instances = [
            IssueInstance(
                id=1,
                run_id=1,
                message_id=1,
                filename="module.py",
                location=SourceLocation(1, 2, 3),
                issue_id=1,
            ),
            IssueInstance(
                id=2,
                run_id=2,
                message_id=1,
                filename="module.py",
                location=SourceLocation(1, 2, 3),
                issue_id=1,
            ),
            IssueInstance(
                id=3,
                run_id=3,
                message_id=1,
                filename="module.py",
                location=SourceLocation(1, 2, 3),
                issue_id=1,
            ),
        ]

        with self.db.make_session() as session:
            session.add(run)
            session.add(issue)
            session.add(issue_instances[0])
            session.add(issue_instances[1])
            session.commit()

        self.interactive.start_repl()

        self.interactive.set_issue(2)
        self.interactive.show()
        stdout = self.stdout.getvalue().strip()
        self.assertNotIn("Issue 1", stdout)
        self.assertIn("Issue 2", stdout)
        self.assertNotIn("Issue 3", stdout)

        self.interactive.set_issue(1)
        self.interactive.show()
        stdout = self.stdout.getvalue().strip()
        self.assertIn("Issue 1", stdout)
        self.assertNotIn("Issue 3", stdout)

    def testSetIssueNonExistent(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)

        with self.db.make_session() as session:
            session.add(run)
            session.commit()

        self.interactive.start_repl()
        self.interactive.set_issue(1)
        stderr = self.stderr.getvalue().strip()

        self.assertIn("Issue 1 doesn't exist", stderr)

    def mock_pager(self, output_string):
        self.pager_calls += 1

    def testPager(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        issue = Issue(
            id=1,
            handle="1",
            first_seen=datetime.now(),
            code=1000,
            callable="module.function1",
        )
        issue_instance = IssueInstance(
            id=1,
            run_id=1,
            message_id=1,
            filename="module.py",
            location=SourceLocation(1, 2, 3),
            issue_id=1,
        )

        with self.db.make_session() as session:
            session.add(run)
            session.add(issue)
            session.add(issue_instance)
            session.commit()

        # Default is no pager in tests
        self.pager_calls = 0
        with patch("IPython.core.page.page", self.mock_pager):
            self.interactive.start_repl()
            self.interactive.issues()
            self.interactive.runs()
        self.assertEqual(self.pager_calls, 0)

        self.pager_calls = 0
        with patch("IPython.core.page.page", self.mock_pager):
            self.interactive.start_repl()
            self.interactive.issues(use_pager=True)
            self.interactive.runs(use_pager=True)
        self.assertEqual(self.pager_calls, 2)
