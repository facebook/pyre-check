#!/usr/bin/env python3

import sys
from datetime import datetime
from io import StringIO
from unittest import TestCase

from sapp.db import DB
from sapp.interactive import Interactive
from sapp.models import Issue, IssueInstance, Run, SharedText, SourceLocation


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
            Run(id=1, date=datetime.now(), status="finished"),
            Run(id=2, date=datetime.now(), status="finished"),
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
