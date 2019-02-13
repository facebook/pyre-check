#!/usr/bin/env python3

import sys
from datetime import datetime
from io import StringIO
from unittest import TestCase

from sapp.db import DB
from sapp.interactive import Interactive
from sapp.models import Issue, IssueInstance, SharedText, SourceLocation


class InteractiveTest(TestCase):
    def setUp(self):
        self.db = DB("memory")
        self.interactive = Interactive("memory", "")
        self.interactive.db = self.db  # we need the tool to refer to the same db
        self.stdout = StringIO()
        sys.stdout = self.stdout  # redirect output

    def tearDown(self):
        sys.stdout = sys.__stdout__  # reset redirect

    def testListIssues(self):
        issues = [
            Issue(
                id=1,
                handle="1",
                first_seen=datetime.now(),
                last_seen=datetime.now(),
                code=1000,
                callable="module.function1",
            ),
            Issue(
                id=2,
                handle="2",
                first_seen=datetime.now(),
                last_seen=datetime.now(),
                code=1001,
                callable="module.function2",
            ),
        ]

        message = SharedText(id=1, contents="message1")

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
            session.add(issue_instances[0])
            session.commit()

        self.interactive.onecmd("list")
        list_output = self.stdout.getvalue().strip()

        self.assertEqual(6, len(list_output.split("\n")))
        self.assertIn("Issue 1", list_output)
        self.assertIn("Code: 1000", list_output)
        self.assertIn("Message: message1", list_output)
        self.assertIn("Callable: module.function1", list_output)
        self.assertIn("Location: module.py:1|2|3", list_output)
