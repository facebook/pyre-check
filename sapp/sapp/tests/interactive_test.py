#!/usr/bin/env python3

import sys
from datetime import datetime
from io import StringIO
from unittest import TestCase
from unittest.mock import patch

from sapp.db import DB
from sapp.interactive import Interactive, TraceTuple
from sapp.models import (
    Issue,
    IssueInstance,
    IssueInstancePostconditionAssoc,
    IssueInstancePreconditionAssoc,
    IssueInstanceTraceFrameAssoc,
    Postcondition,
    PostconditionSourceAssoc,
    Precondition,
    PreconditionSinkAssoc,
    Run,
    RunStatus,
    SharedText,
    Sink,
    Source,
    SourceLocation,
    TraceFrame,
    TraceKind,
)


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

    def _clear_stdout(self):
        self.stdout = StringIO()
        sys.stdout = self.stdout

    def _add_to_session(self, session, data):
        if not isinstance(data, list):
            session.add(data)
            return

        for row in data:
            session.add(row)

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

        issue_instance = IssueInstance(
            id=1,
            run_id=1,
            message_id=1,
            filename="module.py",
            location=SourceLocation(1, 2, 3),
            issue_id=1,
        )

        with self.db.make_session() as session:
            self._add_to_session(session, issues)
            session.add(message)
            session.add(run)
            session.add(issue_instance)
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
        issue = Issue(
            id=1,
            handle="1",
            first_seen=datetime.now(),
            code=1000,
            callable="module.function1",
        )

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
            session.add(issue)
            session.add(message)
            self._add_to_session(session, runs)
            self._add_to_session(session, issue_instances)
            session.commit()

        self.interactive.start_repl()
        self.interactive.issues()
        output = self.stdout.getvalue().strip()

        self.assertNotIn("Issue 1", output)
        self.assertIn("Issue 2", output)

    def _list_issues_filter_setup(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        issues = [
            Issue(
                id=1,
                handle="1",
                first_seen=datetime.now(),
                code=1000,
                callable="module.sub.function1",
                filename="module/sub.py",
            ),
            Issue(
                id=2,
                handle="2",
                first_seen=datetime.now(),
                code=1001,
                callable="module.sub.function2",
                filename="module/sub.py",
            ),
            Issue(
                id=3,
                handle="3",
                first_seen=datetime.now(),
                code=1002,
                callable="module.function3",
                filename="module/__init__.py",
            ),
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
                run_id=1,
                message_id=1,
                filename="module.py",
                location=SourceLocation(1, 2, 3),
                issue_id=2,
            ),
            IssueInstance(
                id=3,
                run_id=1,
                message_id=1,
                filename="module.py",
                location=SourceLocation(1, 2, 3),
                issue_id=3,
            ),
        ]

        with self.db.make_session() as session:
            session.add(run)
            self._add_to_session(session, issues)
            self._add_to_session(session, issue_instances)
            session.commit()

    def testListIssuesFilterCodes(self):
        self._list_issues_filter_setup()

        self.interactive.start_repl()
        self.interactive.issues(codes=1000)
        stderr = self.stderr.getvalue().strip()
        self.assertIn("'codes' should be a list", stderr)

        self.interactive.issues(codes=[1000])
        output = self.stdout.getvalue().strip()
        self.assertIn("Issue 1", output)
        self.assertNotIn("Issue 2", output)
        self.assertNotIn("Issue 3", output)

        self._clear_stdout()
        self.interactive.issues(codes=[1001, 1002])
        output = self.stdout.getvalue().strip()
        self.assertNotIn("Issue 1", output)
        self.assertIn("Issue 2", output)
        self.assertIn("Issue 3", output)

    def testListIssuesFilterCallables(self):
        self._list_issues_filter_setup()

        self.interactive.start_repl()
        self.interactive.issues(callables="function3")
        stderr = self.stderr.getvalue().strip()
        self.assertIn("'callables' should be a list", stderr)

        self.interactive.issues(callables=["%sub%"])
        output = self.stdout.getvalue().strip()
        self.assertIn("Issue 1", output)
        self.assertIn("Issue 2", output)
        self.assertNotIn("Issue 3", output)

        self._clear_stdout()
        self.interactive.issues(callables=["%function3"])
        output = self.stdout.getvalue().strip()
        self.assertNotIn("Issue 1", output)
        self.assertNotIn("Issue 2", output)
        self.assertIn("Issue 3", output)

    def testListIssuesFilterFilenames(self):
        self._list_issues_filter_setup()

        self.interactive.start_repl()
        self.interactive.issues(filenames="hello.py")
        stderr = self.stderr.getvalue().strip()
        self.assertIn("'filenames' should be a list", stderr)

        self.interactive.issues(filenames=["module/s%"])
        output = self.stdout.getvalue().strip()
        self.assertIn("Issue 1", output)
        self.assertIn("Issue 2", output)
        self.assertNotIn("Issue 3", output)

        self._clear_stdout()
        self.interactive.issues(filenames=["%__init__.py"])
        output = self.stdout.getvalue().strip()
        self.assertNotIn("Issue 1", output)
        self.assertNotIn("Issue 2", output)
        self.assertIn("Issue 3", output)

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
            self._add_to_session(session, runs)
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
            self._add_to_session(session, runs)
            self._add_to_session(session, issue_instances)
            session.add(issue)
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
            self._add_to_session(session, runs)
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
            self._add_to_session(session, issue_instances)
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

    def testGetSources(self):
        """
        issue_instance(1)
        - postcondition(1)
          - source(1)
        - postcondition(2)
          - source(2)
        """
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
        sources = [
            Source(id=1, name="source1"),
            Source(id=2, name="source2"),
            Source(id=3, name="source3"),
        ]
        postconditions = [
            Postcondition(
                id=1,
                caller="caller",
                callee="callee",
                callee_location=SourceLocation(1, 1, 1),
                filename="filename",
            ),
            Postcondition(
                id=2,
                caller="caller",
                callee="callee",
                callee_location=SourceLocation(1, 1, 1),
                filename="filename",
            ),
        ]
        assocs = [
            PostconditionSourceAssoc(postcondition_id=1, source_id=1),
            PostconditionSourceAssoc(postcondition_id=2, source_id=2),
            IssueInstancePostconditionAssoc(issue_instance_id=1, postcondition_id=1),
            IssueInstancePostconditionAssoc(issue_instance_id=1, postcondition_id=2),
        ]

        with self.db.make_session() as session:
            session.add(run)
            session.add(issue)
            session.add(issue_instance)
            self._add_to_session(session, sources)
            self._add_to_session(session, postconditions)
            self._add_to_session(session, assocs)
            session.commit()

            sources = self.interactive._get_sources(session, issue_instance)

        self.assertEqual(len(sources), 2)
        self.assertIn("source1", sources)
        self.assertIn("source2", sources)

    def testGetSinks(self):
        """
        issue_instance(1)
        - precondition(1)
          - source(1)
        - precondition(2)
          - source(2)
        """
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
        sinks = [
            Sink(id=1, name="sink1"),
            Sink(id=2, name="sink2"),
            Sink(id=3, name="sink3"),
        ]
        preconditions = [
            Precondition(
                id=1,
                caller="caller",
                callee="callee",
                callee_location=SourceLocation(1, 1, 1),
                filename="filename",
                caller_condition="condition",
                callee_condition="condition",
                message="mesage",
            ),
            Precondition(
                id=2,
                caller="caller",
                callee="callee",
                callee_location=SourceLocation(1, 1, 1),
                filename="filename",
                caller_condition="condition",
                callee_condition="condition",
                message="mesage",
            ),
        ]
        assocs = [
            PreconditionSinkAssoc(precondition_id=1, sink_id=1),
            PreconditionSinkAssoc(precondition_id=2, sink_id=2),
            IssueInstancePreconditionAssoc(issue_instance_id=1, precondition_id=1),
            IssueInstancePreconditionAssoc(issue_instance_id=1, precondition_id=2),
        ]

        with self.db.make_session() as session:
            session.add(run)
            session.add(issue)
            session.add(issue_instance)
            self._add_to_session(session, sinks)
            self._add_to_session(session, preconditions)
            self._add_to_session(session, assocs)
            session.commit()

            sinks = self.interactive._get_sinks(session, issue_instance)

        self.assertEqual(len(sinks), 2)
        self.assertIn("sink1", sinks)
        self.assertIn("sink2", sinks)

    def _basic_trace_frames(self):
        return [
            TraceFrame(
                id=1,
                kind=TraceKind.POSTCONDITION,
                caller="call1",
                caller_port="result",
                callee="call2",
                callee_port="formal",
                callee_location=SourceLocation(1, 1),
                filename="file.py",
                run_id=1,
            ),
            TraceFrame(
                id=2,
                kind=TraceKind.PRECONDITION,
                caller="call2",
                caller_port="formal",
                callee="leaf",
                callee_port="sink",
                callee_location=SourceLocation(1, 2),
                filename="file.py",
                run_id=1,
            ),
        ]

    def testNextTraceFrame(self):
        trace_frames = self._basic_trace_frames()
        with self.db.make_session() as session:
            self._add_to_session(session, trace_frames)
            next_frame = self.interactive._next_trace_frame(session, trace_frames[0])
            self.assertEqual(int(next_frame.id), trace_frames[1].id)

    def testNavigateTraceFrames(self):
        trace_frames = self._basic_trace_frames()
        with self.db.make_session() as session:
            self._add_to_session(session, trace_frames)
            result = self.interactive._navigate_trace_frames(session, trace_frames[0])
            self.assertEqual(len(result), 2)
            self.assertEqual(int(result[0].id), 1)
            self.assertEqual(int(result[1].id), 2)

    def testCreateTraceTuples(self):
        postcondition_traces = [
            TraceFrame(
                callee="leaf",
                callee_port="source",
                caller="dummy caller",
                filename="file1.py",
                callee_location=SourceLocation(1, 1, 1),
            ),
            TraceFrame(
                callee="call2",
                callee_port="result",
                caller="dummy caller",
                filename="file2.py",
                callee_location=SourceLocation(1, 1, 2),
            ),
            TraceFrame(
                callee="call3",
                callee_port="result",
                filename="file3.py",
                callee_location=SourceLocation(1, 1, 3),
                caller="main",
                caller_port="root",
            ),
        ]
        precondition_traces = [
            TraceFrame(
                callee="call4",
                callee_port="result",
                caller="dummy caller",
                filename="file4.py",
                callee_location=SourceLocation(1, 1, 4),
            ),
            TraceFrame(
                callee="call5",
                callee_port="result",
                caller="dummy caller",
                filename="file5.py",
                callee_location=SourceLocation(1, 1, 5),
            ),
            TraceFrame(
                callee="leaf",
                callee_port="sink",
                caller="dummy caller",
                filename="file6.py",
                callee_location=SourceLocation(1, 1, 6),
            ),
        ]
        trace_tuples = self.interactive._create_trace_tuples(
            postcondition_traces, precondition_traces
        )
        self.assertEqual(len(trace_tuples), 7)
        self.assertEqual(
            trace_tuples,
            [
                TraceTuple("leaf", "source"),
                TraceTuple("call2", "result", "file1.py", SourceLocation(1, 1, 1)),
                TraceTuple("call3", "result", "file2.py", SourceLocation(1, 1, 2)),
                TraceTuple("main", "root", "file3.py", SourceLocation(1, 1, 3)),
                TraceTuple("call4", "result", "file4.py", SourceLocation(1, 1, 4)),
                TraceTuple("call5", "result", "file5.py", SourceLocation(1, 1, 5)),
                TraceTuple("leaf", "sink", "file6.py", SourceLocation(1, 1, 6)),
            ],
        )

    def testOutputTraceTuples(self):
        trace_tuples = [
            TraceTuple("leaf", "source"),
            TraceTuple("call2", "result", "file1.py", SourceLocation(1, 1, 1)),
            TraceTuple("call3", "result", "file2.py", SourceLocation(1, 1, 2)),
            TraceTuple("main", "root", "file3.py", SourceLocation(1, 1, 3)),
            TraceTuple("call4", "result", "file4.py", SourceLocation(1, 1, 4)),
            TraceTuple("call5", "result", "file5.py", SourceLocation(1, 1, 5)),
            TraceTuple("leaf", "sink", "file6.py", SourceLocation(1, 1, 6)),
        ]
        self.interactive.current_trace_frame_index = 1
        self.interactive._output_trace_tuples(trace_tuples)
        output = self.stdout.getvalue()
        self.assertEqual(
            output.split("\n"),
            [
                "     leaf  source",
                " --> call2 result file1.py:1|1|1",
                "     call3 result file2.py:1|1|2",
                "     main  root   file3.py:1|1|3",
                "     call4 result file4.py:1|1|4",
                "     call5 result file5.py:1|1|5",
                "     leaf  sink   file6.py:1|1|6",
                "",
            ],
        )

        self._clear_stdout()
        self.interactive.current_trace_frame_index = 4
        self.interactive._output_trace_tuples(trace_tuples)
        output = self.stdout.getvalue()
        self.assertEqual(
            output.split("\n"),
            [
                "     leaf  source",
                "     call2 result file1.py:1|1|1",
                "     call3 result file2.py:1|1|2",
                "     main  root   file3.py:1|1|3",
                " --> call4 result file4.py:1|1|4",
                "     call5 result file5.py:1|1|5",
                "     leaf  sink   file6.py:1|1|6",
                "",
            ],
        )

    def testTrace(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        issue = Issue(
            id=1, handle="1", first_seen=datetime.now(), code=1000, callable="call1"
        )
        issue_instance = IssueInstance(
            id=1,
            run_id=1,
            message_id=1,
            filename="file.py",
            location=SourceLocation(1, 1, 1),
            issue_id=1,
        )
        trace_frames = [
            TraceFrame(
                id=1,
                kind=TraceKind.POSTCONDITION,
                caller="call1",
                caller_port="root",
                callee="leaf",
                callee_port="source",
                callee_location=SourceLocation(1, 1, 1),
                filename="file.py",
                run_id=1,
            ),
            TraceFrame(
                id=2,
                kind=TraceKind.PRECONDITION,
                caller="call1",
                caller_port="root",
                callee="leaf",
                callee_port="sink",
                callee_location=SourceLocation(1, 1, 2),
                filename="file.py",
                run_id=1,
            ),
        ]
        assocs = [
            IssueInstanceTraceFrameAssoc(trace_frame_id=1, issue_instance_id=1),
            IssueInstanceTraceFrameAssoc(trace_frame_id=2, issue_instance_id=1),
        ]

        with self.db.make_session() as session:
            session.add(run)
            session.add(issue)
            session.add(issue_instance)
            self._add_to_session(session, trace_frames)
            self._add_to_session(session, assocs)
            session.commit()

        self.interactive.start_repl()
        self.interactive.trace()
        stderr = self.stderr.getvalue().strip()
        self.assertIn("Use 'set_issue(ID)' to select an issue first.", stderr)

        self.interactive.set_issue(1)
        self.interactive.trace()
        output = self.stdout.getvalue().strip()
        self.assertIn("     leaf  source", output)
        self.assertIn(" --> call1 root   file.py:1|1|1", output)
        self.assertIn("     leaf  sink   file.py:1|1|2", output)

    def testTraceMissingFrames(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        issue = Issue(
            id=1, handle="1", first_seen=datetime.now(), code=1000, callable="call1"
        )
        issue_instance = IssueInstance(
            id=1,
            run_id=1,
            message_id=1,
            filename="file.py",
            location=SourceLocation(1, 1, 1),
            issue_id=1,
        )
        trace_frames = self._basic_trace_frames()
        assocs = [
            IssueInstanceTraceFrameAssoc(trace_frame_id=1, issue_instance_id=1),
            IssueInstanceTraceFrameAssoc(trace_frame_id=2, issue_instance_id=1),
        ]

        # trace_frame[0] no longer connects to trace_frame[1]
        trace_frames[0].callee = "ends_here"
        with self.db.make_session() as session:
            session.add(run)
            session.add(issue)
            session.add(issue_instance)
            self._add_to_session(session, trace_frames)
            self._add_to_session(session, assocs)
            session.commit()

        self.interactive.start_repl()
        self.interactive.set_issue(1)
        self.interactive.trace()
        stdout = self.stdout.getvalue().strip()
        self.assertIn("Missing trace frame: ends_here:formal", stdout)

    def testTraceCursorLocation(self):
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
        trace_frames = [
            TraceFrame(
                id=1,
                kind=TraceKind.POSTCONDITION,
                caller="call1",
                caller_port="root",
                callee="leaf",
                callee_port="source",
                callee_location=SourceLocation(1, 1),
                filename="file.py",
                run_id=1,
            ),
            TraceFrame(
                id=2,
                kind=TraceKind.PRECONDITION,
                caller="call1",
                caller_port="root",
                callee="leaf",
                callee_port="sink",
                callee_location=SourceLocation(1, 2),
                filename="file.py",
                run_id=1,
            ),
        ]
        assocs = [
            IssueInstanceTraceFrameAssoc(trace_frame_id=1, issue_instance_id=1),
            IssueInstanceTraceFrameAssoc(trace_frame_id=2, issue_instance_id=1),
        ]
        with self.db.make_session() as session:
            session.add(run)
            session.add(issue)
            session.add(issue_instance)
            self._add_to_session(session, trace_frames)
            self._add_to_session(session, assocs)
            session.commit()

        self.interactive.start_repl()
        self.interactive.set_issue(1)
        self.assertEqual(self.interactive.current_trace_frame_index, 1)
        self.interactive.next_cursor_location()
        self.assertEqual(self.interactive.current_trace_frame_index, 2)
        self.interactive.next_cursor_location()
        self.assertEqual(self.interactive.current_trace_frame_index, 2)
        self.interactive.prev_cursor_location()
        self.assertEqual(self.interactive.current_trace_frame_index, 1)
        self.interactive.prev_cursor_location()
        self.assertEqual(self.interactive.current_trace_frame_index, 1)

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
