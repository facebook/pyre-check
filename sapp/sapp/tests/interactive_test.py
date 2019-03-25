#!/usr/bin/env python3

import os
import sys
from datetime import datetime
from io import StringIO
from unittest import TestCase
from unittest.mock import mock_open, patch

from sapp.db import DB
from sapp.decorators import UserError
from sapp.interactive import Interactive, TraceTuple
from sapp.models import (
    Issue,
    IssueInstance,
    IssueInstanceSharedTextAssoc,
    IssueInstanceTraceFrameAssoc,
    Run,
    RunStatus,
    SharedText,
    SharedTextKind,
    SourceLocation,
    TraceFrame,
    TraceFrameLeafAssoc,
    TraceKind,
)


class InteractiveTest(TestCase):
    def setUp(self) -> None:
        self.db = DB("memory")
        self.interactive = Interactive(self.db, "")
        self.interactive.db = self.db  # we need the tool to refer to the same db
        self.stdout = StringIO()
        self.stderr = StringIO()
        sys.stdout = self.stdout  # redirect output
        sys.stderr = self.stderr  # redirect output

    def tearDown(self) -> None:
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

    def _generic_issue(self, id: int = 1, callable: str = "call1") -> Issue:
        return Issue(
            id=id,
            handle=str(id),
            first_seen=datetime.now(),
            code=1000 + id - 1,
            callable=callable,
        )

    def _generic_issue_instance(
        self, id: int = 1, run_id: int = 1, issue_id: int = 1, filename: str = "file.py"
    ) -> IssueInstance:
        return IssueInstance(
            id=id,
            run_id=run_id,
            message_id=1,
            filename=filename,
            location=SourceLocation(1, 2, 3),
            issue_id=issue_id,
        )

    def testState(self):
        self.interactive.current_run_id = 1
        self.interactive.current_issue_id = 2
        self.interactive.current_frame_id = 3
        self.interactive.sources = {1}
        self.interactive.sinks = {2}

        self.interactive.state()
        output = self.stdout.getvalue()
        self.assertIn("Database: memory:sapp.db", output)
        self.assertIn("Repository directory: ", output)
        self.assertIn("Current run: 1", output)
        self.assertIn("Current issue: 2", output)
        self.assertIn("Current trace frame: 3", output)
        self.assertIn("Sources filter: {1}", output)
        self.assertIn("Sinks filter: {2}", output)

    def testListIssuesBasic(self):
        issues = [
            self._generic_issue(id=1, callable="module.function1"),
            self._generic_issue(id=2, callable="module.function2"),
        ]

        message = SharedText(id=1, contents="message1")
        run = Run(id=1, date=datetime.now())

        issue_instance = self._generic_issue_instance()

        with self.db.make_session() as session:
            self._add_to_session(session, issues)
            session.add(message)
            session.add(run)
            session.add(issue_instance)
            session.commit()

        self.interactive.setup()
        self.interactive.issues()
        output = self.stdout.getvalue().strip()

        self.assertIn("Issue 1", output)
        self.assertIn("Code: 1000", output)
        self.assertIn("Message: message1", output)
        self.assertIn("Callable: module.function1", output)
        self.assertIn("Location: file.py:1|2|3", output)
        self.assertNotIn("module.function2", output)

    def testListIssuesFromLatestRun(self):
        issue = self._generic_issue()

        message = SharedText(id=1, contents="message1")
        runs = [
            Run(id=1, date=datetime.now(), status=RunStatus.FINISHED),
            Run(id=2, date=datetime.now(), status=RunStatus.FINISHED),
        ]

        issue_instances = [
            self._generic_issue_instance(id=1, run_id=1),
            self._generic_issue_instance(id=2, run_id=2),
        ]

        with self.db.make_session() as session:
            session.add(issue)
            session.add(message)
            self._add_to_session(session, runs)
            self._add_to_session(session, issue_instances)
            session.commit()

        self.interactive.setup()
        self.interactive.issues()
        output = self.stdout.getvalue().strip()

        self.assertNotIn("Issue 1", output)
        self.assertIn("Issue 2", output)

    def _list_issues_filter_setup(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        issues = [
            self._generic_issue(id=1, callable="module.sub.function1"),
            self._generic_issue(id=2, callable="module.sub.function2"),
            self._generic_issue(id=3, callable="module.function3"),
        ]
        issue_instances = [
            self._generic_issue_instance(id=1, issue_id=1, filename="module/sub.py"),
            self._generic_issue_instance(id=2, issue_id=2, filename="module/sub.py"),
            self._generic_issue_instance(
                id=3, issue_id=3, filename="module/__init__.py"
            ),
        ]

        with self.db.make_session() as session:
            session.add(run)
            self._add_to_session(session, issues)
            self._add_to_session(session, issue_instances)
            session.commit()

    def testListIssuesFilterCodes(self):
        self._list_issues_filter_setup()

        self.interactive.setup()
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

        self.interactive.setup()
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

        self.interactive.setup()
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
        self.interactive.setup()
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

        self.interactive.setup()
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
        issue = self._generic_issue()
        issue_instances = [
            self._generic_issue_instance(id=1, run_id=1),
            self._generic_issue_instance(id=2, run_id=2),
        ]

        with self.db.make_session() as session:
            self._add_to_session(session, runs)
            self._add_to_session(session, issue_instances)
            session.add(issue)
            session.commit()

        self.interactive.setup()
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

        self.interactive.setup()
        self.interactive.set_run(2)
        self.interactive.set_run(3)
        stderr = self.stderr.getvalue().strip()

        self.assertIn("Run 2 doesn't exist", stderr)
        self.assertIn("Run 3 doesn't exist", stderr)

    def testSetIssue(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        issue = self._generic_issue()
        issue_instances = [
            self._generic_issue_instance(id=1, run_id=1),
            self._generic_issue_instance(id=2, run_id=2),
            self._generic_issue_instance(id=3, run_id=3),
        ]

        with self.db.make_session() as session:
            session.add(run)
            session.add(issue)
            self._add_to_session(session, issue_instances)
            session.commit()

        self.interactive.setup()

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

        self.interactive.setup()
        self.interactive.set_issue(1)
        stderr = self.stderr.getvalue().strip()

        self.assertIn("Issue 1 doesn't exist", stderr)

    def testGetSources(self):
        issue_instance = self._generic_issue_instance()
        sources = [
            SharedText(id=1, contents="source1", kind=SharedTextKind.SOURCE),
            SharedText(id=2, contents="source2", kind=SharedTextKind.SOURCE),
            SharedText(id=3, contents="source3", kind=SharedTextKind.SOURCE),
        ]
        assocs = [
            IssueInstanceSharedTextAssoc(shared_text_id=1, issue_instance_id=1),
            IssueInstanceSharedTextAssoc(shared_text_id=2, issue_instance_id=1),
        ]

        with self.db.make_session() as session:
            session.add(issue_instance)
            self._add_to_session(session, sources)
            self._add_to_session(session, assocs)
            session.commit()

            sources = self.interactive._get_leaves(
                session, issue_instance, SharedTextKind.SOURCE
            )

        self.assertEqual(len(sources), 2)
        self.assertIn("source1", sources)
        self.assertIn("source2", sources)

    def testGetSinks(self):
        return
        issue_instance = self._generic_issue_instance()
        sinks = [
            SharedText(id=1, contents="sink1", kind=SharedTextKind.SINK),
            SharedText(id=2, contents="sink2", kind=SharedTextKind.SINK),
            SharedText(id=3, contents="sink3", kind=SharedTextKind.SINK),
        ]
        assocs = [
            IssueInstanceSharedTextAssoc(shared_text_id=1, issue_instance_id=1),
            IssueInstanceSharedTextAssoc(shared_text_id=2, issue_instance_id=1),
        ]

        with self.db.make_session() as session:
            session.add(issue_instance)
            self._add_to_session(session, sinks)
            self._add_to_session(session, assocs)
            session.commit()

            sinks = self.interactive._get_leaves(
                session, issue_instance, SharedTextKind.SINK
            )

        self.assertEqual(len(sinks), 2)
        self.assertIn("sink1", sinks)
        self.assertIn("sink2", sinks)

    def _basic_trace_frames(self):
        return [
            TraceFrame(
                id=1,
                kind=TraceKind.PRECONDITION,
                caller="call1",
                caller_port="root",
                callee="call2",
                callee_port="param0",
                callee_location=SourceLocation(1, 1),
                filename="file.py",
                run_id=1,
            ),
            TraceFrame(
                id=2,
                kind=TraceKind.PRECONDITION,
                caller="call2",
                caller_port="param0",
                callee="leaf",
                callee_port="sink",
                callee_location=SourceLocation(1, 2),
                filename="file.py",
                run_id=1,
            ),
        ]

    def testNextTraceFrames(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        trace_frames = self._basic_trace_frames()
        sink = SharedText(id=1, contents="sink1", kind=SharedTextKind.SINK)
        assoc = TraceFrameLeafAssoc(trace_frame_id=2, leaf_id=1, trace_length=1)
        with self.db.make_session() as session:
            self._add_to_session(session, trace_frames)
            session.add(run)
            session.add(sink)
            session.add(assoc)
            session.commit()

            self.interactive.setup()
            self.interactive.sinks = {"sink1"}
            next_frames = self.interactive._next_forward_trace_frames(
                session, trace_frames[0]
            )
            self.assertEqual(len(next_frames), 1)
            self.assertEqual(int(next_frames[0].id), int(trace_frames[1].id))

    def testNextTraceFramesMultipleRuns(self):
        runs = [
            Run(id=1, date=datetime.now(), status=RunStatus.FINISHED),
            Run(id=2, date=datetime.now(), status=RunStatus.FINISHED),
        ]
        trace_frames_run1 = self._basic_trace_frames()
        trace_frames_run2 = self._basic_trace_frames()
        trace_frames_run2[0].id = 3
        trace_frames_run2[0].run_id = 2
        trace_frames_run2[1].id = 4
        trace_frames_run2[1].run_id = 2

        sink = SharedText(id=1, contents="sink1", kind=SharedTextKind.SINK)
        assocs = [
            TraceFrameLeafAssoc(trace_frame_id=2, leaf_id=1, trace_length=0),
            TraceFrameLeafAssoc(trace_frame_id=4, leaf_id=1, trace_length=0),
        ]
        with self.db.make_session() as session:
            self._add_to_session(session, trace_frames_run1)
            self._add_to_session(session, trace_frames_run2)
            self._add_to_session(session, runs)
            self._add_to_session(session, assocs)
            session.add(sink)
            session.commit()

            self.interactive.setup()
            self.interactive.sinks = {"sink1"}
            next_frames = self.interactive._next_forward_trace_frames(
                session, trace_frames_run2[0]
            )
            self.assertEqual(len(next_frames), 1)
            self.assertEqual(int(next_frames[0].id), int(trace_frames_run2[1].id))

    def testNextTraceFramesBackwards(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        trace_frames = self._basic_trace_frames()
        sink = SharedText(id=1, contents="sink1", kind=SharedTextKind.SINK)
        assocs = [
            TraceFrameLeafAssoc(trace_frame_id=1, leaf_id=1, trace_length=1),
            TraceFrameLeafAssoc(trace_frame_id=2, leaf_id=1, trace_length=1),
        ]

        trace_frames[0].callee = "call3"
        trace_frames[0].callee_port = "param1"
        trace_frames[1].callee = "call3"
        trace_frames[1].callee_port = "param1"

        with self.db.make_session() as session:
            self._add_to_session(session, trace_frames)
            self._add_to_session(session, assocs)
            session.add(run)
            session.add(sink)
            session.commit()

            self.interactive.setup()
            self.interactive.sinks = {"sink1"}
            next_frames = self.interactive._next_backward_trace_frames(
                session, trace_frames[1]
            )
            self.assertEqual(len(next_frames), 2)
            self.assertEqual(int(next_frames[0].id), int(trace_frames[0].id))

    def testNavigateTraceFrames(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        trace_frames = self._basic_trace_frames()
        sink = SharedText(id=1, contents="sink1", kind=SharedTextKind.SINK)
        assoc = TraceFrameLeafAssoc(trace_frame_id=2, leaf_id=1, trace_length=1)
        with self.db.make_session() as session:
            self._add_to_session(session, trace_frames)
            session.add(run)
            session.add(sink)
            session.add(assoc)
            session.commit()

            self.interactive.setup()
            self.interactive.sinks = {"sink1"}
            result = self.interactive._navigate_trace_frames(session, [trace_frames[0]])
            self.assertEqual(len(result), 2)
            self.assertEqual(int(result[0][0].id), 1)
            self.assertEqual(int(result[1][0].id), 2)

    def testCreateTraceTuples(self):
        # reverse order
        postcondition_traces = [
            (
                TraceFrame(
                    callee="call3",
                    callee_port="result",
                    filename="file3.py",
                    callee_location=SourceLocation(1, 1, 3),
                    caller="main",
                    caller_port="root",
                ),
                1,
            ),
            (
                TraceFrame(
                    callee="call2",
                    callee_port="result",
                    caller="dummy caller",
                    filename="file2.py",
                    callee_location=SourceLocation(1, 1, 2),
                ),
                2,
            ),
            (
                TraceFrame(
                    callee="leaf",
                    callee_port="source",
                    caller="dummy caller",
                    filename="file1.py",
                    callee_location=SourceLocation(1, 1, 1),
                ),
                3,
            ),
        ]
        trace_tuples = self.interactive._create_trace_tuples(postcondition_traces)
        self.assertEqual(len(trace_tuples), 3)
        self.assertEqual(
            trace_tuples,
            [
                TraceTuple(postcondition_traces[0][0], 1),
                TraceTuple(postcondition_traces[1][0], 2),
                TraceTuple(postcondition_traces[2][0], 3),
            ],
        )

    def testOutputTraceTuples(self):
        trace_tuples = [
            TraceTuple(
                trace_frame=TraceFrame(
                    callee="leaf",
                    callee_port="source",
                    filename="file1.py",
                    callee_location=SourceLocation(1, 1, 1),
                )
            ),
            TraceTuple(
                trace_frame=TraceFrame(
                    callee="call2",
                    callee_port="result",
                    filename="file2.py",
                    callee_location=SourceLocation(1, 1, 2),
                )
            ),
            TraceTuple(
                trace_frame=TraceFrame(
                    callee="call3",
                    callee_port="result",
                    filename="file3.py",
                    callee_location=SourceLocation(1, 1, 3),
                )
            ),
            TraceTuple(
                trace_frame=TraceFrame(
                    callee="main",
                    callee_port="root",
                    filename="file4.py",
                    callee_location=SourceLocation(1, 1, 4),
                )
            ),
            TraceTuple(
                trace_frame=TraceFrame(
                    callee="call4",
                    callee_port="param0",
                    filename="file4.py",
                    callee_location=SourceLocation(1, 1, 4),
                )
            ),
            TraceTuple(
                trace_frame=TraceFrame(
                    callee="call5",
                    callee_port="param1",
                    filename="file5.py",
                    callee_location=SourceLocation(1, 1, 5),
                )
            ),
            TraceTuple(
                trace_frame=TraceFrame(
                    callee="leaf",
                    callee_port="sink",
                    filename="file6.py",
                    callee_location=SourceLocation(1, 1, 6),
                )
            ),
        ]
        self.interactive.current_trace_frame_index = 1
        self.interactive._output_trace_tuples(trace_tuples)
        output = self.stdout.getvalue()
        self.assertEqual(
            output.split("\n"),
            [
                "     # ⎇  [callable] [port] [location]",
                "     1    leaf       source file1.py:1|1|1",
                " --> 2    call2      result file2.py:1|1|2",
                "     3    call3      result file3.py:1|1|3",
                "     4    main       root   file4.py:1|1|4",
                "     5    call4      param0 file4.py:1|1|4",
                "     6    call5      param1 file5.py:1|1|5",
                "     7    leaf       sink   file6.py:1|1|6",
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
                "     # ⎇  [callable] [port] [location]",
                "     1    leaf       source file1.py:1|1|1",
                "     2    call2      result file2.py:1|1|2",
                "     3    call3      result file3.py:1|1|3",
                "     4    main       root   file4.py:1|1|4",
                " --> 5    call4      param0 file4.py:1|1|4",
                "     6    call5      param1 file5.py:1|1|5",
                "     7    leaf       sink   file6.py:1|1|6",
                "",
            ],
        )

    def testTraceFromIssue(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        issue = self._generic_issue()
        issue_instance = self._generic_issue_instance()
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
            TraceFrameLeafAssoc(trace_frame_id=1, leaf_id=1),
            TraceFrameLeafAssoc(trace_frame_id=2, leaf_id=1),
        ]

        with self.db.make_session() as session:
            session.add(run)
            session.add(issue)
            session.add(issue_instance)
            self._add_to_session(session, trace_frames)
            self._add_to_session(session, assocs)
            session.commit()

        self.interactive.setup()
        self.interactive.trace()
        stderr = self.stderr.getvalue().strip()
        self.assertIn("Use 'set_issue(ID)' or 'set_frame(ID)'", stderr)

        self.interactive.set_issue(1)
        self.interactive.trace()
        output = self.stdout.getvalue().strip()
        self.assertIn("     1    leaf       source file.py:1|1|1", output)
        self.assertIn(" --> 2    call1      root   file.py:1|2|3", output)
        self.assertIn("     3    leaf       sink   file.py:1|1|2", output)

    def testTraceFromFrame(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        trace_frames = self._basic_trace_frames()
        shared_text = SharedText(id=1, contents="sink", kind=SharedTextKind.SINK)
        assocs = [
            TraceFrameLeafAssoc(trace_frame_id=1, leaf_id=1, trace_length=1),
            TraceFrameLeafAssoc(trace_frame_id=2, leaf_id=1, trace_length=0),
        ]

        with self.db.make_session() as session:
            self._add_to_session(session, trace_frames)
            self._add_to_session(session, assocs)
            session.add(shared_text)
            session.add(run)
            session.commit()

        self.interactive.setup()
        self.interactive.set_frame(1)

        self._clear_stdout()
        self.interactive.trace()
        self.assertEqual(self.interactive.sinks, {"sink"})
        self.assertEqual(
            self.stdout.getvalue().split("\n"),
            [
                "     # ⎇  [callable] [port] [location]",
                " --> 1    call1      root   file.py:1|1|1",
                "     2    call2      param0 file.py:1|1|1",
                "     3    leaf       sink   file.py:1|2|2",
                "",
            ],
        )

    def testTraceMissingFrames(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        issue = self._generic_issue()
        issue_instance = self._generic_issue_instance()
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
                callee="call2",
                callee_port="param0",
                callee_location=SourceLocation(1, 1, 1),
                filename="file.py",
                run_id=1,
            ),
        ]
        assocs = [
            IssueInstanceTraceFrameAssoc(trace_frame_id=1, issue_instance_id=1),
            IssueInstanceTraceFrameAssoc(trace_frame_id=2, issue_instance_id=1),
            TraceFrameLeafAssoc(trace_frame_id=1, leaf_id=1),
            TraceFrameLeafAssoc(trace_frame_id=2, leaf_id=1),
        ]

        with self.db.make_session() as session:
            session.add(run)
            session.add(issue)
            session.add(issue_instance)
            self._add_to_session(session, trace_frames)
            self._add_to_session(session, assocs)
            session.commit()

        self.interactive.setup()
        self.interactive.set_issue(1)
        self.interactive.trace()
        stdout = self.stdout.getvalue().strip()
        self.assertIn("Missing trace frame: call2:param0", stdout)

    def testTraceCursorLocation(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        issue = self._generic_issue()
        issue_instance = self._generic_issue_instance()
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
            TraceFrameLeafAssoc(trace_frame_id=1, leaf_id=1),
            TraceFrameLeafAssoc(trace_frame_id=2, leaf_id=1),
        ]
        with self.db.make_session() as session:
            session.add(run)
            session.add(issue)
            session.add(issue_instance)
            self._add_to_session(session, trace_frames)
            self._add_to_session(session, assocs)
            session.commit()

        self.interactive.setup()
        self.interactive.set_issue(1)
        self.assertEqual(self.interactive.current_trace_frame_index, 1)
        self.interactive.next_cursor_location()
        self.assertEqual(self.interactive.current_trace_frame_index, 2)
        self.interactive.next_cursor_location()
        self.assertEqual(self.interactive.current_trace_frame_index, 2)
        self.interactive.prev_cursor_location()
        self.assertEqual(self.interactive.current_trace_frame_index, 1)
        self.interactive.prev_cursor_location()
        self.assertEqual(self.interactive.current_trace_frame_index, 0)
        self.interactive.prev_cursor_location()
        self.assertEqual(self.interactive.current_trace_frame_index, 0)

    def testJumpToLocation(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        issue = self._generic_issue()
        issue_instance = self._generic_issue_instance()
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
            TraceFrameLeafAssoc(trace_frame_id=1, leaf_id=1),
            TraceFrameLeafAssoc(trace_frame_id=2, leaf_id=1),
        ]
        with self.db.make_session() as session:
            session.add(run)
            session.add(issue)
            session.add(issue_instance)
            self._add_to_session(session, trace_frames)
            self._add_to_session(session, assocs)
            session.commit()

        self.interactive.setup()
        self.interactive.set_issue(1)
        self.assertEqual(self.interactive.current_trace_frame_index, 1)

        self.interactive.jump(1)
        self.assertEqual(self.interactive.current_trace_frame_index, 0)

        self.interactive.jump(3)
        self.assertEqual(self.interactive.current_trace_frame_index, 2)

        self.interactive.jump(4)
        self.assertEqual(self.interactive.current_trace_frame_index, 2)

        self.interactive.jump(0)
        self.assertEqual(self.interactive.current_trace_frame_index, 2)

    def testTraceNoSinks(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        issue = self._generic_issue()
        issue_instance = self._generic_issue_instance()
        trace_frame = TraceFrame(
            id=1,
            kind=TraceKind.POSTCONDITION,
            caller="call1",
            caller_port="root",
            callee="leaf",
            callee_port="source",
            callee_location=SourceLocation(1, 1),
            filename="file.py",
            run_id=1,
        )
        source = SharedText(id=1, contents="source1", kind=SharedTextKind.SOURCE)
        assocs = [
            IssueInstanceTraceFrameAssoc(trace_frame_id=1, issue_instance_id=1),
            TraceFrameLeafAssoc(trace_frame_id=1, leaf_id=1),
        ]
        with self.db.make_session() as session:
            session.add(run)
            session.add(issue)
            session.add(issue_instance)
            session.add(trace_frame)
            session.add(source)
            self._add_to_session(session, assocs)
            session.commit()

        self.interactive.setup()
        self.interactive.sources = {"source1"}
        self.interactive.set_issue(1)
        self._clear_stdout()
        self.interactive.trace()
        self.assertEqual(
            self.stdout.getvalue().split("\n"),
            [
                "     # ⎇  [callable] [port] [location]",
                "     1    leaf       source file.py:1|1|1",
                " --> 2    call1      root   file.py:1|2|3",
                "",
            ],
        )

    def _set_up_branched_trace(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        issue = self._generic_issue()
        issue_instance = self._generic_issue_instance()
        messages = [
            SharedText(id=1, contents="source1", kind=SharedTextKind.SOURCE),
            SharedText(id=2, contents="sink1", kind=SharedTextKind.SINK),
        ]
        trace_frames = []
        assocs = [
            IssueInstanceSharedTextAssoc(issue_instance_id=1, shared_text_id=1),
            IssueInstanceSharedTextAssoc(issue_instance_id=1, shared_text_id=2),
        ]
        for i in range(6):
            trace_frames.append(
                TraceFrame(
                    id=i + 1,
                    caller="call1",
                    caller_port="root",
                    filename="file.py",
                    callee_location=SourceLocation(i, i, i),
                    run_id=1,
                )
            )
            if i < 2:  # 2 postconditions
                trace_frames[i].kind = TraceKind.POSTCONDITION
                trace_frames[i].callee = "leaf"
                trace_frames[i].callee_port = "source"
                assocs.append(
                    TraceFrameLeafAssoc(trace_frame_id=i + 1, leaf_id=1, trace_length=0)
                )
                assocs.append(
                    IssueInstanceTraceFrameAssoc(
                        trace_frame_id=i + 1, issue_instance_id=1
                    )
                )
            elif i < 4:
                trace_frames[i].kind = TraceKind.PRECONDITION
                trace_frames[i].callee = "call2"
                trace_frames[i].callee_port = "param2"
                assocs.append(
                    TraceFrameLeafAssoc(trace_frame_id=i + 1, leaf_id=2, trace_length=1)
                )
                assocs.append(
                    IssueInstanceTraceFrameAssoc(
                        trace_frame_id=i + 1, issue_instance_id=1
                    )
                )
            else:
                trace_frames[i].kind = TraceKind.PRECONDITION
                trace_frames[i].caller = "call2"
                trace_frames[i].caller_port = "param2"
                trace_frames[i].callee = "leaf"
                trace_frames[i].callee_port = "sink"
                assocs.append(
                    TraceFrameLeafAssoc(trace_frame_id=i + 1, leaf_id=2, trace_length=0)
                )

        with self.db.make_session() as session:
            session.add(run)
            session.add(issue)
            session.add(issue_instance)
            self._add_to_session(session, messages)
            self._add_to_session(session, trace_frames)
            self._add_to_session(session, assocs)
            session.commit()

    def testTraceBranchNumber(self):
        self._set_up_branched_trace()

        self.interactive.setup()
        self.interactive.set_issue(1)

        self.assertEqual(self.interactive.sources, {"source1"})
        self.assertEqual(self.interactive.sinks, {"sink1"})

        self.interactive.trace()
        output = self.stdout.getvalue().strip()
        self.assertIn("     1 +2 leaf       source file.py:0|0|0", output)
        self.assertIn(" --> 2    call1      root   file.py:1|2|3", output)
        self.assertIn("     3 +2 call2      param2 file.py:2|2|2", output)
        self.assertIn("     4 +2 leaf       sink   file.py:4|4|4", output)

    def testExpand(self):
        self._set_up_branched_trace()

        self.interactive.setup()
        self.interactive.set_issue(1)
        # Parent at root
        self.interactive.prev_cursor_location()
        self.interactive.expand()
        output = self.stdout.getvalue().strip()
        self.assertIn(
            "[*] leaf : source\n        [0 hops: source1]\n        [file.py:0|0|0]",
            output,
        )
        self.assertIn(
            "[2] leaf : source\n        [0 hops: source1]\n        [file.py:1|1|1]",
            output,
        )

        self._clear_stdout()
        # Move to call2:param2
        self.interactive.next_cursor_location()
        self.interactive.next_cursor_location()
        self.interactive.expand()
        output = self.stdout.getvalue().strip()
        self.assertIn(
            "[*] call2 : param2\n        [1 hops: sink1]\n        [file.py:2|2|2]",
            output,
        )
        self.assertIn(
            "[2] call2 : param2\n        [1 hops: sink1]\n        [file.py:3|3|3]",
            output,
        )

        self._clear_stdout()
        # Move to leaf:sink
        self.interactive.next_cursor_location()
        self.interactive.expand()
        output = self.stdout.getvalue().strip()
        self.assertIn(
            "[*] leaf : sink\n        [0 hops: sink1]\n        [file.py:4|4|4]", output
        )
        self.assertIn(
            "[2] leaf : sink\n        [0 hops: sink1]\n        [file.py:5|5|5]", output
        )

    def testGetTraceFrameBranches(self):
        self._set_up_branched_trace()

        self.interactive.setup()
        self.interactive.set_issue(1)
        # Parent at root
        self.interactive.prev_cursor_location()

        with self.db.make_session() as session:
            branches = self.interactive._get_trace_frame_branches(session)
            self.assertEqual(len(branches), 2)
            self.assertEqual(int(branches[0].id), 1)
            self.assertEqual(int(branches[1].id), 2)

            # Parent is no longer root
            self.interactive.next_cursor_location()
            self.interactive.next_cursor_location()
            self.interactive.next_cursor_location()

            branches = self.interactive._get_trace_frame_branches(session)
            self.assertEqual(len(branches), 2)
            self.assertEqual(int(branches[0].id), 5)
            self.assertEqual(int(branches[1].id), 6)

    def testBranch(self):
        self._set_up_branched_trace()

        self.interactive.setup()
        self.interactive.set_issue(1)
        self.interactive.prev_cursor_location()

        # We are testing for the source location, which differs between branches
        self._clear_stdout()
        self.interactive.branch(2)  # location 0|0|0 -> 1|1|1
        output = self.stdout.getvalue().strip()
        self.assertIn(" --> 1 +2 leaf       source file.py:1|1|1", output)

        self._clear_stdout()
        self.interactive.branch(1)  # location 1|1|1 -> 0|0|0
        output = self.stdout.getvalue().strip()
        self.assertIn(" --> 1 +2 leaf       source file.py:0|0|0", output)

        self.interactive.next_cursor_location()
        self.interactive.next_cursor_location()

        self._clear_stdout()
        self.interactive.branch(2)  # location 2|2|2 -> 3|3|3
        output = self.stdout.getvalue().strip()
        self.assertIn(" --> 3 +2 call2      param2 file.py:3|3|3", output)

        self.interactive.next_cursor_location()

        self._clear_stdout()
        self.interactive.branch(2)  # location 4|4|4 -> 5|5|5
        output = self.stdout.getvalue().strip()
        self.assertIn("     3 +2 call2      param2 file.py:3|3|3", output)
        self.assertIn(" --> 4 +2 leaf       sink   file.py:5|5|5", output)

        self.interactive.branch(3)  # location 4|4|4 -> 5|5|5
        stderr = self.stderr.getvalue().strip()
        self.assertIn("out of bounds", stderr)

    def testBranchPrefixLengthChanges(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        issue = self._generic_issue()
        issue_instance = self._generic_issue_instance()
        messages = [
            SharedText(id=1, contents="source1", kind=SharedTextKind.SOURCE),
            SharedText(id=2, contents="sink1", kind=SharedTextKind.SINK),
        ]
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
                kind=TraceKind.POSTCONDITION,
                caller="call1",
                caller_port="root",
                callee="prev_call",
                callee_port="result",
                callee_location=SourceLocation(1, 1),
                filename="file.py",
                run_id=1,
            ),
            TraceFrame(
                id=3,
                kind=TraceKind.POSTCONDITION,
                caller="prev_call",
                caller_port="result",
                callee="leaf",
                callee_port="source",
                callee_location=SourceLocation(1, 1),
                filename="file.py",
                run_id=1,
            ),
            TraceFrame(
                id=4,
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
            IssueInstanceSharedTextAssoc(issue_instance_id=1, shared_text_id=1),
            IssueInstanceSharedTextAssoc(issue_instance_id=1, shared_text_id=2),
            IssueInstanceTraceFrameAssoc(issue_instance_id=1, trace_frame_id=1),
            IssueInstanceTraceFrameAssoc(issue_instance_id=1, trace_frame_id=2),
            IssueInstanceTraceFrameAssoc(issue_instance_id=1, trace_frame_id=4),
            TraceFrameLeafAssoc(trace_frame_id=1, leaf_id=1, trace_length=0),
            TraceFrameLeafAssoc(trace_frame_id=2, leaf_id=1, trace_length=1),
            TraceFrameLeafAssoc(trace_frame_id=3, leaf_id=1, trace_length=0),
            TraceFrameLeafAssoc(trace_frame_id=4, leaf_id=2, trace_length=0),
        ]
        with self.db.make_session() as session:
            session.add(run)
            session.add(issue)
            session.add(issue_instance)
            self._add_to_session(session, messages)
            self._add_to_session(session, trace_frames)
            self._add_to_session(session, assocs)
            session.commit()

        self.interactive.setup()
        self.interactive.set_issue(1)

        self._clear_stdout()
        self.interactive.prev_cursor_location()
        self.assertEqual(
            self.stdout.getvalue().split("\n"),
            [
                "     # ⎇  [callable] [port] [location]",
                " --> 1 +2 leaf       source file.py:1|1|1",
                "     2    call1      root   file.py:1|2|3",
                "     3    leaf       sink   file.py:1|2|2",
                "",
            ],
        )

        self._clear_stdout()
        self.interactive.branch(2)
        self.assertEqual(
            self.stdout.getvalue().split("\n"),
            [
                "     # ⎇  [callable] [port] [location]",
                "     1    leaf       source file.py:1|1|1",
                " --> 2 +2 prev_call  result file.py:1|1|1",
                "     3    call1      root   file.py:1|2|3",
                "     4    leaf       sink   file.py:1|2|2",
                "",
            ],
        )

        self._clear_stdout()
        self.interactive.expand()
        output = self.stdout.getvalue().strip()
        self.assertIn("[*] prev_call : result", output)
        self.assertIn("        [1 hops: source1]", output)

    def testCurrentBranchIndex(self):
        trace_frames = [TraceFrame(id=1), TraceFrame(id=2), TraceFrame(id=3)]

        self.interactive.current_trace_frame_index = 0
        self.interactive.trace_tuples = [TraceTuple(trace_frame=TraceFrame(id=1))]

        self.assertEqual(0, self.interactive._current_branch_index(trace_frames))
        self.interactive.trace_tuples[0].trace_frame.id = 2
        self.assertEqual(1, self.interactive._current_branch_index(trace_frames))
        self.interactive.trace_tuples[0].trace_frame.id = 3
        self.assertEqual(2, self.interactive._current_branch_index(trace_frames))

        self.interactive.trace_tuples[0].trace_frame.id = 4
        self.assertEqual(-1, self.interactive._current_branch_index(trace_frames))

    def testVerifyEntrypointSelected(self):
        self.interactive.current_issue_id = -1
        self.interactive.current_frame_id = -1
        with self.assertRaises(UserError):
            self.interactive._verify_entrypoint_selected()

        self.interactive.current_issue_id = 1
        try:
            self.interactive._verify_entrypoint_selected()
        except UserError:
            self.fail("Unexpected UserError")

        self.interactive.current_issue_id = -1
        self.interactive.current_frame_id = 1
        try:
            self.interactive._verify_entrypoint_selected()
        except UserError:
            self.fail("Unexpected UserError")

        self.interactive.current_issue_id = 1
        with self.assertRaises(AssertionError):
            self.interactive._verify_entrypoint_selected()

    def testVerifyMultipleBranches(self):
        self.interactive.current_trace_frame_index = 0
        self.interactive.trace_tuples = [
            TraceTuple(trace_frame=TraceFrame(id=1), branches=1),
            TraceTuple(trace_frame=TraceFrame(id=2), branches=2),
        ]
        with self.assertRaises(UserError):
            self.interactive._verify_multiple_branches()

        self.interactive.current_trace_frame_index = 1
        try:
            self.interactive._verify_multiple_branches()
        except UserError:
            self.fail("Unexpected UserError")

    def testVerifyListFilter(self):
        with self.assertRaises(UserError):
            self.interactive._verify_list_filter("not a list", "arg0")

        with self.assertRaises(UserError):
            self.interactive._verify_list_filter([], "arg0")

        try:
            self.interactive._verify_list_filter(["elem", "elem"], "arg0")
        except UserError:
            self.fail("Unexpected UserError")

    def testAddListFilterToQuery(self):
        shared_texts = [
            SharedText(id=1, contents="prefix"),
            SharedText(id=2, contents="suffix"),
            SharedText(id=3, contents="prefix_suffix"),
            SharedText(id=4, contents="fix"),
        ]

        with self.db.make_session() as session:
            self._add_to_session(session, shared_texts)
            session.commit()

            query = session.query(SharedText.contents)
            self.assertEqual(
                self.interactive._add_list_filter_to_query(
                    ["prefix", "suffix"], query, SharedText.contents
                ).all(),
                [("prefix",), ("suffix",)],
            )
            self.assertEqual(
                self.interactive._add_list_filter_to_query(
                    ["%prefix%"], query, SharedText.contents
                ).all(),
                [("prefix",), ("prefix_suffix",)],
            )
            self.assertEqual(
                self.interactive._add_list_filter_to_query(
                    ["%fix%"], query, SharedText.contents
                ).all(),
                [("prefix",), ("suffix",), ("prefix_suffix",), ("fix",)],
            )

    def testCreateIssueOutputStringNoSourcesNoSinks(self):
        issue = Issue(code=1000, callable="module.function1")
        issue_instance = IssueInstance(
            id=1,
            message=SharedText(contents="leaf"),
            filename="module.py",
            location=SourceLocation(1, 2, 3),
        )
        sources = []
        sinks = ["sink1", "sink2"]
        result = self.interactive._create_issue_output_string(
            issue_instance, issue, sources, sinks
        )
        self.assertIn("Sources: No sources", result)
        self.assertIn("Sinks: sink1", result)

        sources = ["source1", "source2"]
        sinks = []
        result = self.interactive._create_issue_output_string(
            issue_instance, issue, sources, sinks
        )
        self.assertIn("Sources: source1", result)
        self.assertIn("Sinks: No sinks", result)

    def testListSourceCode(self):
        mock_data = """if this_is_true:
    print("This was true")
else:
    print("This was false")
        """
        self.interactive.setup()
        self.interactive.current_issue_id = 1

        self.interactive.current_trace_frame_index = 0
        self.interactive.trace_tuples = [
            TraceTuple(
                trace_frame=TraceFrame(
                    filename="file.py",
                    callee="callee",
                    callee_location=SourceLocation(2, 1, 1),
                )
            )
        ]
        with patch("builtins.open", mock_open(read_data=mock_data)) as mock_file:
            self._clear_stdout()
            self.interactive.list_source_code(2)
            mock_file.assert_called_once_with(f"{os.getcwd()}/file.py", "r")
            output = self.stdout.getvalue()
            self.assertEqual(
                output.split("\n"),
                [
                    "In callee [file.py:2|1|1]",
                    "     1  if this_is_true:",
                    ' --> 2      print("This was true")',
                    "     3  else:",
                    '     4      print("This was false")',
                    "",
                ],
            )

            mock_file.reset_mock()
            self._clear_stdout()
            self.interactive.list_source_code(1)
            mock_file.assert_called_once_with(f"{os.getcwd()}/file.py", "r")
            output = self.stdout.getvalue()
            self.assertEqual(
                output.split("\n"),
                [
                    "In callee [file.py:2|1|1]",
                    "     1  if this_is_true:",
                    ' --> 2      print("This was true")',
                    "     3  else:",
                    "",
                ],
            )

    def testListSourceCodeFileNotFound(self):
        self.interactive.setup()
        self.interactive.current_issue_id = 1

        self.interactive.current_trace_frame_index = 0
        self.interactive.trace_tuples = [
            TraceTuple(
                trace_frame=TraceFrame(
                    filename="file.py", callee_location=SourceLocation(2, 1, 1)
                )
            )
        ]
        with patch("builtins.open", mock_open(read_data="not read")) as mock_file:
            mock_file.side_effect = FileNotFoundError()
            self.interactive.list_source_code()
            self.assertIn("Couldn't open", self.stderr.getvalue())
            self.assertNotIn("file.py", self.stdout.getvalue())

    def testGroupTraceFrames(self):
        trace_frames = [
            TraceFrame(id=1, caller="caller1", caller_port="port1"),
            TraceFrame(id=2, caller="caller1", caller_port="port1"),
            TraceFrame(id=3, caller="caller2", caller_port="port2"),
            TraceFrame(id=4, caller="caller2", caller_port="port2"),
            TraceFrame(id=5, caller="caller2", caller_port="port3"),
        ]

        buckets = self.interactive._group_trace_frames(trace_frames)

        self.assertEqual(3, len(buckets.keys()))
        self.assertIn(("caller1", "port1"), buckets.keys())
        self.assertIn(("caller2", "port2"), buckets.keys())
        self.assertIn(("caller2", "port3"), buckets.keys())

        self.assertEqual(
            [1, 2], [int(frame.id) for frame in buckets[("caller1", "port1")]]
        )
        self.assertEqual(
            [3, 4], [int(frame.id) for frame in buckets[("caller2", "port2")]]
        )
        self.assertEqual(
            [5], [int(frame.id) for frame in buckets[("caller2", "port3")]]
        )

    def testListTracesBasic(self):
        trace_frames = [
            TraceFrame(
                id=1,
                caller="caller1",
                caller_port="port1",
                callee="callee1",
                callee_port="port1",
                callee_location=SourceLocation(1, 1, 1),
                filename="file.py",
                run_id=1,
                kind=TraceKind.POSTCONDITION,
            ),
            TraceFrame(
                id=2,
                caller="caller1",
                caller_port="port1",
                callee="callee2",
                callee_port="port2",
                callee_location=SourceLocation(1, 1, 1),
                filename="file.py",
                run_id=1,
                kind=TraceKind.POSTCONDITION,
            ),
            TraceFrame(
                id=3,
                caller="caller2",
                caller_port="port2",
                callee="callee3",
                callee_port="port3",
                callee_location=SourceLocation(1, 1, 1),
                filename="file.py",
                run_id=1,
                kind=TraceKind.POSTCONDITION,
            ),
            TraceFrame(
                id=4,
                caller="caller2",
                caller_port="port2",
                callee="callee4",
                callee_port="port4",
                callee_location=SourceLocation(1, 1, 1),
                filename="file.py",
                run_id=1,
                kind=TraceKind.POSTCONDITION,
            ),
            TraceFrame(
                id=5,
                caller="caller2",
                caller_port="port3",
                callee="callee5",
                callee_port="port5",
                callee_location=SourceLocation(1, 1, 1),
                filename="file.py",
                run_id=1,
                kind=TraceKind.POSTCONDITION,
            ),
        ]

        with self.db.make_session() as session:
            self._add_to_session(session, trace_frames)
            session.commit()

        self.interactive.current_run_id = 1
        self._clear_stdout()
        self.interactive.frames(kind=TraceKind.POSTCONDITION)
        self.assertEqual(
            self.stdout.getvalue().split("\n"),
            [
                "[id] [caller:caller_port -> callee:callee_port]",
                "---- caller1:port1 ->",
                "1        callee1:port1",
                "2        callee2:port2",
                "---- caller2:port2 ->",
                "3        callee3:port3",
                "4        callee4:port4",
                "---- caller2:port3 ->",
                "5        callee5:port5",
                "",
            ],
        )

        self._clear_stdout()
        self.interactive.frames(kind=TraceKind.PRECONDITION)
        self.assertEqual(self.stdout.getvalue().strip(), "No trace frames found.")

    def testSetFrame(self):
        trace_frames = self._basic_trace_frames()
        shared_text = SharedText(id=1, contents="sink", kind=SharedTextKind.SINK)
        assocs = [
            TraceFrameLeafAssoc(trace_frame_id=1, leaf_id=1, trace_length=1),
            TraceFrameLeafAssoc(trace_frame_id=2, leaf_id=1, trace_length=0),
        ]

        with self.db.make_session() as session:
            self._add_to_session(session, trace_frames)
            self._add_to_session(session, assocs)
            session.add(shared_text)
            session.commit()

        self.interactive.setup()

        self.interactive.set_frame(0)
        self.assertIn("Trace frame 0 doesn't exist.", self.stderr.getvalue())

        self._clear_stdout()
        self.interactive.set_frame(1)
        self.assertIn("Trace frame 1", self.stdout.getvalue())
        self.assertNotIn("Trace frame 2", self.stdout.getvalue())

        self._clear_stdout()
        self.interactive.set_frame(2)
        self.assertNotIn("Trace frame 1", self.stdout.getvalue())
        self.assertIn("Trace frame 2", self.stdout.getvalue())

    def testIsBeforeRoot(self):
        self.interactive.trace_tuples = [
            TraceTuple(trace_frame=TraceFrame(kind=TraceKind.POSTCONDITION)),
            TraceTuple(trace_frame=TraceFrame(kind=TraceKind.PRECONDITION)),
        ]

        self.interactive.current_trace_frame_index = 0
        self.assertTrue(self.interactive._is_before_root())

        self.interactive.current_trace_frame_index = 1
        self.assertFalse(self.interactive._is_before_root())

    def testIsRootTraceTuple(self):
        trace_tuple = TraceTuple(trace_frame=TraceFrame(callee_port="root"))
        self.assertTrue(self.interactive._is_root_trace_tuple(trace_tuple))

        trace_tuple = TraceTuple(trace_frame=TraceFrame(callee_port="not_root"))
        self.assertFalse(self.interactive._is_root_trace_tuple(trace_tuple))

    def testParents(self):
        self._set_up_branched_trace()
        self.interactive.setup()

        self.interactive.set_frame(3)
        self.interactive.current_trace_frame_index = 1

        self._clear_stdout()
        with patch("click.prompt", return_value=0):
            self.interactive.parents()
        self.assertEqual(
            self.stdout.getvalue().split("\n"),
            ["[1] call1 : root", "[2] call1 : root", ""],
        )

        self._clear_stdout()
        self.interactive.current_trace_frame_index = 0
        self.interactive.parents()
        self.assertIn("No parents calling", self.stdout.getvalue())

        self.interactive.current_trace_frame_index = 2
        self.interactive.parents()
        self.assertIn("Try running from a non-leaf node", self.stderr.getvalue())

    def testParentsSelectParent(self):
        self._set_up_branched_trace()
        self.interactive.setup()

        self.interactive.set_frame(3)
        self.interactive.current_trace_frame_index = 1

        self._clear_stdout()
        with patch("click.prompt", return_value=1):
            self.interactive.parents()
        self.assertEqual(
            self.stdout.getvalue().split("\n"),
            [
                "[1] call1 : root",
                "[2] call1 : root",
                "",
                "     # ⎇  [callable] [port] [location]",
                " --> 1    call1      root   file.py:2|2|2",
                "     2    call2      param2 file.py:2|2|2",
                "     3 +2 leaf       sink   file.py:4|4|4",
                "",
            ],
        )

    def testUpdateTraceTuplesNewParent(self):
        self.interactive.setup()
        # Test postcondition
        self.interactive.current_trace_frame_index = 2
        self.interactive.trace_tuples = [
            TraceTuple(TraceFrame(callee="A")),
            TraceTuple(TraceFrame(callee="B")),
            TraceTuple(TraceFrame(callee="C")),
            TraceTuple(TraceFrame(callee="D")),
            TraceTuple(TraceFrame(callee="E")),
        ]

        trace_frame = TraceFrame(
            caller="caller",
            caller_port="caller_port",
            callee="F",
            filename="file.py",
            callee_location=SourceLocation(1, 1, 1),
            kind=TraceKind.POSTCONDITION,
        )
        self.interactive._update_trace_tuples_new_parent(trace_frame)
        self.assertEqual(self.interactive.current_trace_frame_index, 3)
        self.assertEqual(
            [
                trace_tuple.trace_frame.callee
                for trace_tuple in self.interactive.trace_tuples
            ],
            ["A", "B", "F", "caller"],
        )

        # Test precondition
        self.interactive.current_trace_frame_index = 2
        self.interactive.trace_tuples = [
            TraceTuple(TraceFrame(callee="A")),
            TraceTuple(TraceFrame(callee="B")),
            TraceTuple(TraceFrame(callee="C")),
            TraceTuple(TraceFrame(callee="D")),
            TraceTuple(TraceFrame(callee="E")),
        ]
        trace_frame.kind = TraceKind.PRECONDITION
        self.interactive._update_trace_tuples_new_parent(trace_frame)
        self.assertEqual(self.interactive.current_trace_frame_index, 0)
        self.assertEqual(
            [
                trace_tuple.trace_frame.callee
                for trace_tuple in self.interactive.trace_tuples
            ],
            ["caller", "F", "D", "E"],
        )

    def mock_pager(self, output_string):
        self.pager_calls += 1

    def testPager(self):
        run = Run(id=1, date=datetime.now(), status=RunStatus.FINISHED)
        issue = self._generic_issue()
        issue_instance = self._generic_issue_instance()

        with self.db.make_session() as session:
            session.add(run)
            session.add(issue)
            session.add(issue_instance)
            session.commit()

        # Default is no pager in tests
        self.pager_calls = 0
        with patch("IPython.core.page.page", self.mock_pager):
            self.interactive.setup()
            self.interactive.issues()
            self.interactive.runs()
        self.assertEqual(self.pager_calls, 0)

        self.pager_calls = 0
        with patch("IPython.core.page.page", self.mock_pager):
            self.interactive.setup()
            self.interactive.issues(use_pager=True)
            self.interactive.runs(use_pager=True)
        self.assertEqual(self.pager_calls, 2)
