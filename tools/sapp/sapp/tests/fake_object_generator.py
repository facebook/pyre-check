#!/usr/bin/env python3

import datetime
from typing import Optional

from ..bulk_saver import BulkSaver
from ..models import (
    DBID,
    Issue,
    IssueDBID,
    IssueInstance,
    IssueInstanceFixInfo,
    Run,
    SharedText,
    SharedTextKind,
    SourceLocation,
    TraceFrame,
    TraceKind,
)
from ..trace_graph import TraceGraph


class FakeObjectGenerator:
    def __init__(self, graph: Optional[TraceGraph] = None, run_id=0):
        self.reinit(run_id)
        self.graph = graph

    def reinit(self, run_id):
        self.saver = BulkSaver()
        self.handle = 0
        self.source_name_id = 0
        self.sink_name_id = 0
        self.shared_text_name_id = 0
        self.run_id = run_id

    def save_all(self, db):
        if self.graph:
            self.graph.update_bulk_saver(self.saver)
        self.saver.save_all(db)
        self.saver = BulkSaver()

    def issue(
        self,
        filename="/r/lib/server/posts/request.py",
        callable="lib.server.posts.request.new_post_request",
        handle=None,
    ):
        self.handle += 1
        now = datetime.datetime.now()
        result = Issue.Record(
            id=IssueDBID(),
            handle=str(self.handle) if not handle else handle,
            callable_line=1,
            message="Tainted information may eventually flow...",
            code=6015 + self.handle,
            filename=filename,
            callable=callable,
            first_seen=now,
            last_seen=now,
            line=1,
            start=1,
            end=2,
        )
        if self.graph:
            self.graph.add_issue(result)
        else:
            self.saver.add(result)
        return result

    def precondition(
        self,
        caller="double_meh",
        caller_port="at the end of universe",
        callee="triple_meh",
        callee_port="at the beginning of time",
        filename="lib/server/posts/request.py",
        location=(4, 5, 6),
    ):
        filename_record = self.filename(filename)
        caller_record = self.callable(caller)
        callee_record = self.callable(callee)
        trace_frame = TraceFrame.Record(
            id=DBID(),
            kind=TraceKind.PRECONDITION,
            caller_id=caller_record.id,
            caller_port=caller_port,
            callee_id=callee_record.id,
            callee_port=callee_port,
            callee_location=SourceLocation(location[0], location[1], location[2]),
            filename_id=filename_record.id,
            titos=[],
            run_id=self.run_id,
            type_interval_lower=5,
            type_interval_upper=7,
            preserves_type_context=False,
            migrated_id=None,
        )
        if self.graph:
            self.graph.add_trace_frame(trace_frame)
        else:
            self.saver.add(trace_frame)
        return trace_frame

    def postcondition(
        self,
        caller="quadruple_meh",
        caller_port="caller_meh",
        callee="quintuple_meh",
        callee_port="callee_meh",
        filename="lib/server/posts/response.py",
        location=(4, 5, 6),
    ):
        filename_record = self.filename(filename)
        caller_record = self.callable(caller)
        callee_record = self.callable(callee)
        trace_frame = TraceFrame.Record(
            id=DBID(),
            kind=TraceKind.POSTCONDITION,
            caller_id=caller_record.id,
            caller_port=caller_port,
            callee_id=callee_record.id,
            callee_port=callee_port,
            callee_location=SourceLocation(location[0], location[1], location[2]),
            filename_id=filename_record.id,
            titos=[],
            run_id=self.run_id,
            type_interval_lower=5,
            type_interval_upper=7,
            preserves_type_context=False,
            migrated_id=None,
        )
        if self.graph:
            self.graph.add_trace_frame(trace_frame)
        else:
            self.saver.add(trace_frame)
        return trace_frame

    def shared_text(self, contents, kind):
        if self.graph:
            shared_text = self.graph.get_shared_text(kind, contents)
            if shared_text is not None:
                return shared_text

        result = SharedText.Record(id=DBID(), contents=contents, kind=kind)
        if self.graph:
            self.graph.add_shared_text(result)
        else:
            self.saver.add(result)
        return result

    def run(self, differential_id=None, job_id=None, kind=None):
        self.run_id += 1
        # Not added to bulksaver or graph
        return Run(
            id=DBID(self.run_id),
            date=datetime.datetime.now(),
            hh_version="1234567890",
            revision_id=12345,
            differential_id=differential_id,
            job_id=job_id,
            kind=kind,
        )

    def feature(self, name="via:feature"):
        return self.shared_text(contents=name, kind=SharedTextKind.FEATURE)

    def source(self, name="source"):
        return self.shared_text(contents=name, kind=SharedTextKind.SOURCE)

    def sink(self, name="sink"):
        return self.shared_text(contents=name, kind=SharedTextKind.SINK)

    def filename(self, name="/r/some/filename.py"):
        return self.shared_text(contents=name, kind=SharedTextKind.FILENAME)

    def callable(self, name="Foo.barMethod"):
        return self.shared_text(contents=name, kind=SharedTextKind.CALLABLE)

    def message(self, name="this is bad"):
        return self.shared_text(contents=name, kind=SharedTextKind.MESSAGE)

    def instance(
        self,
        message="this is bad",
        filename="/r/some/filename.py",
        callable="Foo.barMethod",
        issue_id=None,
    ):
        issue_id = issue_id if issue_id is not None else DBID(1)
        filename = self.filename(filename)
        message = self.message(message)
        callable = self.callable(callable)
        result = IssueInstance.Record(
            id=DBID(),
            location=SourceLocation(6, 7, 8),
            filename_id=filename.id,
            message_id=message.id,
            callable_id=callable.id,
            run_id=self.run_id,
            issue_id=issue_id,
        )
        if self.graph:
            self.graph.add_issue_instance(result)
        else:
            self.saver.add(result)
        return result

    def fix_info(self):
        result = IssueInstanceFixInfo.Record(id=DBID(), fix_info="fixthis")
        if self.graph:
            self.graph.add_fix_info(result)
        else:
            self.saver.add(result)
        return result
