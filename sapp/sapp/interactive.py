#!/usr/bin/env python3

import os
import sys
from typing import List, NamedTuple, Optional

import IPython
from IPython.core import page
from sapp.db import DB
from sapp.models import (
    Issue,
    IssueInstance,
    Postcondition,
    Precondition,
    Run,
    RunStatus,
    Sink,
    Source,
    SourceLocation,
    TraceFrame,
    TraceKind,
)
from sqlalchemy.orm import joinedload
from sqlalchemy.sql import func
from sqlalchemy.sql.expression import or_


class Interactive:
    help_message = """
Commands =======================================================================

runs()          list all completed runs
issues()        list all for the selected run

set_run(ID)     select a specific run
set_issue(ID)   select a specific issue

show()          show info about selected issue

commands()      show this message
help(COMAMND)   more info about a command
    """
    welcome_message = "Interactive issue exploration. Type 'commands()' for help."

    LEAF_NAMES = {"source", "sink", "leaf"}

    def __init__(self, database, database_name):
        self.db = DB(database, database_name, assertions=True)
        self.scope_vars = {
            "commands": self.help,
            "runs": self.runs,
            "issues": self.issues,
            "set_run": self.set_run,
            "set_issue": self.set_issue,
            "show": self.show,
            "trace": self.trace,
        }

    def start_repl(self):
        with self.db.make_session() as session:
            latest_run_id = (
                session.query(func.max(Run.id))
                .filter(Run.status == RunStatus.FINISHED)
                .scalar()
            )

        if latest_run_id.resolved() is None:
            self.warning(
                "No runs found. "
                f"Try running '{os.path.basename(sys.argv[0])} analyze' first."
            )
            sys.exit(1)

        self.current_run_id = latest_run_id
        self.current_issue_id = None
        # Tuples representing the trace of the current issue
        self.trace_tuples: List[TraceTuple] = []
        # Active trace frame of the current trace
        self.current_trace_frame_index = None
        # The current issue id when 'trace' was last run
        self.trace_tuples_id = None

        print("=" * len(self.welcome_message))
        print(self.welcome_message)
        print("=" * len(self.welcome_message))
        IPython.start_ipython(argv=[], user_ns=self.scope_vars)

    def help(self):
        print(self.help_message)
        print(f"State {'=' * 74}\n")
        print(f"     Database: {self.db.dbtype}:{self.db.dbname}")
        print(f"  Current run: {self.current_run_id}")
        print(f"Current issue: {self.current_issue_id}")

    def runs(self, use_pager=None):
        pager = self._resolve_pager(use_pager)

        with self.db.make_session() as session:
            runs = session.query(Run).filter(Run.status == RunStatus.FINISHED).all()

        run_strings = [
            "\n".join([f"Run {run.id}", f"Date: {run.date}", "-" * 80]) for run in runs
        ]
        run_output = "\n".join(run_strings)

        pager(run_output)
        print(f"Found {len(runs)} runs.")

    def set_run(self, run_id):
        with self.db.make_session() as session:
            selected_run = (
                session.query(Run)
                .filter(Run.status == RunStatus.FINISHED)
                .filter(Run.id == run_id)
                .scalar()
            )

        if selected_run is None:
            self.warning(
                f"Run {run_id} doesn't exist or is not finished. "
                "Type 'runs()' for available runs."
            )
            return

        self.current_run_id = selected_run.id
        print(f"Set run to {run_id}.")

    def set_issue(self, issue_id):
        with self.db.make_session() as session:
            selected_issue = (
                session.query(IssueInstance)
                .filter(IssueInstance.id == issue_id)
                .scalar()
            )

        if selected_issue is None:
            self.warning(
                f"Issue {issue_id} doesn't exist. "
                "Type 'issues()' for available issues."
            )
            return

        self.current_issue_id = selected_issue.id
        self.current_trace_frame_index = 1  # first one after the source
        print(f"Set issue to {issue_id}.")
        self.show()

    def show(self):
        """ More details about the selected issue.
        """
        if self.current_issue_id is None:
            self.warning("Use 'set_issue(ID)' to select an issue first.")
            return

        with self.db.make_session() as session:
            issue_instance, issue = self._get_current_issue(session)
            sources = self._get_sources(session, issue_instance)
            sinks = self._get_sinks(session, issue_instance)

        page.display_page(
            self._create_issue_output_string(issue_instance, issue, sources, sinks)
        )

    def issues(
        self,
        use_pager: bool = None,
        *,
        codes: Optional[List[int]] = None,
        callables: Optional[List[str]] = None,
        filenames: Optional[List[str]] = None,
    ):
        """Lists issues for the selected run.

        Parameters (all optional):
            use_pager: bool         use a unix style pager for output
            codes: list[int]        issue codes to filter on
            callables: list[str]    callables to filter on (supports wildcards)
            filenames: list[str]    filenames to filter on (supports wildcards)

        String filters support LIKE wildcards (%, _) from SQL:
            % matches anything (like .* in regex)
            _ matches 1 character (like . in regex)

        For example:
            callables=[
                "%occurs.anywhere%",
                "%at.end",
                "at.start%",
                "etc.",
            ])
        """
        pager = self._resolve_pager(use_pager)

        with self.db.make_session() as session:
            query = (
                session.query(IssueInstance, Issue)
                .filter(IssueInstance.run_id == self.current_run_id)
                .join(Issue, IssueInstance.issue_id == Issue.id)
            )

            # Process filters

            if codes is not None:
                if not isinstance(codes, list):
                    self.warning("'codes' should be a list.")
                    return
                query = query.filter(Issue.code.in_(codes))

            if callables is not None:
                if not isinstance(callables, list):
                    self.warning("'callables' should be a list.")
                    return
                query = query.filter(
                    or_(*[Issue.callable.like(callable) for callable in callables])
                )

            if filenames is not None:
                if not isinstance(filenames, list):
                    self.warning("'filenames' should be a list.")
                    return
                query = query.filter(
                    or_(*[Issue.filename.like(filename) for filename in filenames])
                )

            issues = query.options(joinedload(IssueInstance.message)).all()
            sources_list = [
                self._get_sources(session, issue_instance)
                for issue_instance, _ in issues
            ]
            sinks_list = [
                self._get_sinks(session, issue_instance) for issue_instance, _ in issues
            ]

        issue_strings = [
            self._create_issue_output_string(issue_instance, issue, sources, sinks)
            for (issue_instance, issue), sources, sinks in zip(
                issues, sources_list, sinks_list
            )
        ]
        issue_output = f"\n{'-' * 80}\n".join(issue_strings)
        pager(issue_output)
        print(f"Found {len(issues)} issues with run_id {self.current_run_id}.")

    def trace(self):
        """Show a trace for the selected issue.

        The '-->' token points to the currently active trace frame within the
        trace.
        """
        if self.current_issue_id is None:
            self.warning("Use 'set_issue(ID)' to select an issue first.")
            return

        self._generate_trace()

        self._output_trace_tuples(self.trace_tuples)

    def _generate_trace(self):
        if self.trace_tuples_id == self.current_issue_id:
            return  # already generated

        with self.db.make_session() as session:
            issue_instance, issue = self._get_current_issue(session)

            postcondition_traces = self._navigate_trace_frames(
                session, self._initial_trace_frame(session, TraceKind.POSTCONDITION)
            )
            precondition_traces = self._navigate_trace_frames(
                session, self._initial_trace_frame(session, TraceKind.PRECONDITION)
            )

        self.trace_tuples = self._create_trace_tuples(
            postcondition_traces[::-1], precondition_traces
        )
        self.trace_tuples_id = self.current_issue_id

    def _output_trace_tuples(self, trace_tuples):
        max_length_callable = max(
            len(trace_tuple.callable) for trace_tuple in trace_tuples
        )
        max_length_condition = max(
            len(trace_tuple.condition) for trace_tuple in trace_tuples
        )

        for i in range(len(trace_tuples)):
            prefix = "-->" if i == self.current_trace_frame_index else " " * 3
            callable, condition, filename, location, missing = trace_tuples[i]

            if missing:
                output_string = (
                    f" {prefix}"
                    f" [Missing trace frame: {trace_tuples[i].callable}:"
                    f"{trace_tuples[i].condition}]"
                )
            else:
                output_string = (
                    f" {prefix}"
                    f" {callable:{max_length_callable}}"
                    f" {condition:{max_length_condition}}"
                )
                if filename is not None and location is not None:
                    output_string += f" {filename}:{location}"

            print(output_string)

    def _create_trace_tuples(self, postcondition_traces, precondition_traces):
        trace_tuples = [
            TraceTuple(
                postcondition_traces[0].callee,
                postcondition_traces[0].callee_port,
                missing=postcondition_traces[0].caller is None,
            )
        ]
        for i in range(1, len(postcondition_traces)):
            trace_tuples.append(
                TraceTuple(
                    postcondition_traces[i].callee,
                    postcondition_traces[i].callee_port,
                    # i - 1 since it's a postcondition
                    postcondition_traces[i - 1].filename,
                    postcondition_traces[i - 1].callee_location,
                    missing=postcondition_traces[i].caller is None,
                )
            )

        trace_tuples.append(
            TraceTuple(
                postcondition_traces[-1].caller,
                postcondition_traces[-1].caller_port,
                postcondition_traces[-1].filename,
                postcondition_traces[-1].callee_location,
            )
        )  # add the issue root

        for trace_frame in precondition_traces:
            trace_tuples.append(
                TraceTuple(
                    trace_frame.callee,
                    trace_frame.callee_port,
                    trace_frame.filename,
                    trace_frame.callee_location,
                    missing=trace_frame.caller is None,
                )
            )

        return trace_tuples

    def _initial_trace_frame(self, session, kind):
        return (
            session.query(TraceFrame)
            .filter(TraceFrame.issue_instances.any(id=self.current_issue_id))
            .filter(TraceFrame.kind == kind)
            .first()  # just first for now
        )

    def _navigate_trace_frames(self, session, initial_trace_frame):
        trace_frames = [initial_trace_frame]
        while not self._is_leaf(trace_frames[-1]):
            next_trace_frame = self._next_trace_frame(session, trace_frames[-1])

            if next_trace_frame is None:
                # Denote a missing frame by setting caller to None
                trace_frames.append(
                    TraceFrame(
                        callee=trace_frames[-1].callee,
                        callee_port=trace_frames[-1].callee_port,
                        caller=None,
                    )
                )
                return trace_frames

            trace_frames.append(next_trace_frame)
        return trace_frames

    def _is_leaf(self, trace_frame: TraceFrame) -> bool:
        return trace_frame.callee_port in self.LEAF_NAMES

    def _next_trace_frame(self, session, trace_frame):
        return (
            session.query(TraceFrame)
            .filter(
                TraceFrame.caller != TraceFrame.callee
            )  # skip recursive calls for now
            .filter(TraceFrame.caller == trace_frame.callee)
            .filter(TraceFrame.caller_port == trace_frame.callee_port)
            .first()  # just first for now
        )

    def _create_issue_output_string(self, issue_instance, issue, sources, sinks):
        sources_output = f"\n{' ' * 10}".join(sources)
        sinks_output = f"\n{' ' * 10}".join(sinks)
        return "\n".join(
            [
                f"Issue {issue_instance.id}",
                f"    Code: {issue.code}",
                f" Message: {issue_instance.message.contents}",
                f"Callable: {issue.callable}",
                f" Sources: {sources_output}",
                f"   Sinks: {sinks_output}",
                (
                    f"Location: {issue_instance.filename}"
                    f":{SourceLocation.to_string(issue_instance.location)}"
                ),
            ]
        )

    def _resolve_pager(self, use_pager):
        use_pager = sys.stdout.isatty() if use_pager is None else use_pager
        return page.page if use_pager else page.display_page

    def _get_current_issue(self, session):
        return (
            session.query(IssueInstance, Issue)
            .filter(IssueInstance.id == self.current_issue_id)
            .join(Issue, IssueInstance.issue_id == Issue.id)
            .options(joinedload(IssueInstance.message))
            .first()
        )

    def _get_sources(self, session, issue_instance):
        return [
            source
            for source, in session.query(Source.name)
            .filter(
                Source.postconditions.any(
                    Postcondition.issue_instances.any(id=issue_instance.id)
                )
            )
            .all()
        ]

    def _get_sinks(self, session, issue_instance):
        return [
            sink
            for sink, in session.query(Sink.name)
            .filter(
                Sink.preconditions.any(
                    Precondition.issue_instances.any(id=issue_instance.id)
                )
            )
            .all()
        ]

    def warning(self, message: str) -> None:
        print(message, file=sys.stderr)


class TraceTuple(NamedTuple):
    callable: str
    condition: str
    filename: Optional[str] = None
    location: Optional[SourceLocation] = None
    missing: bool = False
