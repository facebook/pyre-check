#!/usr/bin/env python3

import os
import sys
from collections import defaultdict
from typing import (
    Callable,
    DefaultDict,
    Dict,
    Iterable,
    List,
    NamedTuple,
    Optional,
    Set,
    Tuple,
    Union,
)

import click
from IPython.core import page
from pygments import highlight
from pygments.formatters import TerminalFormatter
from pygments.lexers import get_lexer_for_filename
from sqlalchemy.orm import Session
from sqlalchemy.orm.attributes import InstrumentedAttribute
from sqlalchemy.orm.query import Query
from sqlalchemy.sql import func
from sqlalchemy.sql.expression import or_

from .db import DB
from .decorators import UserError, catch_keyboard_interrupt, catch_user_error
from .models import (
    DBID,
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


class IssueQueryResult(NamedTuple):
    id: DBID
    filename: str
    location: SourceLocation
    code: int
    callable: str
    contents: str


class TraceTuple(NamedTuple):
    trace_frame: TraceFrame
    branches: int = 1
    missing: bool = False
    # Placeholder flag is used when we need to "fake" a trace node to display
    #   the entire trace.
    # Suppose we select a trace frame (A->B) and the generated trace is
    #   (A->B), (B->C), (C->D) with D as leaf.
    # When we display traces, we only use the callee, so this trace would look
    #   like B->C->D. If we also want to see A->, then we need to add a
    #   placeholder trace tuple whose callee is A.
    placeholder: bool = False


class ListFilterException(Exception):
    pass


class Interactive:
    # @lint-ignore FBPYTHON2
    list_string = "list()"
    help_message = f"""
Commands =======================================================================

commands()               show this message
help(COMAMND)            more info about a command
state()                  show the internal state of the tool for debugging

runs()                   list all completed static analysis runs
set_run(ID)              select a specific run for browsing issues
issues()                 list all issues for the selected run
set_issue_instance(ID)   select a specific issue for browsing a trace
show()                   show info about selected issue or trace frame
trace()                  show a trace of the selected issue or trace frame
prev()/p()               move backward within the trace
next()/n()               move forward within the trace
jump(NUM)                jump to a specific trace frame in a trace
branch(INDEX)            select a trace branch
{list_string}                   show source code at the current trace frame

frames()                 show trace frames independently of an issue
set_frame(ID)            select a trace frame to explore
parents()                show trace frames that call the current trace frame
details()                show additional information about the current trace frame
"""
    welcome_message = "Interactive issue exploration. Type 'commands()' for help."

    LEAF_NAMES = {"source", "sink", "leaf"}

    def __init__(self, database: DB, repository_directory: str = ""):
        self.db = database
        self.scope_vars: Dict[str, Union[Callable, TraceKind]] = {
            "precondition": TraceKind.PRECONDITION,
            "postcondition": TraceKind.POSTCONDITION,
            "commands": self.help,
            "state": self.state,
            "runs": self.runs,
            "issues": self.issues,
            "set_run": self.set_run,
            "set_issue_instance": self.set_issue_instance,
            "show": self.show,
            "trace": self.trace,
            "next": self.next_cursor_location,
            "n": self.next_cursor_location,
            "prev": self.prev_cursor_location,
            "p": self.prev_cursor_location,
            "jump": self.jump,
            "branch": self.branch,
            "list": self.list_source_code,
            "frames": self.frames,
            "set_frame": self.set_frame,
            "parents": self.parents,
            "details": self.details,
        }
        self.repository_directory = repository_directory or os.getcwd()

        self.current_run_id: int = -1

        # Trace exploration relies on either of these
        self.current_issue_instance_id: int = -1
        self.current_frame_id: int = -1

        self.sources: Set[str] = set()
        self.sinks: Set[str] = set()
        self.sources_dict: Dict[int, str] = {}
        self.sinks_dict: Dict[int, str] = {}

        # Tuples representing the trace of the current issue
        self.trace_tuples: List[TraceTuple] = []
        # Active trace frame of the current trace
        self.current_trace_frame_index: int = -1

    def setup(self) -> Dict[str, Callable]:
        with self.db.make_session() as session:
            latest_run_id = (
                session.query(func.max(Run.id))
                .filter(Run.status == RunStatus.FINISHED)
                .scalar()
            )

            self.sources_dict = self._all_leaves_by_kind(session, SharedTextKind.SOURCE)
            self.sinks_dict = self._all_leaves_by_kind(session, SharedTextKind.SINK)

        if latest_run_id.resolved() is None:
            self.warning(
                "No runs found. "
                f"Try running '{os.path.basename(sys.argv[0])} analyze' first."
            )

        self.current_run_id = latest_run_id
        print("=" * len(self.welcome_message))
        print(self.welcome_message)
        print("=" * len(self.welcome_message))
        return self.scope_vars

    def help(self):
        print(self.help_message)

    def state(self):
        print(f"              Database: {self.db.dbtype}:{self.db.dbname}")
        print(f"  Repository directory: {self.repository_directory}")
        print(f"           Current run: {self.current_run_id}")
        print(f"Current issue instance: {self.current_issue_instance_id}")
        print(f"   Current trace frame: {self.current_frame_id}")
        print(f"        Sources filter: {self.sources}")
        print(f"          Sinks filter: {self.sinks}")

    @catch_keyboard_interrupt()
    def runs(self, use_pager=None):
        pager = self._resolve_pager(use_pager)

        with self.db.make_session() as session:
            runs = session.query(Run).filter(Run.status == RunStatus.FINISHED)

            run_strings = [
                "\n".join([f"Run {run.id}", f"Date: {run.date}", "-" * 80])
                for run in runs
            ]
        run_output = "\n".join(run_strings)

        pager(run_output)
        print(f"Found {len(run_strings)} runs.")

    @catch_keyboard_interrupt()
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

    @catch_keyboard_interrupt()
    def set_issue_instance(self, issue_instance_id):
        with self.db.make_session() as session:
            selected_issue = (
                session.query(IssueInstance)
                .filter(IssueInstance.id == issue_instance_id)
                .scalar()
            )

            if selected_issue is None:
                self.warning(
                    f"Issue {issue_instance_id} doesn't exist. "
                    "Type 'issues()' for available issues."
                )
                return

            self.sources = set(
                self._get_leaves_issue_instance(
                    session, issue_instance_id, SharedTextKind.SOURCE
                )
            )
            self.sinks = set(
                self._get_leaves_issue_instance(
                    session, issue_instance_id, SharedTextKind.SINK
                )
            )

        self.current_issue_instance_id = selected_issue.id
        self.current_frame_id = -1
        self.current_trace_frame_index = 1  # first one after the source

        self._generate_trace_from_issue()

        print(f"Set issue to {issue_instance_id}.")
        self.show()

    @catch_keyboard_interrupt()
    @catch_user_error()
    def show(self):
        """ More details about the selected issue or trace frame.
        """
        self._verify_entrypoint_selected()

        if self.current_issue_instance_id != -1:
            self._show_current_issue_instance()
            return

        self._show_current_trace_frame()

    @catch_keyboard_interrupt()
    @catch_user_error()
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
            query = session.query(
                IssueInstance.id,
                IssueInstance.filename,
                IssueInstance.location,
                Issue.code,
                Issue.callable,
                SharedText.contents,
            ).filter(IssueInstance.run_id == self.current_run_id)

            if codes is not None:
                self._verify_list_filter(codes, "codes")
                query = query.filter(Issue.code.in_(codes))

            if callables is not None:
                self._verify_list_filter(callables, "callables")
                query = self._add_list_filter_to_query(callables, query, Issue.callable)

            if filenames is not None:
                self._verify_list_filter(filenames, "filenames")
                query = self._add_list_filter_to_query(
                    filenames, query, IssueInstance.filename
                )

            issues = query.join(Issue, IssueInstance.issue_id == Issue.id).join(
                SharedText, SharedText.id == IssueInstance.message_id
            )

            sources_list = [
                self._get_leaves_issue_instance(
                    session, int(issue.id), SharedTextKind.SOURCE
                )
                for issue in issues
            ]
            sinks_list = [
                self._get_leaves_issue_instance(
                    session, int(issue.id), SharedTextKind.SINK
                )
                for issue in issues
            ]

            issue_strings = [
                self._create_issue_output_string(issue, sources, sinks)
                for issue, sources, sinks in zip(issues, sources_list, sinks_list)
            ]
        issue_output = f"\n{'-' * 80}\n".join(issue_strings)
        pager(issue_output)
        print(f"Found {len(issue_strings)} issues with run_id {self.current_run_id}.")

    @catch_user_error()
    def trace(self):
        """Show a trace for the selected issue or trace frame.

        The '-->' token points to the currently active trace frame within the
        trace.

        Trace output has 4 columns:
        - branches: the number of siblings a node has (including itself)
          [indicates that the trace branches into multiple paths]
        - callable: the name of the object that was called
        - port/condition: a description of the type of trace frame
          - source: where data originally comes from
          - root: the main callable through which the data propagates
          - sink: where data eventually flows to
        - location: the relative location of the trace frame's source code

        Example output:
             ⎇  [callable]            [port]    [location]
             +2 leaf                  source    module/main.py:26|4|8
         -->    module.main           root      module/helper.py:76|5|10
                module.helper.process root      module/helper.py:76|5|10
             +3 leaf                  sink      module/main.py:74|1|9
        """
        self._verify_entrypoint_selected()
        self._output_trace_tuples(self.trace_tuples)

    @catch_keyboard_interrupt()
    @catch_user_error()
    def frames(
        self,
        *,
        callers: Optional[List[str]] = None,
        callees: Optional[List[str]] = None,
        kind: Optional[TraceKind] = None,
    ):
        """Display trace frames independent of the current issue.

        Parameters (all optional):
            callers: list[str]                  filter traces by this caller name
            callees: list[str]                  filter traces by this callee name
            kind: precondition|postcondition    the type of trace frames to show

        Sample usage:
            frames(callers=["module.function"], kind=postcondition)

        String filters support LIKE wildcards (%, _) from SQL:
            % matches anything (like .* in regex)
            _ matches 1 character (like . in regex)
        """
        with self.db.make_session() as session:
            query = session.query(TraceFrame).filter(
                TraceFrame.run_id == self.current_run_id
            )

            if callers is not None:
                self._verify_list_filter(callers, "callers")
                query = self._add_list_filter_to_query(
                    callers, query, TraceFrame.caller
                )

            if callees is not None:
                self._verify_list_filter(callees, "callees")
                query = self._add_list_filter_to_query(
                    callees, query, TraceFrame.callee
                )

            if kind is not None:
                if kind not in {TraceKind.PRECONDITION, TraceKind.POSTCONDITION}:
                    raise UserError(
                        "Try 'frames(kind=postcondition)'"
                        " or 'frames(kind=precondition)'."
                    )
                query = query.filter(TraceFrame.kind == kind)

            trace_frames = query.group_by(TraceFrame.id).order_by(
                TraceFrame.caller, TraceFrame.callee
            )

            self._output_trace_frames(self._group_trace_frames(trace_frames))

    @catch_keyboard_interrupt()
    def set_frame(self, frame_id: int) -> None:
        with self.db.make_session() as session:
            selected_frame = (
                session.query(TraceFrame).filter(TraceFrame.id == frame_id).scalar()
            )

            if selected_frame is None:
                self.warning(
                    f"Trace frame {frame_id} doesn't exist. "
                    "Type 'frames()' for available trace frames."
                )
                return

            leaves = {leaf.contents for leaf in selected_frame.leaves}
            if selected_frame.kind == TraceKind.POSTCONDITION:
                self.sinks = set()
                self.sources = leaves
            else:
                self.sinks = leaves
                self.sources = set()

        self.current_frame_id = selected_frame.id
        self.current_issue_instance_id = -1

        self._generate_trace_from_frame()

        print(f"Set trace frame to {frame_id}.")
        self.show()

    @catch_keyboard_interrupt()
    @catch_user_error()
    def parents(self) -> None:
        self._verify_entrypoint_selected()
        current_trace_frame = self.trace_tuples[
            self.current_trace_frame_index
        ].trace_frame

        # Don't allow calling from the leaf node in a trace. Instead, call
        # parents() from the placeholder of the caller of the leaf node.
        if self._is_leaf(current_trace_frame):
            raise UserError("Try running from a non-leaf node.")

        with self.db.make_session() as session:
            parent_trace_frames = self._next_trace_frames(
                session, current_trace_frame, backwards=True
            )

        if len(parent_trace_frames) == 0:
            print(
                f"No parents calling [{current_trace_frame.callee} "
                f": {current_trace_frame.callee_port}]."
            )
            return

        parent_trace_frame = self._select_parent_trace_frame(parent_trace_frames)

        self._update_trace_tuples_new_parent(parent_trace_frame)
        self.trace()

    def _generate_trace_from_issue(self):
        with self.db.make_session() as session:
            issue = self._get_current_issue(session)

            postcondition_navigation = self._navigate_trace_frames(
                session,
                self._initial_trace_frames(session, issue.id, TraceKind.POSTCONDITION),
            )
            precondition_navigation = self._navigate_trace_frames(
                session,
                self._initial_trace_frames(session, issue.id, TraceKind.PRECONDITION),
            )

        self.trace_tuples = (
            self._create_trace_tuples(reversed(postcondition_navigation))
            + [
                TraceTuple(
                    trace_frame=TraceFrame(
                        callee=issue.callable,
                        callee_port="root",
                        filename=issue.filename,
                        callee_location=issue.location,
                    ),
                    placeholder=True,
                )
            ]
            + self._create_trace_tuples(precondition_navigation)
        )
        self.trace_tuples_id = self.current_issue_instance_id
        self.current_trace_frame_index = len(postcondition_navigation)

    def _generate_trace_from_frame(self):
        with self.db.make_session() as session:
            trace_frame = (
                session.query(TraceFrame)
                .filter(TraceFrame.id == self.current_frame_id)
                .scalar()
            )

            navigation = self._navigate_trace_frames(session, [trace_frame])

        first_trace_frame = navigation[0][0]
        placeholder_tuple = [
            TraceTuple(
                trace_frame=TraceFrame(
                    caller=None,
                    caller_port=None,
                    callee=first_trace_frame.caller,
                    callee_port=first_trace_frame.caller_port,
                    filename=first_trace_frame.filename,
                    callee_location=first_trace_frame.callee_location,
                    kind=first_trace_frame.kind,
                ),
                placeholder=True,
            )
        ]

        self.trace_tuples = self._create_trace_tuples(navigation)

        if trace_frame.kind == TraceKind.POSTCONDITION:
            self.trace_tuples = self.trace_tuples[::-1] + placeholder_tuple
            self.current_trace_frame_index = len(self.trace_tuples) - 1
            return

        self.trace_tuples = placeholder_tuple + self.trace_tuples
        self.current_trace_frame_index = 0

    @catch_user_error()
    def next_cursor_location(self):
        """Move cursor to the next trace frame.
        """
        self._verify_entrypoint_selected()
        self.current_trace_frame_index = min(
            self.current_trace_frame_index + 1, len(self.trace_tuples) - 1
        )
        self.trace()

    @catch_user_error()
    def prev_cursor_location(self):
        """Move cursor to the previous trace frame.
        """
        self._verify_entrypoint_selected()
        self.current_trace_frame_index = max(self.current_trace_frame_index - 1, 0)
        self.trace()

    @catch_user_error()
    def jump(self, selected_number: int) -> None:
        """Jump to a specific trace frame in a trace.

        Parameters:
            selected_number: int    the trace frame number from trace() output
        """
        self._verify_entrypoint_selected()
        if selected_number < 1 or selected_number > len(self.trace_tuples):
            raise UserError(
                "Trace frame number out of bounds "
                f"(expected 1-{len(self.trace_tuples)} but got {selected_number})."
            )

        self.current_trace_frame_index = selected_number - 1
        self.trace()

    def _get_branch_options(
        self, session: Session
    ) -> Tuple[List[TraceFrame], List[str]]:
        current_trace_tuple = self.trace_tuples[self.current_trace_frame_index]
        filter_leaves = (
            self.sources
            if current_trace_tuple.trace_frame.kind == TraceKind.POSTCONDITION
            else self.sinks
        )

        branches = self._get_trace_frame_branches(session)

        leaves_strings = []
        for frame in branches:
            if frame.kind == TraceKind.POSTCONDITION:
                shared_text_kind = SharedTextKind.SOURCE
            elif frame.kind == TraceKind.PRECONDITION:
                shared_text_kind = SharedTextKind.SINK
            else:
                assert False, f"{frame.kind} is invalid"
            leaves_strings.append(
                ", ".join(
                    [
                        leaf
                        for leaf in self._get_leaves_trace_frame(
                            session, int(frame.id), shared_text_kind
                        )
                        if leaf in filter_leaves
                    ]
                )
            )

        return branches, leaves_strings

    @catch_keyboard_interrupt()
    @catch_user_error()
    def branch(self, selected_number: Optional[int] = None) -> None:
        """Show and select branches for a branched trace.
        - [*] signifies the current branch that is selected
        - will prompt for branch selection if called with no argument
        - will automatically select a branch if called with an argument

        Parameters (optional):
            selected_number: int    branch number from expand() output

        Example output:

        Suppose we have the trace output:
             ⎇  [callable]            [port]    [location]
         --> +2 leaf                  source    module/main.py:26|4|8
                module.main           root      module/helper.py:76|5|10
                module.helper.process root      module/helper.py:76|5|10
             +3 leaf                  sink      module/main.py:74|1|9

        Calling expand will result in the output:
        [*] leaf
                [0 hops: source]
                [module/main.py:26|4|8]
        [1] module.helper.preprocess
                [1 hops: source]
                [module/main.py:21|4|8]
        """
        self._verify_entrypoint_selected()
        self._verify_multiple_branches()

        with self.db.make_session() as session:
            branches, leaves_strings = self._get_branch_options(session)

            if selected_number is None:
                selected_number = self._select_branch_trace_frame(
                    branches, leaves_strings
                )

            if (
                not isinstance(selected_number, int)
                or selected_number < 1
                or selected_number > len(branches)
            ):
                raise UserError(
                    "Branch number invalid "
                    f"(expected 1-{len(branches)} but got {selected_number})."
                )

            new_navigation = self._navigate_trace_frames(
                session, branches, selected_number - 1
            )

        new_trace_tuples = self._create_trace_tuples(new_navigation)

        if self._is_before_root():
            new_trace_tuples.reverse()
            self.trace_tuples = (
                new_trace_tuples
                + self.trace_tuples[self.current_trace_frame_index + 1 :]
            )

            # If length of prefix changes, it will change some indices
            trace_frame_index_delta = (
                len(new_navigation) - self.current_trace_frame_index - 1
            )
            self.current_trace_frame_index += trace_frame_index_delta
        else:
            self.trace_tuples = (
                self.trace_tuples[: self.current_trace_frame_index] + new_trace_tuples
            )

        self.trace()

    @catch_keyboard_interrupt()
    @catch_user_error()
    def list_source_code(self, context: int = 5) -> None:
        """Show source code around the current trace frame location.

        Parameters:
            context: int    number of lines to show above and below trace location
                            (default: 5)
        """
        self._verify_entrypoint_selected()

        current_trace_frame = self.trace_tuples[
            self.current_trace_frame_index
        ].trace_frame

        filename = os.path.join(self.repository_directory, current_trace_frame.filename)
        file_lines: List[str] = []

        try:
            # Use readlines instead of enumerate(file) because mock_open
            # doesn't support __iter__ until python 3.7.1.
            with open(filename, "r") as file:
                file_lines = file.readlines()
        except FileNotFoundError:
            self.warning(f"Couldn't open {filename}.")
            return

        self._output_file_lines(current_trace_frame, file_lines, context)

    def details(self) -> None:
        """Show additional info about the current trace frame.
        """
        self._verify_entrypoint_selected()

        current_trace_frame = self.trace_tuples[
            self.current_trace_frame_index
        ].trace_frame

        print(self._create_trace_frame_output_string(current_trace_frame))
        print(
            f"\nIssues in callable ({current_trace_frame.callee}):",
            self._num_issues_with_callable(current_trace_frame.callee),
        )

    def warning(self, message: str) -> None:
        # pyre-fixme[6]: Expected `Optional[_Writer]` for 2nd param but got `TextIO`.
        print(message, file=sys.stderr)

    def _get_trace_frame_branches(self, session: Session) -> List[TraceFrame]:
        delta_from_parent = 1 if self._is_before_root() else -1
        parent_index = self.current_trace_frame_index + delta_from_parent
        parent_trace_tuple = self.trace_tuples[parent_index]

        if self._is_root_trace_tuple(parent_trace_tuple):
            kind = (
                TraceKind.POSTCONDITION
                if self._is_before_root()
                else TraceKind.PRECONDITION
            )
            return self._initial_trace_frames(
                session, self.current_issue_instance_id, kind
            )

        parent_trace_frame = self.trace_tuples[parent_index].trace_frame
        return self._next_forward_trace_frames(session, parent_trace_frame)

    def _is_before_root(self) -> bool:
        trace_tuple = self.trace_tuples[self.current_trace_frame_index]
        return trace_tuple.trace_frame.kind == TraceKind.POSTCONDITION

    def _is_root_trace_tuple(self, trace_tuple: TraceTuple) -> bool:
        return trace_tuple.trace_frame.callee_port == "root"

    def _current_branch_index(self, branches: List[TraceFrame]) -> int:
        selected_branch_id = int(
            self.trace_tuples[self.current_trace_frame_index].trace_frame.id
        )
        for i, branch in enumerate(branches):
            if selected_branch_id == int(branch.id):
                return i
        return -1

    def _group_trace_frames(
        self, trace_frames: Iterable[TraceFrame]
    ) -> Dict[Tuple[str, str], List[TraceFrame]]:
        """Buckets together trace frames that have the same caller:caller_port.
        """
        # pyre-fixme[9]: caller_buckets has type `DefaultDict[Tuple[str, str], List[T...
        caller_buckets: DefaultDict[Tuple[str, str], List[TraceFrame]] = defaultdict(
            list
        )
        for trace_frame in trace_frames:
            caller_buckets[(trace_frame.caller, trace_frame.caller_port)].append(
                trace_frame
            )
        return caller_buckets

    def _verify_list_filter(self, filter: List, argument_name: str) -> None:
        # Check this because filter is user input
        if not isinstance(filter, list):
            raise UserError(f"'{argument_name}' should be a list.")

        if not filter:
            raise UserError(f"'{argument_name}' should be non-empty.")

    def _add_list_filter_to_query(
        self, filter: List[str], query: Query, column: InstrumentedAttribute
    ) -> Query:
        return query.filter(or_(*[column.like(item) for item in filter]))

    def _output_file_lines(
        self, trace_frame: TraceFrame, file_lines: List[str], context: int
    ) -> None:
        print(
            f"In {trace_frame.caller or trace_frame.callee} "
            f"[{trace_frame.filename}:{trace_frame.callee_location}]"
        )
        center_line_number = trace_frame.callee_location.line_no
        line_number_width = len(str(center_line_number + context))

        for i in range(
            max(center_line_number - context, 1),
            min(center_line_number + context, len(file_lines)) + 1,
        ):
            line = file_lines[i - 1]

            prefix = " --> " if i == center_line_number else " " * 5
            prefix += f"{i:<{line_number_width}} "
            if sys.stdout.isatty():
                line = highlight(
                    line,
                    get_lexer_for_filename(trace_frame.filename),
                    TerminalFormatter(),
                )
            print(f"{prefix} {line}", end="")

    def _output_trace_expansion(
        self, trace_frames: List[TraceFrame], leaves_strings: List[str]
    ) -> None:
        for i, (frame, leaves) in enumerate(zip(trace_frames, leaves_strings)):
            prefix = (
                "[*]" if i == self._current_branch_index(trace_frames) else f"[{i + 1}]"
            )
            print(f"{prefix} {frame.callee} : {frame.callee_port}")
            print(f"{' ' * 8}[{frame.leaf_assoc[0].trace_length} hops: {leaves}]")
            print(f"{' ' * 8}[{frame.filename}:{frame.callee_location}]")

    def _output_trace_frames(
        self, trace_buckets: Dict[Tuple[str, str], List[TraceFrame]]
    ) -> None:
        if not trace_buckets:
            print("No trace frames found.")
            return

        max_len_id = max(
            max(
                max(len(str(trace_frame.id)) for trace_frame in value)
                for key, value in trace_buckets.items()
            ),
            len("[id]"),
        )

        print(f"{'[id]':{max_len_id}} [caller:caller_port -> callee:callee_port]")

        callers = trace_buckets.keys()
        for caller, caller_port in callers:
            print(f"{'-' * max_len_id} {caller}:{caller_port} ->")
            trace_frames = trace_buckets[(caller, caller_port)]
            for trace_frame in trace_frames:
                print(
                    f"{int(trace_frame.id):<{max_len_id}} "
                    f"    {trace_frame.callee}:{trace_frame.callee_port}"
                )

    def _output_trace_tuples(self, trace_tuples):
        expand = "+"
        max_length_index = len(str(len(trace_tuples) - 1)) + 1
        max_length_split = max(
            max(
                len(str(trace_tuple.branches)) + len(expand)
                for trace_tuple in trace_tuples
            ),
            len("⎇"),
        )
        max_length_callable = max(
            max(len(trace_tuple.trace_frame.callee) for trace_tuple in trace_tuples),
            len("[callable]"),
        )
        max_length_condition = max(
            max(
                len(trace_tuple.trace_frame.callee_port) for trace_tuple in trace_tuples
            ),
            len("[port]"),
        )

        print(  # table header
            f"{' ' * 5}"
            f"{'#':{max_length_index}}"
            f"{'⎇':{max_length_split}}"
            f" {'[callable]':{max_length_callable}}"
            f" {'[port]':{max_length_condition}}"
            f" [location]"
        )

        for i, trace_tuple in enumerate(trace_tuples):
            prefix = "-->" if i == self.current_trace_frame_index else " " * 3
            prefix += f" {(i + 1):<{max_length_index}}"

            if trace_tuple.missing:
                output_string = (
                    f" {prefix}"
                    f" [Missing trace frame: {trace_tuple.trace_frame.callee}:"
                    f"{trace_tuple.trace_frame.callee_port}]"
                )
            else:
                branches_string = (
                    f"{expand}"
                    f"{str(trace_tuple.branches):{max_length_split - len(expand)}}"
                    if trace_tuple.branches > 1
                    else " " * max_length_split
                )
                output_string = (
                    f" {prefix}"
                    f"{branches_string}"
                    f" {trace_tuple.trace_frame.callee:{max_length_callable}}"
                    f" {trace_tuple.trace_frame.callee_port:{max_length_condition}}"
                    f" {trace_tuple.trace_frame.filename}"
                    f":{trace_tuple.trace_frame.callee_location}"
                )

            print(output_string)

    def _create_trace_tuples(
        self, navigation: Iterable[Tuple[TraceFrame, int]]
    ) -> List[TraceTuple]:
        return [
            TraceTuple(
                trace_frame=trace_frame,
                branches=branches,
                missing=trace_frame.caller is None,
            )
            for trace_frame, branches in navigation
        ]

    def _initial_trace_frames(self, session, issue_instance_id, kind):
        return (
            session.query(TraceFrame)
            .join(
                IssueInstanceTraceFrameAssoc,
                IssueInstanceTraceFrameAssoc.trace_frame_id == TraceFrame.id,
            )
            .filter(IssueInstanceTraceFrameAssoc.issue_instance_id == issue_instance_id)
            .filter(TraceFrame.kind == kind)
            .join(TraceFrame.leaf_assoc)
            .group_by(TraceFrame.id)
            .order_by(TraceFrameLeafAssoc.trace_length, TraceFrame.callee_location)
            .all()
        )

    def _navigate_trace_frames(
        self, session: Session, initial_trace_frames: List[TraceFrame], index: int = 0
    ) -> List[Tuple[TraceFrame, int]]:
        if not initial_trace_frames:
            return []

        trace_frames = [(initial_trace_frames[index], len(initial_trace_frames))]
        while not self._is_leaf(trace_frames[-1][0]):
            trace_frame, branches = trace_frames[-1]
            next_nodes = self._next_forward_trace_frames(session, trace_frame)

            if len(next_nodes) == 0:
                # Denote a missing frame by setting caller to None
                trace_frames.append(
                    (
                        TraceFrame(
                            callee=trace_frame.callee,
                            callee_port=trace_frame.callee_port,
                            caller=None,
                        ),
                        0,
                    )
                )
                return trace_frames

            trace_frames.append((next_nodes[0], len(next_nodes)))
        return trace_frames

    def _is_leaf(self, trace_frame: TraceFrame) -> bool:
        return trace_frame.callee_port in self.LEAF_NAMES

    def _next_forward_trace_frames(
        self, session: Session, trace_frame: TraceFrame
    ) -> List[TraceFrame]:
        return self._next_trace_frames(session, trace_frame, backwards=False)

    def _next_backward_trace_frames(
        self, session: Session, trace_frame: TraceFrame
    ) -> List[TraceFrame]:
        return self._next_trace_frames(session, trace_frame, backwards=True)

    def _next_trace_frames(
        self, session: Session, trace_frame: TraceFrame, backwards: bool = False
    ) -> List[TraceFrame]:
        """Finds all trace frames that the given trace_frame flows to.

        When backwards=True, the result will include the parameter trace_frame,
        since we are filtering on the parameter's callee.
        """
        query = (
            session.query(TraceFrame)
            .filter(TraceFrame.run_id == self.current_run_id)
            .filter(
                TraceFrame.caller != TraceFrame.callee
            )  # skip recursive calls for now
            .filter(TraceFrame.kind == trace_frame.kind)
        )
        if backwards:
            query = query.filter(TraceFrame.callee == trace_frame.callee).filter(
                TraceFrame.callee_port == trace_frame.callee_port
            )
        else:
            query = query.filter(TraceFrame.caller == trace_frame.callee).filter(
                TraceFrame.caller_port == trace_frame.callee_port
            )

        results = (
            query.join(TraceFrame.leaf_assoc)
            .group_by(TraceFrame.id)
            .order_by(TraceFrameLeafAssoc.trace_length, TraceFrame.callee_location)
        )
        filter_leaves = (
            self.sources if trace_frame.kind == TraceKind.POSTCONDITION else self.sinks
        )
        filtered_results = [
            frame
            for frame in results
            if filter_leaves.intersection({leaf.contents for leaf in frame.leaves})
        ]

        return filtered_results

    def _create_issue_output_string(
        self, issue: IssueQueryResult, sources: List[str], sinks: List[str]
    ) -> str:
        sources_output = f"\n{' ' * 10}".join(sources)
        sinks_output = f"\n{' ' * 10}".join(sinks)
        return "\n".join(
            [
                f"Issue {issue.id}",
                f"    Code: {issue.code}",
                f" Message: {issue.contents}",
                f"Callable: {issue.callable}",
                f" Sources: {sources_output if sources_output else 'No sources'}",
                f"   Sinks: {sinks_output if sinks_output else 'No sinks'}",
                (f"Location: {issue.filename}" f":{issue.location}"),
            ]
        )

    def _create_trace_frame_output_string(self, trace_frame):
        if trace_frame.kind == TraceKind.POSTCONDITION:
            leaves_label = "Sources"
            leaf_kind = SharedTextKind.SOURCE
        elif trace_frame.kind == TraceKind.PRECONDITION:
            leaves_label = "Sinks"
            leaf_kind = SharedTextKind.SINK
        else:
            assert False, f"{trace_frame.kind} is not valid."

        with self.db.make_session() as session:
            leaves_output = f"\n{' ' * 13}".join(
                self._get_leaves_trace_frame(session, trace_frame.id, leaf_kind)
            )

        return "\n".join(
            [
                f"Trace frame {trace_frame.id or 'placeholder'}",
                f"     Caller: {trace_frame.caller} : {trace_frame.caller_port}",
                f"     Callee: {trace_frame.callee} : {trace_frame.callee_port}",
                f"       Kind: {trace_frame.kind}",
                f"{leaves_label:>{11}}: {leaves_output}",
                (
                    f"   Location: {trace_frame.filename}"
                    f":{trace_frame.callee_location}"
                ),
            ]
        )

    def _select_parent_trace_frame(
        self, parent_trace_frames: List[TraceFrame]
    ) -> TraceFrame:
        for i, parent in enumerate(parent_trace_frames):
            print(f"[{i + 1}] {parent.caller} : {parent.caller_port}")
        parent_number = self._prompt_for_number(
            "Parent number", len(parent_trace_frames)
        )
        return parent_trace_frames[parent_number - 1]

    def _select_branch_trace_frame(
        self, branch_trace_frames: List[TraceFrame], leaves_string: List[str]
    ) -> int:
        self._output_trace_expansion(branch_trace_frames, leaves_string)
        return self._prompt_for_number("Branch number", len(branch_trace_frames))

    def _prompt_for_number(self, prefix: str, max_valid: int) -> int:
        try:
            selected_number = click.prompt(
                f"{prefix} (1-{max_valid}, ctrl-D to abort)", type=int
            )
            if selected_number < 1 or selected_number > max_valid:
                raise UserError("Out of bounds.")
            print()
            return selected_number
        except click.Abort:
            raise KeyboardInterrupt()

    def _update_trace_tuples_new_parent(self, parent_trace_frame: TraceFrame) -> None:
        # Construct the placeholder trace tuple and the actual one.
        new_head = [
            TraceTuple(
                trace_frame=TraceFrame(
                    caller=None,
                    caller_port=None,
                    callee=parent_trace_frame.caller,
                    callee_port=parent_trace_frame.caller_port,
                    filename=parent_trace_frame.filename,
                    callee_location=parent_trace_frame.callee_location,
                    kind=parent_trace_frame.kind,
                ),
                placeholder=True,
            ),
            TraceTuple(trace_frame=parent_trace_frame),
        ]

        if parent_trace_frame.kind == TraceKind.POSTCONDITION:
            # If current state is: C in [A,B,C,D,E]
            # Then new state is:   [A,B] + new_tail
            new_tail = new_head[::-1]
            self.trace_tuples = (
                self.trace_tuples[: self.current_trace_frame_index] + new_tail
            )
            self.current_trace_frame_index = len(self.trace_tuples) - 1
            return

        # If current state is: C in [A,B,C,D,E]
        # Then new state is:   new_head + [D,E]
        self.trace_tuples = (
            new_head + self.trace_tuples[self.current_trace_frame_index + 1 :]
        )
        self.current_trace_frame_index = 0

    def _resolve_pager(self, use_pager):
        use_pager = sys.stdin.isatty() if use_pager is None else use_pager
        return page.page if use_pager else page.display_page

    def _get_current_issue(self, session: Session) -> IssueQueryResult:
        return (
            session.query(
                IssueInstance.id,
                IssueInstance.filename,
                IssueInstance.location,
                Issue.code,
                Issue.callable,
                SharedText.contents,
            )
            .filter(IssueInstance.id == self.current_issue_instance_id)
            .join(Issue, IssueInstance.issue_id == Issue.id)
            .join(SharedText, SharedText.id == IssueInstance.message_id)
            .first()
        )

    def _get_leaves_issue_instance(
        self, session: Session, issue_instance_id: int, kind: SharedTextKind
    ) -> List[str]:
        message_ids = [
            int(id)
            for id, in session.query(SharedText.id)
            .join(
                IssueInstanceSharedTextAssoc,
                SharedText.id == IssueInstanceSharedTextAssoc.shared_text_id,
            )
            .filter(IssueInstanceSharedTextAssoc.issue_instance_id == issue_instance_id)
            .filter(SharedText.kind == kind)
        ]
        return self._leaf_dict_lookups(message_ids, kind)

    def _get_leaves_trace_frame(
        self, session: Session, trace_frame_id: int, kind: SharedTextKind
    ) -> List[str]:
        message_ids = [
            int(id)
            for id, in session.query(SharedText.id)
            .join(TraceFrameLeafAssoc, SharedText.id == TraceFrameLeafAssoc.leaf_id)
            .filter(TraceFrameLeafAssoc.trace_frame_id == trace_frame_id)
            .filter(SharedText.kind == kind)
        ]
        return self._leaf_dict_lookups(message_ids, kind)

    def _leaf_dict_lookups(
        self, message_ids: List[int], kind: SharedTextKind
    ) -> List[str]:
        leaf_dict = (
            self.sources_dict if kind == SharedTextKind.SOURCE else self.sinks_dict
        )
        return [leaf_dict[id] for id in message_ids if id in leaf_dict]

    def _show_current_issue_instance(self):
        with self.db.make_session() as session:
            issue = self._get_current_issue(session)

        page.display_page(
            self._create_issue_output_string(issue, self.sources, self.sinks)
        )

    def _show_current_trace_frame(self):
        with self.db.make_session() as session:
            trace_frame = (
                session.query(TraceFrame)
                .filter(TraceFrame.id == self.current_frame_id)
                .scalar()
            )

        page.display_page(self._create_trace_frame_output_string(trace_frame))

    def _verify_entrypoint_selected(self) -> None:
        assert self.current_issue_instance_id == -1 or self.current_frame_id == -1

        if self.current_issue_instance_id == -1 and self.current_frame_id == -1:
            raise UserError(
                "Use 'set_issue_instance(ID)' or 'set_frame(ID)' to select an"
                " entrypoint first."
            )

    def _verify_multiple_branches(self) -> None:
        current_trace_tuple = self.trace_tuples[self.current_trace_frame_index]
        if current_trace_tuple.branches < 2:
            raise UserError("This trace frame has no alternate branches to take.")

    def _all_leaves_by_kind(
        self, session: Session, kind: SharedTextKind
    ) -> Dict[int, str]:
        return {
            int(id): contents
            for id, contents in session.query(
                SharedText.id, SharedText.contents
            ).filter(SharedText.kind == kind)
        }

    def _num_issues_with_callable(self, callable: str) -> int:
        with self.db.make_session() as session:
            return (
                session.query(func.count(IssueInstance.id))
                .join(Issue, Issue.id == IssueInstance.issue_id)
                .filter(Issue.callable == callable)
                .scalar()
            )
