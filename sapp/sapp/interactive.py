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

from IPython.core import page
from pygments import highlight
from pygments.formatters import TerminalFormatter
from pygments.lexers import get_lexer_for_filename
from sapp.db import DB
from sapp.decorators import UserError, catch_user_error
from sapp.models import (
    Issue,
    IssueInstance,
    Run,
    RunStatus,
    SharedText,
    SharedTextKind,
    Sink,
    Source,
    SourceLocation,
    TraceFrame,
    TraceFrameLeafAssoc,
    TraceKind,
)
from sqlalchemy import distinct
from sqlalchemy.orm import Session, joinedload
from sqlalchemy.orm.attributes import InstrumentedAttribute
from sqlalchemy.orm.query import Query
from sqlalchemy.sql import func
from sqlalchemy.sql.expression import or_


class TraceTuple(NamedTuple):
    trace_frame: TraceFrame
    branches: int = 1
    missing: bool = False


class ListFilterException(Exception):
    pass


class Interactive:
    # @lint-ignore FBPYTHON2
    list_string = "list()"
    help_message = f"""
Commands =======================================================================

commands()      show this message
help(COMAMND)   more info about a command
state()         show the internal state of the tool for debugging

runs()          list all completed static analysis runs
set_run(ID)     select a specific run for browsing issues
issues()        list all issues for the selected run
set_issue(ID)   select a specific issue for browsing a trace
show()          show info about selected issue or trace frame
trace()         show a trace of the selected issue or trace frame
prev()/p()      move backward within the trace
next()/n()      move forward within the trace
expand()        show alternative trace branches
branch(INDEX)   select a trace branch
{list_string}          show source code at the current trace frame

frames()        show trace frames independently of an issue
set_frame(ID)   select a trace frame to explore
"""
    welcome_message = "Interactive issue exploration. Type 'commands()' for help."

    LEAF_NAMES = {"source", "sink", "leaf"}

    def __init__(self, database, database_name, repository_directory: str = ""):
        self.db = DB(database, database_name, assertions=True)
        self.scope_vars: Dict[str, Union[Callable, TraceKind]] = {
            "precondition": TraceKind.PRECONDITION,
            "postcondition": TraceKind.POSTCONDITION,
            "commands": self.help,
            "state": self.state,
            "runs": self.runs,
            "issues": self.issues,
            "set_run": self.set_run,
            "set_issue": self.set_issue,
            "show": self.show,
            "trace": self.trace,
            "next": self.next_cursor_location,
            "n": self.next_cursor_location,
            "prev": self.prev_cursor_location,
            "p": self.prev_cursor_location,
            "expand": self.expand,
            "branch": self.branch,
            "list": self.list_source_code,
            "frames": self.frames,
            "set_frame": self.set_frame,
        }
        self.repository_directory = repository_directory or os.getcwd()

        self.current_run_id: int = -1

        # Trace exploration relies on either of these
        self.current_issue_id: int = -1
        self.current_frame_id: int = -1

        self.sources: Set[str] = set()
        self.sinks: Set[str] = set()
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
        print(f"            Database: {self.db.dbtype}:{self.db.dbname}")
        print(f"Repository directory: {self.repository_directory}")
        print(f"         Current run: {self.current_run_id}")
        print(f"       Current issue: {self.current_issue_id}")
        print(f" Current trace frame: {self.current_frame_id}")
        print(f"      Sources filter: {self.sources}")
        print(f"        Sinks filter: {self.sinks}")

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

            self.sources = set(
                self._get_leaves(session, selected_issue, SharedTextKind.SOURCE)
            )
            self.sinks = set(
                self._get_leaves(session, selected_issue, SharedTextKind.SINK)
            )

        self.current_issue_id = selected_issue.id
        self.current_frame_id = -1
        self.current_trace_frame_index = 1  # first one after the source

        self._generate_trace_from_issue()

        print(f"Set issue to {issue_id}.")
        self.show()

    @catch_user_error()
    def show(self):
        """ More details about the selected issue or trace frame.
        """
        self._verify_entrypoint_selected()

        if self.current_issue_id != -1:
            self._show_current_issue_instance()
            return

        self._show_current_trace_frame()

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
            query = (
                session.query(IssueInstance, Issue)
                .filter(IssueInstance.run_id == self.current_run_id)
                .join(Issue, IssueInstance.issue_id == Issue.id)
            )

            if codes is not None:
                self._verify_list_filter(codes, "codes")
                query = query.filter(Issue.code.in_(codes))

            if callables is not None:
                self._verify_list_filter(callables, "callables")
                query = self._add_list_filter_to_query(callables, query, Issue.callable)

            if filenames is not None:
                self._verify_list_filter(filenames, "filenames")
                query = self._add_list_filter_to_query(filenames, query, Issue.filename)

            issues = query.options(joinedload(IssueInstance.message)).all()
            sources_list = [
                self._get_leaves(session, issue_instance, SharedTextKind.SOURCE)
                for issue_instance, _ in issues
            ]
            sinks_list = [
                self._get_leaves(session, issue_instance, SharedTextKind.SINK)
                for issue_instance, _ in issues
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

    @catch_user_error()
    def frames(
        self, *, callers: Optional[List[str]] = None, kind: Optional[TraceKind] = None
    ):
        """Display trace frames independent of the current issue.

        Parameters (all optional):
            callers: list[str]                  filter traces by this caller name
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

            if kind is not None:
                if kind not in {TraceKind.PRECONDITION, TraceKind.POSTCONDITION}:
                    raise UserError(
                        "Try 'frames(kind=postcondition)'"
                        " or 'frames(kind=precondition)'."
                    )
                query = query.filter(TraceFrame.kind == kind)

            trace_frames = (
                query.group_by(TraceFrame.id)
                .order_by(TraceFrame.caller, TraceFrame.callee)
                .all()
            )

            self._output_trace_frames(self._group_trace_frames(trace_frames))

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
        self.current_issue_id = -1

        self._generate_trace_from_frame()

        print(f"Set trace frame to {frame_id}.")
        self.show()

    def _generate_trace_from_issue(self):
        with self.db.make_session() as session:
            issue_instance, issue = self._get_current_issue(session)

            postcondition_navigation = self._navigate_trace_frames(
                session,
                self._initial_trace_frames(
                    session, issue_instance.id, TraceKind.POSTCONDITION
                ),
            )
            precondition_navigation = self._navigate_trace_frames(
                session,
                self._initial_trace_frames(
                    session, issue_instance.id, TraceKind.PRECONDITION
                ),
            )

        self.trace_tuples = (
            self._create_trace_tuples(reversed(postcondition_navigation))
            + [
                TraceTuple(
                    trace_frame=TraceFrame(
                        callee=issue.callable,
                        callee_port="root",
                        filename=issue_instance.filename,
                        callee_location=issue_instance.location,
                    )
                )
            ]
            + self._create_trace_tuples(precondition_navigation)
        )
        self.trace_tuples_id = self.current_issue_id
        self.current_trace_frame_index = len(postcondition_navigation)

    def _generate_trace_from_frame(self):
        with self.db.make_session() as session:
            trace_frame = (
                session.query(TraceFrame)
                .filter(TraceFrame.id == self.current_frame_id)
                .scalar()
            )

            navigation = self._navigate_trace_frames(session, [trace_frame])

        self.current_trace_frame_index = 0
        if trace_frame.kind == TraceKind.POSTCONDITION:
            self.current_trace_frame_index = len(navigation) - 1
            navigation = reversed(navigation)

        self.trace_tuples = self._create_trace_tuples(navigation)

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
    def expand(self):
        """Show and select branches for a branched trace.
        - [*] signifies the current branch that is selected

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

        current_trace_tuple = self.trace_tuples[self.current_trace_frame_index]
        filter_leaves = (
            self.sources
            if current_trace_tuple.trace_frame.kind == TraceKind.POSTCONDITION
            else self.sinks
        )

        with self.db.make_session() as session:
            branches = self._get_trace_frame_branches(session)
            leaves_strings = [
                ", ".join(
                    [
                        leaf.contents
                        for leaf in frame.leaves
                        if leaf.contents in filter_leaves
                    ]
                )
                for frame in branches
            ]
            self._output_trace_expansion(branches, leaves_strings)

    @catch_user_error()
    def branch(self, selected_index: int) -> None:
        """Selects a branch when there are multiple possible traces to follow.

        The trace output that follows includes the new branch and its children
        frames.

        Parameters:
            selected_index: int    branch index from expand() output
        """
        self._verify_entrypoint_selected()
        self._verify_multiple_branches()

        with self.db.make_session() as session:
            branches = self._get_trace_frame_branches(session)

            if selected_index < 0 or selected_index >= len(branches):
                self.warning(
                    "Branch index out of bounds "
                    f"(expected 0-{len(branches) - 1} but got {selected_index})."
                )
                return

            new_navigation = self._navigate_trace_frames(
                session, branches, selected_index
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

    def warning(self, message: str) -> None:
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
            return self._initial_trace_frames(session, self.current_issue_id, kind)

        parent_trace_frame = self.trace_tuples[parent_index].trace_frame
        return self._next_trace_frames(session, parent_trace_frame)

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
        self, trace_frames: List[TraceFrame]
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
                "[*]" if i == self._current_branch_index(trace_frames) else f"[{i}]"
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
            f"{'⎇':{max_length_split}}"
            f" {'[callable]':{max_length_callable}}"
            f" {'[port]':{max_length_condition}}"
            f" [location]"
        )

        for i, trace_tuple in enumerate(trace_tuples):
            prefix = "-->" if i == self.current_trace_frame_index else " " * 3

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
                    f" {branches_string}"
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
            .filter(TraceFrame.issue_instances.any(id=issue_instance_id))
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
        while not self._is_leaf(trace_frames[-1]):
            trace_frame, branches = trace_frames[-1]
            next_nodes = self._next_trace_frames(session, trace_frame)

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

    def _is_leaf(self, node: Tuple[TraceFrame, int]) -> bool:
        trace_frame, branches = node
        return trace_frame.callee_port in self.LEAF_NAMES

    def _next_trace_frames(self, session, trace_frame):
        results = (
            session.query(TraceFrame)
            .filter(TraceFrame.run_id == self.current_run_id)
            .filter(
                TraceFrame.caller != TraceFrame.callee
            )  # skip recursive calls for now
            .filter(TraceFrame.caller == trace_frame.callee)
            .filter(TraceFrame.caller_port == trace_frame.callee_port)
            .join(TraceFrame.leaf_assoc)
            .group_by(TraceFrame.id)
            .order_by(TraceFrameLeafAssoc.trace_length, TraceFrame.callee_location)
            .all()
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

    def _create_issue_output_string(self, issue_instance, issue, sources, sinks):
        sources_output = f"\n{' ' * 10}".join(sources)
        sinks_output = f"\n{' ' * 10}".join(sinks)
        return "\n".join(
            [
                f"Issue {issue_instance.id}",
                f"    Code: {issue.code}",
                f" Message: {issue_instance.message.contents}",
                f"Callable: {issue.callable}",
                f" Sources: {sources_output if sources_output else 'No sources'}",
                f"   Sinks: {sinks_output if sinks_output else 'No sinks'}",
                (f"Location: {issue_instance.filename}" f":{issue_instance.location}"),
            ]
        )

    def _create_trace_frame_output_string(self, trace_frame):
        leaves_label = (
            "Sinks" if trace_frame.kind == TraceKind.PRECONDITION else "Sources"
        )
        leaves_output = f"\n{' ' * 13}".join(
            [leaf.contents for leaf in trace_frame.leaves]
        )
        return "\n".join(
            [
                f"Trace frame {trace_frame.id}",
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

    def _resolve_pager(self, use_pager):
        use_pager = sys.stdin.isatty() if use_pager is None else use_pager
        return page.page if use_pager else page.display_page

    def _get_current_issue(self, session):
        return (
            session.query(IssueInstance, Issue)
            .filter(IssueInstance.id == self.current_issue_id)
            .join(Issue, IssueInstance.issue_id == Issue.id)
            .options(joinedload(IssueInstance.message))
            .first()
        )

    def _get_leaves(
        self, session: Session, issue_instance: IssueInstance, kind: SharedTextKind
    ) -> List[str]:
        return [
            leaf
            for leaf, in session.query(distinct(SharedText.contents))
            .join(SharedText.shared_text_issue_instance)
            .filter(SharedText.issue_instances.any(id=issue_instance.id))
            .filter(SharedText.kind == kind)
            .all()
        ]

    def _show_current_issue_instance(self):
        with self.db.make_session() as session:
            issue_instance, issue = self._get_current_issue(session)
            sources = self._get_leaves(session, issue_instance, SharedTextKind.SOURCE)
            sinks = self._get_leaves(session, issue_instance, SharedTextKind.SINK)

        page.display_page(
            self._create_issue_output_string(issue_instance, issue, sources, sinks)
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
        assert self.current_issue_id == -1 or self.current_frame_id == -1

        if self.current_issue_id == -1 and self.current_frame_id == -1:
            raise UserError(
                "Use 'set_issue(ID)' or 'set_frame(ID)' to select an"
                " entrypoint first."
            )

    def _verify_multiple_branches(self) -> None:
        current_trace_tuple = self.trace_tuples[self.current_trace_frame_index]
        if current_trace_tuple.branches < 2:
            raise UserError("This trace frame has no alternate branches to take.")
