#!/usr/bin/env python3

import builtins
import itertools
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
    Type,
    TypeVar,
    Union,
)

import click
import IPython
from IPython import paths
from IPython.core import page
from prompt_toolkit import prompt
from prompt_toolkit.auto_suggest import AutoSuggestFromHistory
from prompt_toolkit.contrib.completers import PathCompleter
from prompt_toolkit.history import FileHistory, History
from pygments import highlight
from pygments.formatters import TerminalFormatter
from pygments.lexers import get_lexer_for_filename
from sqlalchemy.orm import Session, aliased
from sqlalchemy.orm.attributes import InstrumentedAttribute
from sqlalchemy.orm.query import Query
from sqlalchemy.sql import func
from sqlalchemy.sql.expression import or_

from .analysis_output import AnalysisOutput, AnalysisOutputError
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


T = TypeVar("T")

FilenameText = aliased(SharedText)
CallableText = aliased(SharedText)
CallerText = aliased(SharedText)
CalleeText = aliased(SharedText)
MessageText = aliased(SharedText)


class IssueQueryResult(NamedTuple):
    id: DBID
    filename: str
    location: SourceLocation
    code: int
    callable: str
    message: str


class TraceFrameQueryResult(NamedTuple):
    id: DBID
    caller: str
    caller_port: str
    callee: str
    callee_port: str
    caller_id: Optional[DBID] = None
    callee_id: Optional[DBID] = None
    callee_location: Optional[SourceLocation] = None
    kind: Optional[TraceKind] = None
    filename: Optional[str] = None
    trace_length: Optional[int] = None


class TraceTuple(NamedTuple):
    trace_frame: TraceFrameQueryResult
    branches: int = 1
    missing: bool = False
    # Suppose we select a trace frame (A->B) and the generated trace is
    #   (A->B), (B->C), (C->D) with D as leaf.
    # When we display traces, we only use the callee, so this trace would look
    #   like B->C->D. If we also want to see A->, then we need to add a
    #   placeholder trace tuple. We do this by setting our trace tuples to
    #   [(A->B, placeholder=True), (A->B), (B->C), (C->D)]. When placeholder is
    #   True, that means we need to output the caller rather than the callee.
    placeholder: bool = False


class ListFilterException(Exception):
    pass


class Interactive:
    help_message = f"""
Commands =======================================================================

== Information commands ==
help                 show this message
help COMMAND         more info about a command
state                show the internal state of the tool for debugging

== Display commands ==
runs                 list all completed static analysis runs
issues               list all issues for the selected run
frames               show trace frames independently of an issue
show                 show info about selected issue or trace frame

== Selection commands ==
analysis_output DIR  sets the location of the analysis output
run ID               select a specific run for browsing issues
latest_run KIND      sets run to the latest of the specified kind
issue ID             select a specific issue for browsing a trace
frame ID             select a trace frame to explore

== Trace commands ==
trace                show a trace of the selected issue or trace frame
prev/p               move backward within the trace
next/n               move forward within the trace
jump NUM             jump to a specific trace frame in a trace
branch INDEX         select a trace branch
list                 show source code at the current trace frame

== Debugging commands ==
parents              show trace frames that call the current trace frame
details              show additional information about the current trace frame
"""
    welcome_message = "Interactive issue exploration. Type 'help' for help."

    LEAF_NAMES = {"source", "sink", "leaf"}

    SELF_SCOPE_KEY = "_interactive"

    def __init__(self, database: DB, repository_directory: str = ""):
        self.db = database
        self.scope_vars: Dict[str, Union[Callable, TraceKind]] = {
            "precondition": TraceKind.PRECONDITION,
            "postcondition": TraceKind.POSTCONDITION,
            "help": self.help,
            "state": self.state,
            "runs": self.runs,
            "issues": self.issues,
            "run": self.run,
            "latest_run": self.latest_run,
            "issue": self.issue,
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
            "frame": self.frame,
            "parents": self.parents,
            "details": self.details,
            "analysis_output": self.analysis_output,
            "callable": self.callable,
            self.SELF_SCOPE_KEY: self,
        }
        self.repository_directory = repository_directory or os.getcwd()
        self.current_analysis_output: Optional[AnalysisOutput] = None

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

        # Persist history for prompts that opt-into it, by specifying
        # history_key on self.prompt().
        self.prompt_history: Dict[str, History] = {}

    def setup(self) -> Dict[str, Callable]:
        with self.db.make_session() as session:
            latest_run_id = (
                session.query(func.max(Run.id))
                .filter(Run.status == RunStatus.FINISHED)
                .scalar()
            )

            self.sources_dict = self._all_leaves_by_kind(session, SharedTextKind.SOURCE)
            self.sinks_dict = self._all_leaves_by_kind(session, SharedTextKind.SINK)

        print("=" * len(self.welcome_message))
        print(self.welcome_message)
        print("=" * len(self.welcome_message))

        if latest_run_id.resolved() is None:
            self.warning(
                "No runs found. "
                f"Try running '{os.path.basename(sys.argv[0])} analyze' first."
            )
        else:
            self.current_run_id = int(latest_run_id)

        return self.scope_vars

    def help(self, object=None):
        if object is None:
            print(self.help_message)
            return

        builtins.help(object)

    def state(self):
        print(f"              Database: {self.db.dbtype}:{self.db.dbname}")
        print(f"       Analysis Output: {self.current_analysis_output}")
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
    def run(self, run_id):
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
                "Type 'runs' for available runs."
            )
            return

        self.current_run_id = int(selected_run.id)
        print(f"Set run to {run_id}.")

    def _get_profile_basedir(self) -> str:
        profile_name = IPython.get_ipython().profile
        return paths.locate_profile(profile=profile_name)

    def _get_prompt_history(self, key: Optional[str]) -> Optional[History]:
        if key is None:
            return None

        history = self.prompt_history.get(key)
        if not history:
            basedir = os.path.join(self._get_profile_basedir(), "prompt_history")
            os.makedirs(basedir, exist_ok=True)
            history = FileHistory(os.path.join(basedir, key))
            self.prompt_history[key] = history
        return history

    def prompt(
        self,
        message: Optional[str] = "",
        history_key: Optional[str] = None,
        *args,
        **kwargs,
    ) -> str:
        try:
            ret = prompt(
                message,
                history=self._get_prompt_history(history_key),
                auto_suggest=AutoSuggestFromHistory(),
                *args,
                **kwargs,
            )
            if ret == "":
                raise KeyboardInterrupt()
            return ret
        except EOFError:
            raise KeyboardInterrupt()

    @catch_keyboard_interrupt()
    @catch_user_error()
    def analysis_output(self, location: Optional[str] = None) -> None:
        """Sets the location of the output from the static analysis tool.

        Parameters:
            location: str   Filesystem location for the results.
        """
        try:
            if not location:
                location = self.prompt(
                    "Analysis results: ",
                    history_key="analysis_results",
                    completer=PathCompleter(),
                )
            self.current_analysis_output = AnalysisOutput.from_str(location)
        except AnalysisOutputError as e:
            raise UserError(f"Error loading results: {e}")

    @catch_user_error()
    def latest_run(self, run_kind: str) -> None:
        """Sets the current run to the latest run of a given kind.

        Parameters (required):
            run_kind: str    the run kind to filter by

        Example:
            'latest_run "master"' will set the current run to the latest
            run whose kind field is "master"
        """
        if not run_kind or not isinstance(run_kind, str):
            raise UserError("Please provide a non-empty string for 'run_kind'.")

        with self.db.make_session() as session:
            selected_run_id = (
                session.query(func.max(Run.id))
                .filter(Run.kind == run_kind)
                .filter(Run.status == RunStatus.FINISHED)
                .scalar()
            )

        if selected_run_id.resolved() is None:
            raise UserError(f"No runs with kind '{run_kind}'.")

        self.current_run_id = int(selected_run_id)
        print(f"Set run to {self.current_run_id}.")

    @catch_keyboard_interrupt()
    def issue(self, issue_instance_id):
        """Select an issue.

        Parameters:
            issue_instance_id: int    id of the issue instance to select

        Note: We are selecting issue instances, even though the command is called
        issue.
        """
        with self.db.make_session() as session:
            selected_issue = (
                session.query(IssueInstance)
                .filter(IssueInstance.id == issue_instance_id)
                .scalar()
            )

            if selected_issue is None:
                self.warning(
                    f"Issue {issue_instance_id} doesn't exist. "
                    "Type 'issues' for available issues."
                )
                return

            self.sources = self._get_leaves_issue_instance(
                session, issue_instance_id, SharedTextKind.SOURCE
            )

            self.sinks = self._get_leaves_issue_instance(
                session, issue_instance_id, SharedTextKind.SINK
            )

        self.current_issue_instance_id = int(selected_issue.id)
        self.current_frame_id = -1
        self.current_trace_frame_index = 1  # first one after the source

        print(f"Set issue to {issue_instance_id}.")
        if int(selected_issue.run_id) != self.current_run_id:
            self.current_run_id = int(selected_issue.run_id)
            print(f"Set run to {self.current_run_id}.")
        print()

        self._generate_trace_from_issue()
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
        codes: Optional[Union[int, List[int]]] = None,
        callables: Optional[Union[str, List[str]]] = None,
        filenames: Optional[Union[str, List[str]]] = None,
    ):
        """Lists issues for the selected run.

        Parameters (all optional):
            use_pager: bool                use a unix style pager for output
            codes: int or list[int]        issue codes to filter on
            callables: str or list[str]    callables to filter on (supports wildcards)
            filenames: str or list[str]    filenames to filter on (supports wildcards)

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
                session.query(
                    IssueInstance.id,
                    FilenameText.contents.label("filename"),
                    IssueInstance.location,
                    Issue.code,
                    CallableText.contents.label("callable"),
                    MessageText.contents.label("message"),
                )
                .filter(IssueInstance.run_id == self.current_run_id)
                .join(FilenameText, FilenameText.id == IssueInstance.filename_id)
                .join(CallableText, CallableText.id == IssueInstance.callable_id)
            )

            if codes is not None:
                query = self._add_list_or_int_filter_to_query(
                    codes, query, Issue.code, "codes"
                )

            if callables is not None:
                query = self._add_list_or_string_filter_to_query(
                    callables, query, CallableText.contents, "callables"
                )

            if filenames is not None:
                query = self._add_list_or_string_filter_to_query(
                    filenames, query, FilenameText.contents, "filenames"
                )

            issues = query.join(Issue, IssueInstance.issue_id == Issue.id).join(
                MessageText, MessageText.id == IssueInstance.message_id
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
        callers: Optional[Union[str, List[str]]] = None,
        callees: Optional[Union[str, List[str]]] = None,
        kind: Optional[TraceKind] = None,
        limit: Optional[int] = 10,
    ):
        """Display trace frames independent of the current issue.

        Parameters (all optional):
            callers: str or list[str]            filter traces by this caller name
            callees: str or list[str]            filter traces by this callee name
            kind: precondition|postcondition    the type of trace frames to show
            limit: int (default: 10)            how many trace frames to display
                                                (specify limit=None for all)

        Sample usage:
            frames callers="module.function", kind=postcondition

        String filters support LIKE wildcards (%, _) from SQL:
            % matches anything (like .* in regex)
            _ matches 1 character (like . in regex)
        """
        with self.db.make_session() as session:
            query = (
                session.query(
                    TraceFrame.id,
                    CallerText.contents.label("caller"),
                    TraceFrame.caller_port,
                    CalleeText.contents.label("callee"),
                    TraceFrame.callee_port,
                )
                .filter(TraceFrame.run_id == self.current_run_id)
                .join(CallerText, CallerText.id == TraceFrame.caller_id)
                .join(CalleeText, CalleeText.id == TraceFrame.callee_id)
            )

            if callers is not None:
                query = self._add_list_or_string_filter_to_query(
                    callers, query, CallerText.contents, "callers"
                )

            if callees is not None:
                query = self._add_list_or_string_filter_to_query(
                    callees, query, CalleeText.contents, "callees"
                )

            if kind is not None:
                if kind not in {TraceKind.PRECONDITION, TraceKind.POSTCONDITION}:
                    raise UserError(
                        "Try 'frames kind=postcondition'"
                        " or 'frames kind=precondition'."
                    )
                query = query.filter(TraceFrame.kind == kind)

            if limit is not None and not isinstance(limit, int):
                raise UserError("'limit' should be an int or None.")

            trace_frames = query.group_by(TraceFrame.id).order_by(
                CallerText.contents, CalleeText.contents
            )

            total_trace_frames = trace_frames.count()
            limit = limit or total_trace_frames

            self._output_trace_frames(
                self._group_trace_frames(trace_frames, limit), limit, total_trace_frames
            )

    @catch_keyboard_interrupt()
    def frame(self, frame_id: int) -> None:
        with self.db.make_session() as session:
            selected_frame = (
                session.query(TraceFrame.id, TraceFrame.kind, TraceFrame.run_id)
                .filter(TraceFrame.id == frame_id)
                .first()
            )

            if selected_frame is None:
                self.warning(
                    f"Trace frame {frame_id} doesn't exist. "
                    "Type 'frames' for available trace frames."
                )
                return

            if selected_frame.kind == TraceKind.POSTCONDITION:
                self.sinks = set()
                self.sources = self._get_leaves_trace_frame(
                    session, int(selected_frame.id), SharedTextKind.SOURCE
                )

            else:
                self.sinks = self._get_leaves_trace_frame(
                    session, int(selected_frame.id), SharedTextKind.SINK
                )

                self.sources = set()

        self.current_frame_id = int(selected_frame.id)
        self.current_issue_instance_id = -1

        print(f"Set trace frame to {frame_id}.")
        if int(selected_frame.run_id) != self.current_run_id:
            self.current_run_id = int(selected_frame.run_id)
            print(f"Set run to {self.current_run_id}.")
        print()

        self._generate_trace_from_frame()
        self.show()

    @catch_keyboard_interrupt()
    @catch_user_error()
    def parents(self) -> None:
        self._verify_entrypoint_selected()
        current_trace_tuple = self.trace_tuples[self.current_trace_frame_index]

        # Don't allow calling from the leaf node in a trace. Instead, call
        # parents() from the placeholder of the caller of the leaf node.
        if self._is_leaf(current_trace_tuple.trace_frame):
            raise UserError("Try running from a non-leaf node.")

        # Backwards trace checks against the given frame's _caller_.
        # If we have A->B as a placeholder, we want parents that call A, since A
        #   is what is displayed in the trace.
        # If we have A->B as a non-placeholder, we want parents that call B,
        #   since B is what is displayed in the trace. So we go to (A->B)'s child
        #   B->C, and look for frames that call B.
        if not current_trace_tuple.placeholder:
            delta_from_child = -1 if self._is_before_root() else 1
            current_trace_tuple = self.trace_tuples[
                self.current_trace_frame_index + delta_from_child
            ]

        with self.db.make_session() as session:
            parent_trace_frames = self._next_backward_trace_frames(
                session, current_trace_tuple.trace_frame, set()
            )

        if len(parent_trace_frames) == 0:
            print(
                f"No parents calling [{current_trace_tuple.trace_frame.caller} "
                f": {current_trace_tuple.trace_frame.caller_port}]."
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
                    trace_frame=TraceFrameQueryResult(
                        id=DBID(0),
                        caller="",
                        caller_port="",
                        callee=issue.callable,
                        callee_port="root",
                        filename=issue.filename,
                        callee_location=issue.location,
                    )
                )
            ]
            + self._create_trace_tuples(precondition_navigation)
        )
        self.trace_tuples_id = self.current_issue_instance_id
        self.current_trace_frame_index = len(postcondition_navigation)

    def _generate_trace_from_frame(self) -> None:
        with self.db.make_session() as session:
            trace_frame = (
                session.query(
                    TraceFrame.id,
                    TraceFrame.caller_id,
                    CallerText.contents.label("caller"),
                    TraceFrame.caller_port,
                    TraceFrame.callee_id,
                    CalleeText.contents.label("callee"),
                    TraceFrame.callee_port,
                    TraceFrame.callee_location,
                    TraceFrame.kind,
                    FilenameText.contents.label("filename"),
                )
                .filter(TraceFrame.id == self.current_frame_id)
                .join(CallerText, CallerText.id == TraceFrame.caller_id)
                .join(CalleeText, CalleeText.id == TraceFrame.callee_id)
                .join(FilenameText, FilenameText.id == TraceFrame.filename_id)
                .one()
            )

            navigation = self._navigate_trace_frames(session, [trace_frame])

        first_trace_frame = navigation[0][0]
        placeholder_tuple = [
            TraceTuple(trace_frame=first_trace_frame, placeholder=True)
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
            selected_number: int    the trace frame number from trace output
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
    ) -> Tuple[List[TraceFrameQueryResult], List[str]]:
        current_trace_tuple = self.trace_tuples[self.current_trace_frame_index]
        filter_leaves = (
            self.sources
            if current_trace_tuple.trace_frame.kind == TraceKind.POSTCONDITION
            else self.sinks
        )

        branches = self._get_trace_frame_branches(session)

        leaves_strings = []
        for frame in branches:
            leaves_strings.append(
                ", ".join(
                    [
                        leaf
                        for leaf in self._get_leaves_trace_frame(
                            session,
                            int(frame.id),
                            self._trace_kind_to_shared_text_kind(frame.kind),
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
            selected_number: int    branch number from expand output

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

    def details(
        self, *, limit: Optional[int] = 5, kind: Optional[TraceKind] = None
    ) -> None:
        """Show additional info about the current trace frame.

        Parameters (all optional):
            limit: int    number of related post/pre conditions to show
                          (pass limit=None to show all)
        """
        self._verify_entrypoint_selected()
        if limit is not None and not isinstance(limit, int):
            raise UserError("'limit' should be an int or None.")
        if kind not in {TraceKind.PRECONDITION, TraceKind.POSTCONDITION, None}:
            raise UserError(
                "Try 'details kind=postcondition'" " or 'details kind=precondition'."
            )

        current_trace_tuple = self.trace_tuples[self.current_trace_frame_index]
        current_trace_frame = current_trace_tuple.trace_frame
        callable, _port = self._get_callable_from_trace_tuple(current_trace_tuple)

        print(self._create_trace_frame_output_string(current_trace_frame))
        print(
            f"\nIssues in callable ({callable}):",
            self._num_issues_with_callable(current_trace_frame.callee),
        )

        if kind is None or kind == TraceKind.POSTCONDITION:
            print(f"\nPostconditions with caller ({callable}):")
            self.frames(callers=callable, kind=TraceKind.POSTCONDITION, limit=limit)

        if kind is None or kind == TraceKind.PRECONDITION:
            print(f"\nPreconditions with caller ({callable}):")
            self.frames(callers=callable, kind=TraceKind.PRECONDITION, limit=limit)

    def warning(self, message: str) -> None:
        # pyre-fixme[6]: Expected `Optional[_Writer]` for 2nd param but got `TextIO`.
        print(message, file=sys.stderr)

    def _get_trace_frame_branches(
        self, session: Session
    ) -> List[TraceFrameQueryResult]:
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
        return self._next_forward_trace_frames(session, parent_trace_frame, set())

    def _is_before_root(self) -> bool:
        trace_tuple = self.trace_tuples[self.current_trace_frame_index]
        return trace_tuple.trace_frame.kind == TraceKind.POSTCONDITION

    def _is_root_trace_tuple(self, trace_tuple: TraceTuple) -> bool:
        return trace_tuple.trace_frame.callee_port == "root"

    def _current_branch_index(self, branches: List[TraceFrameQueryResult]) -> int:
        selected_branch_id = int(
            self.trace_tuples[self.current_trace_frame_index].trace_frame.id
        )
        for i, branch in enumerate(branches):
            if selected_branch_id == int(branch.id):
                return i
        return -1

    def _group_trace_frames(
        self, trace_frames: Iterable[TraceFrameQueryResult], limit: int
    ) -> Dict[Tuple[str, str], List[TraceFrameQueryResult]]:
        """Buckets together trace frames that have the same caller:caller_port.
        """
        # pyre-fixme[9]: caller_buckets has type `DefaultDict[Tuple[str, str], List[T...
        caller_buckets: DefaultDict[
            Tuple[str, str], List[TraceFrameQueryResult]
        ] = defaultdict(list)
        for trace_frame in itertools.islice(trace_frames, limit):
            caller_buckets[(trace_frame.caller, trace_frame.caller_port)].append(
                trace_frame
            )
        return caller_buckets

    def _add_list_or_string_filter_to_query(
        self,
        filter: Union[str, List[str]],
        query: Query,
        column: InstrumentedAttribute,
        argument_name: str,
    ):
        return self._add_list_or_element_filter_to_query(
            filter, query, column, argument_name, str
        )

    def _add_list_or_int_filter_to_query(
        self,
        filter: Union[int, List[int]],
        query: Query,
        column: InstrumentedAttribute,
        argument_name: str,
    ):
        return self._add_list_or_element_filter_to_query(
            filter, query, column, argument_name, int
        )

    def _add_list_or_element_filter_to_query(
        self,
        filter: Union[T, List[T]],
        query: Query,
        column: InstrumentedAttribute,
        argument_name: str,
        element_type: Type,
    ) -> Query:
        if isinstance(filter, element_type):
            return query.filter(column.like(filter))
        if isinstance(filter, list):
            if not filter:
                raise UserError(f"'{argument_name}' should be non-empty.")
            return query.filter(or_(*[column.like(item) for item in filter]))
        raise UserError(
            f"'{argument_name}' should be {element_type} or " f"list of {element_type}."
        )

    def _output_file_lines(
        self, trace_frame: TraceFrameQueryResult, file_lines: List[str], context: int
    ) -> None:
        print(
            f"In {trace_frame.caller or trace_frame.callee} "
            f"[{trace_frame.filename}:{trace_frame.callee_location}]"
        )
        assert trace_frame.callee_location is not None
        location = trace_frame.callee_location
        center_line_number = location.line_no
        begin_lineno = max(center_line_number - context, 1)
        end_lineno = min(center_line_number + context, len(file_lines))
        lineno_width = len(str(end_lineno))

        lines_to_show = file_lines[begin_lineno - 1 : end_lineno]

        # In both cases this removes trailing newlines on each line.
        if sys.stdout.isatty():
            lines_to_show = highlight(
                "".join(lines_to_show),
                # startinline is to skip the <? check for php lexer
                get_lexer_for_filename(trace_frame.filename, startinline=True),
                TerminalFormatter(),
            ).split("\n")
        else:
            lines_to_show = [line.rstrip("\n") for line in lines_to_show]

        for i in range(begin_lineno, end_lineno + 1):
            line = lines_to_show[i - begin_lineno]

            prefix = " --> " if i == center_line_number else " " * 5
            prefix += f"{i:<{lineno_width}} "
            print(f"{prefix} {line}")
            if i == center_line_number:
                print(
                    " " * (len(prefix) + location.begin_column),
                    "^" * (location.end_column - location.begin_column),
                )

    def _output_trace_expansion(
        self, trace_frames: List[TraceFrameQueryResult], leaves_strings: List[str]
    ) -> None:
        for i, (frame, leaves) in enumerate(zip(trace_frames, leaves_strings)):
            prefix = (
                "[*]" if i == self._current_branch_index(trace_frames) else f"[{i + 1}]"
            )
            print(f"{prefix} {frame.callee} : {frame.callee_port}")
            print(f"{' ' * 8}[{frame.trace_length} hops: {leaves}]")
            print(f"{' ' * 8}[{frame.filename}:{frame.callee_location}]")

    def _output_trace_frames(
        self,
        trace_buckets: Dict[Tuple[str, str], List[TraceFrameQueryResult]],
        limit: int,
        total_trace_frames: int,
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

        if limit < total_trace_frames:
            print(
                f"...\nShowing {limit}/{total_trace_frames} matching frames. "
                "To see more, call 'frames' with the 'limit' argument."
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
            max(
                len(self._get_callable_from_trace_tuple(trace_tuple)[0])
                for trace_tuple in trace_tuples
            ),
            len("[callable]"),
        )
        max_length_condition = max(
            max(
                len(self._get_callable_from_trace_tuple(trace_tuple)[1])
                for trace_tuple in trace_tuples
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
                print(
                    f" {prefix}"
                    f" [Missing trace frame: {trace_tuple.trace_frame.callee}:"
                    f"{trace_tuple.trace_frame.callee_port}]"
                )
                continue

            callable, callable_port = self._get_callable_from_trace_tuple(trace_tuple)
            branches_string = (
                f"{expand}"
                f"{str(trace_tuple.branches):{max_length_split - len(expand)}}"
                if trace_tuple.branches > 1
                else " " * max_length_split
            )
            output_string = (
                f" {prefix}"
                f"{branches_string}"
                f" {callable:{max_length_callable}}"
                f" {callable_port:{max_length_condition}}"
                f" {trace_tuple.trace_frame.filename}"
                f":{trace_tuple.trace_frame.callee_location}"
            )

            print(output_string)

    def _create_trace_tuples(
        self, navigation: Iterable[Tuple[TraceFrameQueryResult, int]]
    ) -> List[TraceTuple]:
        return [
            TraceTuple(
                trace_frame=trace_frame,
                branches=branches,
                missing=trace_frame.caller == "",
            )
            for trace_frame, branches in navigation
        ]

    def _initial_trace_frames(self, session, issue_instance_id, kind):
        return (
            session.query(
                TraceFrame.id,
                TraceFrame.caller_id,
                CallerText.contents.label("caller"),
                TraceFrame.caller_port,
                TraceFrame.callee_id,
                CalleeText.contents.label("callee"),
                TraceFrame.callee_port,
                TraceFrame.callee_location,
                TraceFrame.kind,
                FilenameText.contents.label("filename"),
                TraceFrameLeafAssoc.trace_length,
            )
            .filter(TraceFrame.kind == kind)
            .join(
                IssueInstanceTraceFrameAssoc,
                IssueInstanceTraceFrameAssoc.trace_frame_id == TraceFrame.id,
            )
            .filter(IssueInstanceTraceFrameAssoc.issue_instance_id == issue_instance_id)
            .join(CallerText, CallerText.id == TraceFrame.caller_id)
            .join(CalleeText, CalleeText.id == TraceFrame.callee_id)
            .join(FilenameText, FilenameText.id == TraceFrame.filename_id)
            .join(
                TraceFrameLeafAssoc, TraceFrameLeafAssoc.trace_frame_id == TraceFrame.id
            )
            .group_by(TraceFrame.id)
            .order_by(TraceFrameLeafAssoc.trace_length, TraceFrame.callee_location)
            .all()
        )

    def _navigate_trace_frames(
        self,
        session: Session,
        initial_trace_frames: List[TraceFrameQueryResult],
        index: int = 0,
    ) -> List[Tuple[TraceFrameQueryResult, int]]:
        if not initial_trace_frames:
            return []

        trace_frames = [(initial_trace_frames[index], len(initial_trace_frames))]
        visited_ids: Set[int] = {int(initial_trace_frames[index].id)}
        while not self._is_leaf(trace_frames[-1][0]):
            trace_frame, branches = trace_frames[-1]
            next_nodes = self._next_forward_trace_frames(
                session, trace_frame, visited_ids
            )

            if len(next_nodes) == 0:
                # Denote a missing frame by setting caller to None
                trace_frames.append(
                    (
                        TraceFrameQueryResult(
                            id=DBID(0),
                            callee=trace_frame.callee,
                            callee_port=trace_frame.callee_port,
                            caller="",
                            caller_port="",
                        ),
                        0,
                    )
                )
                return trace_frames

            visited_ids.add(int(next_nodes[0].id))
            trace_frames.append((next_nodes[0], len(next_nodes)))
        return trace_frames

    def _is_leaf(self, trace_frame: TraceFrameQueryResult) -> bool:
        return trace_frame.callee_port in self.LEAF_NAMES

    def _next_forward_trace_frames(
        self,
        session: Session,
        trace_frame: TraceFrameQueryResult,
        visited_ids: Set[int],
    ) -> List[TraceFrameQueryResult]:
        return self._next_trace_frames(
            session, trace_frame, visited_ids, backwards=False
        )

    def _next_backward_trace_frames(
        self,
        session: Session,
        trace_frame: TraceFrameQueryResult,
        visited_ids: Set[int],
    ) -> List[TraceFrameQueryResult]:
        return self._next_trace_frames(
            session, trace_frame, visited_ids, backwards=True
        )

    def _next_trace_frames(
        self,
        session: Session,
        trace_frame: TraceFrameQueryResult,
        visited_ids: Set[int],
        backwards: bool = False,
    ) -> List[TraceFrameQueryResult]:
        """Finds all trace frames that the given trace_frame flows to.

        When backwards=True, the result will include the parameter trace_frame,
        since we are filtering on the parameter's callee.
        """
        query = (
            session.query(
                TraceFrame.id,
                TraceFrame.caller_id,
                CallerText.contents.label("caller"),
                TraceFrame.caller_port,
                TraceFrame.callee_id,
                CalleeText.contents.label("callee"),
                TraceFrame.callee_port,
                TraceFrame.callee_location,
                TraceFrame.kind,
                FilenameText.contents.label("filename"),
                TraceFrameLeafAssoc.trace_length,
            )
            .filter(TraceFrame.run_id == self.current_run_id)
            .filter(TraceFrame.kind == trace_frame.kind)
            .join(CallerText, CallerText.id == TraceFrame.caller_id)
            .join(CalleeText, CalleeText.id == TraceFrame.callee_id)
            .join(FilenameText, FilenameText.id == TraceFrame.filename_id)
            .filter(
                TraceFrame.caller_id != TraceFrame.callee_id
            )  # skip recursive calls for now
        )
        if backwards:
            query = query.filter(TraceFrame.callee_id == trace_frame.caller_id).filter(
                TraceFrame.callee_port == trace_frame.caller_port
            )
        else:
            query = query.filter(TraceFrame.caller_id == trace_frame.callee_id).filter(
                TraceFrame.caller_port == trace_frame.callee_port
            )

        results = (
            query.join(
                TraceFrameLeafAssoc, TraceFrameLeafAssoc.trace_frame_id == TraceFrame.id
            )
            .group_by(TraceFrame.id)
            .order_by(TraceFrameLeafAssoc.trace_length, TraceFrame.callee_location)
        )
        filter_leaves = (
            self.sources if trace_frame.kind == TraceKind.POSTCONDITION else self.sinks
        )

        filtered_results = []
        for frame in results:
            if int(frame.id) not in visited_ids and filter_leaves.intersection(
                set(
                    self._get_leaves_trace_frame(
                        session,
                        int(frame.id),
                        self._trace_kind_to_shared_text_kind(frame.kind),
                    )
                )
            ):
                filtered_results.append(frame)

        return filtered_results

    def _create_issue_output_string(
        self, issue: IssueQueryResult, sources: Set[str], sinks: Set[str]
    ) -> str:
        sources_output = f"\n{' ' * 10}".join(sources)
        sinks_output = f"\n{' ' * 10}".join(sinks)
        return "\n".join(
            [
                f"Issue {issue.id}",
                f"    Code: {issue.code}",
                f" Message: {issue.message}",
                f"Callable: {issue.callable}",
                f" Sources: {sources_output if sources_output else 'No sources'}",
                f"   Sinks: {sinks_output if sinks_output else 'No sinks'}",
                (f"Location: {issue.filename}" f":{issue.location}"),
            ]
        )

    def _create_trace_frame_output_string(
        self, trace_frame: TraceFrameQueryResult
    ) -> str:
        leaf_kind = self._trace_kind_to_shared_text_kind(trace_frame.kind)
        leaves_label = "Sources" if leaf_kind == SharedTextKind.SOURCE else "Sinks"

        with self.db.make_session() as session:
            leaves_output = f"\n{' ' * 13}".join(
                self._get_leaves_trace_frame(session, trace_frame.id, leaf_kind)
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

    def _select_parent_trace_frame(
        self, parent_trace_frames: List[TraceFrameQueryResult]
    ) -> TraceFrameQueryResult:
        for i, parent in enumerate(parent_trace_frames):
            print(f"[{i + 1}] {parent.caller} : {parent.caller_port}")
        parent_number = self._prompt_for_number(
            "Parent number", len(parent_trace_frames)
        )
        return parent_trace_frames[parent_number - 1]

    def _select_branch_trace_frame(
        self, branch_trace_frames: List[TraceFrameQueryResult], leaves_string: List[str]
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

    def _update_trace_tuples_new_parent(
        self, parent_trace_frame: TraceFrameQueryResult
    ) -> None:
        # Construct the placeholder trace tuple and the actual one.
        new_head = [
            TraceTuple(trace_frame=parent_trace_frame, placeholder=True),
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
                FilenameText.contents.label("filename"),
                IssueInstance.location,
                Issue.code,
                CallableText.contents.label("callable"),
                MessageText.contents.label("message"),
            )
            .filter(IssueInstance.id == self.current_issue_instance_id)
            .join(Issue, IssueInstance.issue_id == Issue.id)
            .join(FilenameText, FilenameText.id == IssueInstance.filename_id)
            .join(CallableText, CallableText.id == IssueInstance.callable_id)
            .join(MessageText, MessageText.id == IssueInstance.message_id)
            .first()
        )

    def _get_leaves_issue_instance(
        self, session: Session, issue_instance_id: int, kind: SharedTextKind
    ) -> Set[str]:
        message_ids = [
            int(id)
            for id, in session.query(SharedText.id)
            .distinct(SharedText.id)
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
    ) -> Set[str]:
        message_ids = [
            int(id)
            for id, in session.query(SharedText.id)
            .distinct(SharedText.id)
            .join(TraceFrameLeafAssoc, SharedText.id == TraceFrameLeafAssoc.leaf_id)
            .filter(TraceFrameLeafAssoc.trace_frame_id == trace_frame_id)
            .filter(SharedText.kind == kind)
        ]
        return self._leaf_dict_lookups(message_ids, kind)

    def _leaf_dict_lookups(
        self, message_ids: List[int], kind: SharedTextKind
    ) -> Set[str]:
        leaf_dict = (
            self.sources_dict if kind == SharedTextKind.SOURCE else self.sinks_dict
        )
        return {leaf_dict[id] for id in message_ids if id in leaf_dict}

    def _show_current_issue_instance(self):
        with self.db.make_session() as session:
            issue = self._get_current_issue(session)

        page.display_page(
            self._create_issue_output_string(issue, self.sources, self.sinks)
        )

    def _show_current_trace_frame(self):
        with self.db.make_session() as session:
            trace_frame = (
                session.query(
                    TraceFrame.id,
                    CallerText.contents.label("caller"),
                    TraceFrame.caller_port,
                    CalleeText.contents.label("callee"),
                    TraceFrame.callee_port,
                    TraceFrame.kind,
                    FilenameText.contents.label("filename"),
                    TraceFrame.callee_location,
                )
                .filter(TraceFrame.id == self.current_frame_id)
                .join(CallerText, CallerText.id == TraceFrame.caller_id)
                .join(CalleeText, CalleeText.id == TraceFrame.callee_id)
                .join(FilenameText, FilenameText.id == TraceFrame.filename_id)
                .one()
            )

        page.display_page(self._create_trace_frame_output_string(trace_frame))

    def callable(self) -> Optional[str]:
        """Show the name of the current callable in the trace"""
        if self.current_trace_frame_index != -1:
            return self._get_callable_from_trace_tuple(
                self.trace_tuples[self.current_trace_frame_index]
            )[0]
        return None

    def _verify_entrypoint_selected(self) -> None:
        assert self.current_issue_instance_id == -1 or self.current_frame_id == -1

        if self.current_issue_instance_id == -1 and self.current_frame_id == -1:
            raise UserError(
                "Use 'issue ID' or 'frame ID' to select an entrypoint first."
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
                .join(CallableText, CallableText.id == IssueInstance.callable_id)
                .filter(CallableText.contents == callable)
                .scalar()
            )

    def _trace_kind_to_shared_text_kind(
        self, trace_kind: Optional[TraceKind]
    ) -> SharedTextKind:
        if trace_kind == TraceKind.POSTCONDITION:
            return SharedTextKind.SOURCE
        if trace_kind == TraceKind.PRECONDITION:
            return SharedTextKind.SINK

        assert False, f"{trace_kind} is invalid"

    def _get_callable_from_trace_tuple(
        self, trace_tuple: TraceTuple
    ) -> Tuple[str, str]:
        """Returns either (caller, caller_port) or (callee, callee_port).
        """
        trace_frame = trace_tuple.trace_frame
        if trace_tuple.placeholder:
            return trace_frame.caller, trace_frame.caller_port
        return trace_frame.callee, trace_frame.callee_port
