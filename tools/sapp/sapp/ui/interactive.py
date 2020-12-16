# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import builtins
import enum
import itertools
import os
import sys
from collections import defaultdict
from typing import (
    Any,
    Callable,
    DefaultDict,
    Dict,
    Iterable,
    List,
    Optional,
    Sequence,
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
from prompt_toolkit.completion import PathCompleter
from prompt_toolkit.history import FileHistory, History
from pygments import highlight
from pygments.formatters import TerminalFormatter
from pygments.lexers import get_lexer_for_filename
from sqlalchemy.orm import Session, aliased
from sqlalchemy.orm.attributes import InstrumentedAttribute
from sqlalchemy.orm.query import Query as RawQuery
from sqlalchemy.sql import func
from sqlalchemy.sql.expression import or_

from ..analysis_output import AnalysisOutput, AnalysisOutputError
from ..db import DB
from ..decorators import UserError, catch_keyboard_interrupt, catch_user_error
from ..models import (
    DBID,
    Issue,
    IssueInstance,
    IssueInstanceSharedTextAssoc,
    Run,
    RunStatus,
    SharedText,
    SharedTextKind,
    TraceFrame,
    TraceKind,
    create as create_models,
)
from ..pipeline.base_parser import BaseParser
from . import issues as issues_module, trace
from .issues import Instance, IssueQueryResult
from .trace import TraceFrameQueryResult, TraceTuple


T = TypeVar("T")

# pyre-ignore[5]: SQLAlchemy
FilenameText = aliased(SharedText)
# pyre-ignore[5]: SQLAlchemy
CallableText = aliased(SharedText)
# pyre-ignore[5]: SQLAlchemy
CallerText = aliased(SharedText)
# pyre-ignore[5]: SQLAlchemy
CalleeText = aliased(SharedText)
# pyre-ignore[5]: SQLAlchemy
MessageText = aliased(SharedText)


class ListFilterException(Exception):
    pass


class LeafOrderBy(str, enum.Enum):
    name = "name"
    number_issues = "number_issues"


ScopeVariables = Dict[str, Union[Callable[..., None], TraceKind]]


class Interactive:
    help_message = """
Commands =======================================================================

== Information commands ==
help                 show this message
help COMMAND         more info about a command
state                show the internal state of the tool for debugging

== Display commands ==
runs                 list all completed static analysis runs
issues               list all issues for the selected run
frames               show trace frames independently of an issue
leaves               list all leaves of issues for the selected run
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
    PARSER_CLASS_SCOPE_KEY = "_parser_class"

    def __init__(
        self,
        *,
        database: DB,
        repository_directory: Optional[str] = None,
        parser_class: Type[BaseParser],
    ) -> None:
        self.db = database
        self.scope_vars: ScopeVariables = {
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
            "leaves": self.leaves,
            "parents": self.parents,
            "details": self.details,
            "analysis_output": self.analysis_output,
            "callable": self.callable,
            self.SELF_SCOPE_KEY: self,
            self.PARSER_CLASS_SCOPE_KEY: parser_class,
        }
        self.repository_directory: str = repository_directory or os.getcwd()
        self.current_analysis_output: Optional[AnalysisOutput] = None

        self._current_run_id: DBID = DBID(-1)

        # Trace exploration relies on either of these
        self.current_issue_instance_id: DBID = DBID(-1)
        self.current_frame_id: DBID = DBID(-1)

        self.sources: Set[str] = set()
        self.sinks: Set[str] = set()
        self.features: Set[str] = set()
        self.sinks_dict: Dict[int, str] = {}

        self._leaf_lookup: trace.LeafLookup = trace.LeafLookup({}, {}, {})

        # Tuples representing the trace of the current issue
        self.trace_tuples: List[TraceTuple] = []
        # Active trace frame of the current trace
        self.current_trace_frame_index: int = -1

        # Persist history for prompts that opt-into it, by specifying
        # history_key on self.prompt().
        self.prompt_history: Dict[str, History] = {}

    def setup(self) -> ScopeVariables:
        create_models(self.db)
        with self.db.make_session() as session:
            latest_run_id = (
                session.query(func.max(Run.id))
                .filter(Run.status == RunStatus.FINISHED)
                .scalar()
            )

            self._leaf_lookup = trace.LeafLookup.create(session)

        print("=" * len(self.welcome_message))
        print(self.welcome_message)
        print("=" * len(self.welcome_message))

        if latest_run_id.resolved() is None:
            self.warning(
                "No runs found. "
                f"Try running '{os.path.basename(sys.argv[0])} analyze' first."
            )
        else:
            self._current_run_id = latest_run_id

        return self.scope_vars

    def help(self, object: Optional[object] = None) -> None:
        if object is None:
            print(self.help_message)
            return

        # pyre-fixme[16]: Module `builtins` has no attribute `help`.
        builtins.help(object)

    def state(self) -> None:
        print(f"              Database: {self.db.dbtype}:{self.db.dbname}")
        print(f"       Analysis Output: {self.current_analysis_output}")
        print(f"  Repository directory: {self.repository_directory}")
        print(f"           Current run: {self._current_run_id}")
        print(f"Current issue instance: {int(self.current_issue_instance_id)}")
        print(f"   Current trace frame: {int(self.current_frame_id)}")
        print(f"        Sources filter: {self.sources}")
        print(f"          Sinks filter: {self.sinks}")

    @catch_keyboard_interrupt()
    def runs(self, use_pager: bool = False) -> None:
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
    def run(self, run_id: DBID) -> None:
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

        self._current_run_id = selected_run.id
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
        *args: Any,
        **kwargs: Any,
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

        self._current_run_id = selected_run_id
        print(f"Set run to {self._current_run_id}.")

    @catch_keyboard_interrupt()
    def issue(self, issue_instance_id: DBID) -> None:
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

            self.features = self._get_leaves_issue_instance(
                session, issue_instance_id, SharedTextKind.FEATURE
            )

        self.current_issue_instance_id = selected_issue.id
        self.current_frame_id = DBID(-1)
        self.current_trace_frame_index = 1  # first one after the source

        print(f"Set issue to {issue_instance_id}.")
        if selected_issue.run_id != self._current_run_id:
            self._current_run_id = selected_issue.run_id
            print(f"Set run to {self._current_run_id}.")
        print()

        self._generate_trace_from_issue()
        self.show()

    @catch_keyboard_interrupt()
    @catch_user_error()
    def show(self) -> None:
        """More details about the selected issue or trace frame."""
        self._verify_entrypoint_selected()

        if int(self.current_issue_instance_id) != -1:
            self._show_current_issue_instance()
            return

        self._show_current_trace_frame()

    @catch_keyboard_interrupt()
    @catch_user_error()
    def issues(
        self,
        use_pager: bool = False,
        *,
        codes: Optional[Union[int, List[int]]] = None,
        callables: Optional[Union[str, List[str]]] = None,
        filenames: Optional[Union[str, List[str]]] = None,
        all_features: Optional[Union[str, List[str]]] = None,
        any_features: Optional[Union[str, List[str]]] = None,
        exclude_features: Optional[Union[str, List[str]]] = None,
        exact_trace_length_to_sources: Optional[int] = None,
        exact_trace_length_to_sinks: Optional[int] = None,
        max_trace_length_to_sources: Optional[int] = None,
        max_trace_length_to_sinks: Optional[int] = None,
    ) -> None:
        """Lists issues for the selected run.

        Parameters (all optional):
            use_pager: bool                use a unix style pager for output
            codes: int or list[int]        issue codes to filter on
            callables: str or list[str]    callables to filter on (supports wildcards)
            filenames: str or list[str]    filenames to filter on (supports wildcards)
            all_features: str or list[str] features to filter on
            any_features: str or list[str] features to inclusively filter on
            exclude_features: str or list[str]
                features to exclude issues based upon
            exact_trace_length_to_sources: int
                exact values for min trace length to sources to filter on
            exact_trace_length_to_sinks: int
                exact values for min trace length to sinks to filter on
            max_trace_length_to_sources: int
                maximum value for min trace length to sources to filter on
            max_trace_length_to_sinks: int
                maximum value for min trace length to sinks to filter on

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
            builder = Instance(session, self._current_run_id)

            if codes is not None:
                if not isinstance(codes, int) and not isinstance(codes, list):
                    raise UserError("'codes' should be int or list of int.")
                if isinstance(codes, int):
                    codes = [codes]
                builder = builder.where_codes_is_any_of(codes)

            if callables is not None:
                if not isinstance(callables, str) and not isinstance(callables, list):
                    raise UserError("'callables' should be str or list of str.")
                if isinstance(callables, str):
                    callables = [callables]
                builder = builder.where_callables_is_any_of(callables)

            if filenames is not None:
                if not isinstance(filenames, str) and not isinstance(filenames, list):
                    raise UserError("'filenames' should be str or list of str.")
                if isinstance(filenames, str):
                    filenames = [filenames]
                builder = builder.where_path_is_any_of(filenames)

            if (exact_trace_length_to_sources is not None) and (
                max_trace_length_to_sources is not None
            ):
                raise UserError(
                    (
                        "'exact_trace_length_to_sources' and "
                        "'max_trace_length_to_sources' can't be set together"
                    )
                )

            if (exact_trace_length_to_sinks is not None) and (
                max_trace_length_to_sinks is not None
            ):
                raise UserError(
                    (
                        "'exact_trace_length_to_sinks' and "
                        "'max_trace_length_to_sinks' can't be set together"
                    )
                )

            if exact_trace_length_to_sources is not None:
                if not isinstance(exact_trace_length_to_sources, int):
                    raise UserError("'exact_trace_length_to_sources' should be int.")
                builder = builder.where_trace_length_to_sources(
                    exact_trace_length_to_sources, exact_trace_length_to_sources
                )

            if exact_trace_length_to_sinks is not None:
                if not isinstance(exact_trace_length_to_sinks, int):
                    raise UserError("'exact_trace_length_to_sinks' should be int.")
                builder = builder.where_trace_length_to_sinks(
                    exact_trace_length_to_sinks, exact_trace_length_to_sinks
                )

            if max_trace_length_to_sources is not None:
                if not isinstance(max_trace_length_to_sources, int):
                    raise UserError("'max_trace_length_to_sources' should be int.")
                builder = builder.where_trace_length_to_sources(
                    0, max_trace_length_to_sources
                )

            if max_trace_length_to_sinks is not None:
                if not isinstance(max_trace_length_to_sinks, int):
                    raise UserError("'max_trace_length_to_sinks' should be int.")
                builder = builder.where_trace_length_to_sinks(
                    0, max_trace_length_to_sinks
                )

            if all_features is not None:
                if not isinstance(all_features, str) and not isinstance(
                    all_features, list
                ):
                    raise UserError("'all_features' should be str or list of str.")
                if isinstance(all_features, str):
                    all_features = [all_features]
                builder = builder.where_all_features(all_features)

            if any_features is not None:
                if not isinstance(any_features, str) and not isinstance(
                    any_features, list
                ):
                    raise UserError("'any_features' should be str or list of str.")
                if isinstance(any_features, str):
                    any_features = [any_features]
                builder = builder.where_any_features(any_features)

            if exclude_features is not None:
                if not isinstance(exclude_features, str) and not isinstance(
                    exclude_features, list
                ):
                    raise UserError("'any_features' should be str or list of str.")
                if isinstance(exclude_features, str):
                    exclude_features = [exclude_features]
                builder = builder.where_exclude_features(exclude_features)

            issues = builder.get()
            sources_list = [
                issues_module.sources(session, issue.issue_instance_id)
                for issue in issues
            ]
            sinks_list = [
                issues_module.sinks(session, issue.issue_instance_id)
                for issue in issues
            ]
            features_list = [
                issues_module.features(session, issue.issue_instance_id)
                for issue in issues
            ]

        issue_strings = []
        for issue, sources, sinks, features in zip(
            issues, sources_list, sinks_list, features_list
        ):
            issue_strings.append(
                self._create_issue_output_string(issue, sources, sinks, features)
            )

        issue_output = f"\n{'-' * 80}\n".join(issue_strings)
        pager(issue_output)
        print(f"Found {len(issue_strings)} issues with run_id {self._current_run_id}.")

    @catch_user_error()
    def trace(self, features: bool = False) -> None:
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
        self._output_trace_tuples(self.trace_tuples, features)

    @catch_keyboard_interrupt()
    @catch_user_error()
    def frames(
        self,
        *,
        callers: Optional[Union[str, List[str]]] = None,
        callees: Optional[Union[str, List[str]]] = None,
        kind: Optional[TraceKind] = None,
        limit: Optional[int] = 10,
    ) -> None:
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
                .filter(TraceFrame.run_id == self._current_run_id)
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
                self.sources = trace.get_leaves_trace_frame(
                    session, int(selected_frame.id), SharedTextKind.SOURCE
                )

            else:
                self.sinks = trace.get_leaves_trace_frame(
                    session, int(selected_frame.id), SharedTextKind.SINK
                )

                self.sources = set()

        self.current_frame_id = selected_frame.id
        self.current_issue_instance_id = DBID(-1)

        print(f"Set trace frame to {frame_id}.")
        if int(selected_frame.run_id) != self._current_run_id:
            self._current_run_id = selected_frame.run_id
            print(f"Set run to {self._current_run_id}.")
        print()

        self._generate_trace_from_frame()
        self.show()

    @catch_keyboard_interrupt()
    @catch_user_error()
    def leaves(
        self,
        kind: str = "sink",
        use_pager: bool = False,
        limit: Optional[int] = None,
        order_by: Optional[LeafOrderBy] = None,
    ) -> None:
        """Lists leaves of issues for the selected run.

        Parameters (all optional):
            kind: source|sink|feature (default: sink)       the type of leaves to show
            use_pager: bool                                 use a unix style pager for output
            limit: int (default: all)                       how many leaves to display
            order_by: name|number_issues (default: random)  sort by a criteria
        """
        pager = self._resolve_pager(use_pager)
        leaves: DefaultDict[str, int] = defaultdict(int)

        text_kind = SharedTextKind.from_string(kind)
        if text_kind is None:
            raise UserError("Invalid kind.")

        # Show leaf names instead of leaf kinds.
        if text_kind == SharedTextKind.source:
            text_kind = SharedTextKind.source_detail
        elif text_kind == SharedTextKind.sink:
            text_kind = SharedTextKind.sink_detail

        with self.db.make_session() as session:
            query = (
                session.query(
                    IssueInstanceSharedTextAssoc.shared_text_id, SharedText.contents
                )
                .join(
                    SharedText,
                    IssueInstanceSharedTextAssoc.shared_text_id == SharedText.id,
                )
                .join(
                    IssueInstance,
                    IssueInstanceSharedTextAssoc.issue_instance_id == IssueInstance.id,
                )
                .filter(IssueInstance.run_id == self._current_run_id)
                .filter(SharedText.kind == text_kind)
            )
            for (_, name) in query:
                leaves[name] += 1

        # pyre-fixme[35]: Target cannot be annotated.
        query: Iterable[Tuple[str, int]]
        if order_by == LeafOrderBy.name:
            query = sorted(leaves.items(), key=lambda leaf: leaf[0])
        elif order_by == LeafOrderBy.number_issues:
            query = sorted(leaves.items(), key=lambda leaf: leaf[1], reverse=True)
        elif order_by is None:
            query = leaves.items()
        else:
            raise UserError("Invalid order_by method.")

        if limit is not None:
            query = itertools.islice(query, limit)

        leaves_strings = []
        for name, number_issues in query:
            leaves_strings.append(f"{name} (in {number_issues} issues)")

        pager("\n".join(leaves_strings))
        print(
            f"Found {len(leaves)} {kind}s in issues with run_id {self._current_run_id}."
        )

    @catch_keyboard_interrupt()
    @catch_user_error()
    def parents(self) -> None:
        self._verify_entrypoint_selected()
        current_trace_tuple = self.trace_tuples[self.current_trace_frame_index]

        # Don't allow calling from the leaf node in a trace. Instead, call
        # parents() from the placeholder of the caller of the leaf node.
        if current_trace_tuple.trace_frame.is_leaf():
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
            if current_trace_tuple.trace_frame.kind == TraceKind.POSTCONDITION:
                leaf_kind = self.sources
            elif current_trace_tuple.trace_frame.kind == TraceKind.PRECONDITION:
                leaf_kind = self.sinks
            else:
                assert (
                    current_trace_tuple.trace_frame.kind == TraceKind.POSTCONDITION
                    or current_trace_tuple.trace_frame.kind == TraceKind.PRECONDITION
                )

            parent_trace_frames = trace.next_frames(
                session,
                current_trace_tuple.trace_frame,
                leaf_kind,
                set(),
                self._current_run_id,
                backwards=True,
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

    def _generate_trace_from_issue(self) -> None:
        with self.db.make_session() as session:
            issue = self._get_current_issue(session)
            postcondition_initial_frames = trace.initial_frames(
                session,
                issue.issue_instance_id,
                TraceKind.POSTCONDITION,
            )
            precondition_initial_frames = trace.initial_frames(
                session,
                issue.issue_instance_id,
                TraceKind.PRECONDITION,
            )

            postcondition_navigation = trace.navigate_trace_frames(
                session,
                postcondition_initial_frames,
                self.sources,
                self.sinks,
            )
            precondition_navigation = trace.navigate_trace_frames(
                session,
                precondition_initial_frames,
                self.sources,
                self.sinks,
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
            navigation = trace.navigate_trace_frames(
                session,
                [TraceFrameQueryResult.from_record(trace_frame)],
                self.sources,
                self.sinks,
            )

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
    def next_cursor_location(self) -> None:
        """Move cursor to the next trace frame."""
        self._verify_entrypoint_selected()
        self.current_trace_frame_index = min(
            self.current_trace_frame_index + 1, len(self.trace_tuples) - 1
        )
        self.trace()

    @catch_user_error()
    def prev_cursor_location(self) -> None:
        """Move cursor to the previous trace frame."""
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
                        for leaf in trace.get_leaves_trace_frame(
                            session,
                            int(frame.id),
                            trace.trace_kind_to_shared_text_kind(frame.kind),
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
            new_navigation = trace.navigate_trace_frames(
                session,
                branches,
                self.sources,
                self.sinks,
                selected_number - 1,
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

        filename = os.path.join(
            self.repository_directory, current_trace_frame.filename or "<undefined>"
        )
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
            return trace.initial_frames(session, self.current_issue_instance_id, kind)

        parent_trace_frame = self.trace_tuples[parent_index].trace_frame

        if parent_trace_frame.kind == TraceKind.POSTCONDITION:
            leaf_kind = self.sources
        elif parent_trace_frame.kind == TraceKind.PRECONDITION:
            leaf_kind = self.sinks
        else:
            assert (
                parent_trace_frame.kind == TraceKind.POSTCONDITION
                or parent_trace_frame.kind == TraceKind.PRECONDITION
            )

        return trace.next_frames(
            session,
            parent_trace_frame,
            leaf_kind,
            set(),
            self._current_run_id,
        )

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
        """Buckets together trace frames that have the same caller:caller_port."""
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
        query: RawQuery[T],
        column: InstrumentedAttribute,
        argument_name: str,
    ) -> RawQuery[T]:
        # pyre-fixme[7]: Expected `RawQuery[Variable[T]]` but got `RawQuery[str]`.
        return self._add_list_or_element_filter_to_query(
            filter,
            # pyre-fixme[6]: Expected `RawQuery[Variable[T]]` for 2nd param but got
            #  `RawQuery[Variable[T]]`.
            query,
            column,
            argument_name,
            str,
        )

    def _add_list_or_int_filter_to_query(
        self,
        filter: Union[int, List[int]],
        query: RawQuery[T],
        column: InstrumentedAttribute,
        argument_name: str,
    ) -> RawQuery[T]:
        # pyre-fixme[7]: Expected `RawQuery[Variable[T]]` but got `RawQuery[int]`.
        return self._add_list_or_element_filter_to_query(
            filter,
            # pyre-fixme[6]: Expected `RawQuery[Variable[T]]` for 2nd param but got
            #  `RawQuery[Variable[T]]`.
            query,
            column,
            argument_name,
            int,
        )

    def _add_list_or_element_filter_to_query(
        self,
        filter: Union[T, List[T]],
        query: RawQuery[T],
        column: InstrumentedAttribute,
        argument_name: str,
        element_type: Type[Union[str, int]],
    ) -> RawQuery[T]:
        if isinstance(filter, element_type):
            return query.filter(column.like(filter))
        if isinstance(filter, list):
            if not filter:
                raise UserError(f"'{argument_name}' should be non-empty.")
            # pyre-fixme[6]: Expected `str` for 1st param but got `T`.
            return query.filter(or_(*[column.like(item) for item in filter]))
        raise UserError(
            f"'{argument_name}' should be {element_type} or " f"list of {element_type}."
        )

    def _add_max_int_filter_to_query(
        self,
        filter: int,
        query: RawQuery[T],
        column: InstrumentedAttribute,
        argument_name: str,
    ) -> RawQuery[T]:
        if isinstance(filter, int):
            return query.filter(column <= filter)
        raise UserError(f"'{argument_name}' should be int.")

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

    def _output_trace_tuples(
        self, trace_tuples: Sequence[TraceTuple], features: bool = False
    ) -> None:
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
            if features:
                frame_features = [
                    text.contents
                    for text in trace_tuple.trace_frame.shared_texts
                    if text.kind is SharedTextKind.FEATURE
                ]
                if frame_features:
                    prefix = " " * 11 + "--F:"
                    print(f"{prefix} {frame_features}")

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

    def _create_issue_output_string(
        self,
        issue: IssueQueryResult,
        sources: Set[str],
        sinks: Set[str],
        features: Set[str],
    ) -> str:
        sources_output = f"\n{' ' * 18}".join(sources)
        sinks_output = f"\n{' ' * 18}".join(sinks)
        features_output = f"\n{' ' * 18}".join(features)
        return "\n".join(
            [
                f"Issue {issue.issue_instance_id}",
                f"            Code: {issue.code}",
                f"         Message: {issue.message}",
                f"        Callable: {issue.callable}",
                (
                    f"         Sources: "
                    f"{sources_output if sources_output else 'No sources'}"
                ),
                (
                    f"           Sinks: "
                    f"{sinks_output if sinks_output else 'No sinks'}"
                ),
                (
                    f"        Features: "
                    f"{features_output if features_output else 'No features'}"
                ),
                (f"        Location: " f"{issue.filename}" f":{issue.location}"),
                (
                    f"Min Trace Length: "
                    f"Source ({issue.min_trace_length_to_sources}) | "
                    f"Sink ({issue.min_trace_length_to_sinks})"
                ),
            ]
        )

    def _create_trace_frame_output_string(
        self, trace_frame: TraceFrameQueryResult
    ) -> str:
        leaf_kind = trace.trace_kind_to_shared_text_kind(trace_frame.kind)
        leaves_label = "Sources" if leaf_kind == SharedTextKind.SOURCE else "Sinks"

        with self.db.make_session() as session:
            leaves_output = f"\n{' ' * 13}".join(
                trace.get_leaves_trace_frame(session, trace_frame.id, leaf_kind)
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

    # pyre-fixme[3]: Return type must be annotated.
    def _resolve_pager(self, use_pager: bool):
        use_pager = sys.stdin.isatty() if use_pager is None else use_pager
        return page.page if use_pager else page.display_page

    def _get_current_issue(self, session: Session) -> IssueQueryResult:
        return (
            session.query(
                # pyre-ignore[16]: SQLAlchemy
                IssueInstance.id.label("issue_instance_id"),
                FilenameText.contents.label("filename"),
                IssueInstance.location,
                Issue.code,
                CallableText.contents.label("callable"),
                MessageText.contents.label("message"),
                IssueInstance.min_trace_length_to_sources,
                IssueInstance.min_trace_length_to_sinks,
            )
            .filter(IssueInstance.id == self.current_issue_instance_id)
            .join(Issue, IssueInstance.issue_id == Issue.id)
            .join(FilenameText, FilenameText.id == IssueInstance.filename_id)
            .join(CallableText, CallableText.id == IssueInstance.callable_id)
            .join(MessageText, MessageText.id == IssueInstance.message_id)
            .first()
        )

    def _get_leaves_issue_instance(
        self, session: Session, issue_instance_id: DBID, kind: SharedTextKind
    ) -> Set[str]:
        ids = [
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
        return self._leaf_lookup.resolve(ids, kind)

    def _show_current_issue_instance(self) -> None:
        with self.db.make_session() as session:
            issue = self._get_current_issue(session)

        page.display_page(
            self._create_issue_output_string(
                issue, self.sources, self.sinks, self.features
            )
        )

    def _show_current_trace_frame(self) -> None:
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
        assert (
            int(self.current_issue_instance_id) == -1
            or int(self.current_frame_id) == -1
        )

        if (
            int(self.current_issue_instance_id) == -1
            and int(self.current_frame_id) == -1
        ):
            raise UserError(
                "Use 'issue ID' or 'frame ID' to select an entrypoint first."
            )

    def _verify_multiple_branches(self) -> None:
        current_trace_tuple = self.trace_tuples[self.current_trace_frame_index]
        if current_trace_tuple.branches < 2:
            raise UserError("This trace frame has no alternate branches to take.")

    def _num_issues_with_callable(self, callable: str) -> int:
        with self.db.make_session() as session:
            return (
                session.query(func.count(IssueInstance.id))
                .join(CallableText, CallableText.id == IssueInstance.callable_id)
                .filter(CallableText.contents == callable)
                .scalar()
            )

    def _get_callable_from_trace_tuple(
        self, trace_tuple: TraceTuple
    ) -> Tuple[str, str]:
        """Returns either (caller, caller_port) or (callee, callee_port)."""
        trace_frame = trace_tuple.trace_frame
        if trace_tuple.placeholder:
            return trace_frame.caller, trace_frame.caller_port
        return trace_frame.callee, trace_frame.callee_port
