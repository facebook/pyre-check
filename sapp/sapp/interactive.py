#!/usr/bin/env python3

import os
import sys
from typing import List, Optional

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
            print(
                "No runs found. "
                f"Try running '{os.path.basename(sys.argv[0])} analyze' first.",
                file=sys.stderr,
            )
            sys.exit(1)

        self.current_run_id = latest_run_id
        self.current_issue_id = None

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
            print(
                f"Run {run_id} doesn't exist or is not finished. "
                "Type 'runs()' for available runs.",
                file=sys.stderr,
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
            print(
                f"Issue {issue_id} doesn't exist. "
                "Type 'issues()' for available issues.",
                file=sys.stderr,
            )
            return

        self.current_issue_id = selected_issue.id
        print(f"Set issue to {issue_id}.")
        self.show()

    def show(self):
        """ More details about the selected issue.
        """
        if self.current_issue_id is None:
            print("Use 'set_issue(ID)' to select an issue first.", file=sys.stderr)
            return

        with self.db.make_session() as session:
            issue_instance, issue = (
                session.query(IssueInstance, Issue)
                .filter(IssueInstance.id == self.current_issue_id)
                .join(Issue, IssueInstance.issue_id == Issue.id)
                .options(joinedload(IssueInstance.message))
                .first()
            )
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
                    print("'codes' should be a list.", file=sys.stderr)
                    return
                query = query.filter(Issue.code.in_(codes))

            if callables is not None:
                if not isinstance(callables, list):
                    print("'callables' should be a list.", file=sys.stderr)
                    return
                query = query.filter(
                    or_(*[Issue.callable.like(callable) for callable in callables])
                )

            if filenames is not None:
                if not isinstance(filenames, list):
                    print("'filenames' should be a list.", file=sys.stderr)
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
        """ Show a trace for the selected issue.
        """
        pass

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
