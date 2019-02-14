#!/usr/bin/env python3

import os
import sys

import IPython
from IPython.core import page
from sapp.db import DB
from sapp.models import Issue, IssueInstance, Run, RunStatus, SourceLocation
from sqlalchemy.orm import joinedload
from sqlalchemy.sql import func


class Interactive:
    help_message = """
runs()          list all completed runs
set_run(ID)     select a specific run
issues()        list all for the selected run
help()          show this message
    """
    welcome_message = "Interactive issue exploration. Type 'help()' for help."

    def __init__(self, database, database_name):
        self.db = DB(database, database_name, assertions=True)
        self.scope_vars = {
            "help": self.help,
            "runs": self.runs,
            "set_run": self.set_run,
            "issues": self.issues,
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

        print("=" * len(self.welcome_message))
        print(self.welcome_message)
        print("=" * len(self.welcome_message))
        IPython.start_ipython(argv=[], user_ns=self.scope_vars)

    def help(self):
        print(self.help_message)

    def runs(self, use_pager=None):
        use_pager = sys.stdout.isatty() if use_pager is None else use_pager
        pager = page.page if use_pager else page.display_page

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
                "Type 'runs' for available runs.",
                file=sys.stderr,
            )
            return

        self.current_run_id = selected_run.id

    def issues(self, use_pager=None):
        use_pager = sys.stdout.isatty() if use_pager is None else use_pager
        pager = page.page if use_pager else page.display_page

        with self.db.make_session() as session:
            issues = (
                session.query(IssueInstance, Issue)
                .filter(IssueInstance.run_id == self.current_run_id)
                .join(Issue, IssueInstance.issue_id == Issue.id)
                .options(joinedload(IssueInstance.message))
                .all()
            )

        issue_strings = [
            "\n".join(
                [
                    f"Issue {issue_instance.id}",
                    f"    Code: {issue.code}",
                    f" Message: {issue_instance.message.contents}",
                    f"Callable: {issue.callable}",
                    (
                        f"Location: {issue_instance.filename}"
                        f":{SourceLocation.to_string(issue_instance.location)}"
                    ),
                    "-" * 80,
                ]
            )
            for issue_instance, issue in issues
        ]
        issue_output = "\n".join(issue_strings)

        pager(issue_output)
        print(f"Found {len(issues)} issues with run_id {self.current_run_id}.")
