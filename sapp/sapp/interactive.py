#!/usr/bin/env python3

import cmd

from sapp.db import DB
from sapp.models import Issue, IssueInstance, SourceLocation
from sqlalchemy.orm import joinedload


class Interactive(cmd.Cmd):
    intro = "Welcome to issue explorer. Type help or ? to list commands."

    def __init__(self, database, database_name):
        super().__init__()
        self.db = DB(database, database_name, assertions=True)
        self.prompt = f"{self.db.dbtype}:{self.db.dbname}> "

    def do_list(self, arg):
        "list all issues instances"
        with self.db.make_session() as session:
            issues = (
                session.query(IssueInstance, Issue)
                .join(Issue, IssueInstance.issue_id == Issue.id)
                .options(joinedload(IssueInstance.message))
                .all()
            )

        for issue_instance, issue in issues:
            print(f"Issue {issue_instance.id}")
            print(f"    Code: {issue.code}")
            print(f" Message: {issue_instance.message.contents}")
            print(f"Callable: {issue.callable}")
            print(
                f"Location: {issue_instance.filename}"
                f":{SourceLocation.to_string(issue_instance.location)}"
            )
            print("-" * 80)
