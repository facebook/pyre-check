# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
from operator import itemgetter
from pathlib import Path
from typing import List

import click
from sqlalchemy.orm import aliased

from .cli_lib import require_option
from .models import Issue, IssueInstance, SharedText, TraceFrame


# pyre-fixme[5]: Global expression must be annotated.
MessageText = aliased(SharedText)
# pyre-fixme[5]: Global expression must be annotated.
FilenameText = aliased(SharedText)
# pyre-fixme[5]: Global expression must be annotated.
CallerText = aliased(SharedText)
# pyre-fixme[5]: Global expression must be annotated.
CalleeText = aliased(SharedText)


@click.command()
@click.pass_context
@click.option("--run-id", type=int, required=True)
@click.argument(
    "filenames",
    type=click.Path(exists=True, readable=True, resolve_path=True),
    nargs=-1,
    required=True,
)
def lint(click_ctx: click.Context, run_id: int, filenames: List[str]) -> None:
    """Output DB models in a lint-friendly format"""
    ctx = click_ctx.obj
    require_option(click_ctx, "repository")

    paths = [Path(p).resolve() for p in filenames]
    root = Path(ctx.repository).resolve()
    relative = [str(Path(f).relative_to(root)) for f in paths]

    with ctx.database.make_session() as session:
        instances = (
            session.query(
                IssueInstance.location,
                FilenameText.contents.label("filename"),
                MessageText.contents.label("message"),
                Issue.code,
            )
            .filter(IssueInstance.run_id == run_id)
            .join(Issue, Issue.id == IssueInstance.issue_id)
            .join(FilenameText, FilenameText.id == IssueInstance.filename_id)
            .filter(FilenameText.contents.in_(relative))
            .join(MessageText, MessageText.id == IssueInstance.message_id)
            .all()
        )

    with ctx.database.make_session() as session:
        frames = (
            session.query(
                TraceFrame.callee_location,
                TraceFrame.kind,
                TraceFrame.callee_port,
                TraceFrame.caller_port,
                CallerText.contents.label("caller"),
                CalleeText.contents.label("callee"),
                FilenameText.contents.label("filename"),
            )
            .filter(TraceFrame.run_id == run_id)
            .join(FilenameText, FilenameText.id == TraceFrame.filename_id)
            .filter(FilenameText.contents.in_(relative))
            .join(CallerText, CallerText.id == TraceFrame.caller_id)
            .join(CalleeText, CalleeText.id == TraceFrame.callee_id)
            .all()
        )

    # pyre-fixme[53]: Captured variable `root` is not annotated.
    # pyre-fixme[3]: Return type must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    def entry(filename, code, message, location):
        return {
            "filename": str(root / filename),
            "code": code,
            "message": message,
            "line": location.line_no,
            "col": location.begin_column,
            "length": location.begin_column + location.end_column + 1,
        }

    lints = [
        entry(i.filename, str(i.code), i.message, i.location) for i in instances
    ] + [
        entry(
            i.filename,
            i.kind.name,
            f"{i.caller}:{i.caller_port} -> {i.callee}->{i.callee_port}",
            i.callee_location,
        )
        for i in frames
    ]

    for lint in sorted(lints, key=itemgetter("filename", "line", "code", "col")):
        click.echo(json.dumps(lint))
