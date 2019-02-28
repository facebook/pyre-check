#!/usr/bin/env python3

import IPython
from click import Choice, Path, argument, group, option
from sapp.analysis_output import AnalysisOutput
from sapp.database_saver import DatabaseSaver
from sapp.db import DB
from sapp.interactive import Interactive
from sapp.model_generator import ModelGenerator
from sapp.models import PrimaryKeyGenerator
from sapp.pipeline import Pipeline
from sapp.pysa_taint_parser import Parser
from sapp.trim_trace_graph import TrimTraceGraph


@group()
def cli():
    pass


@cli.command(help="interactive exploration of issues")
@option(
    "--database",
    type=Choice(["memory", "sqlite"]),
    default="sqlite",
    help="database engine to use",
)
@option("--database-name", "--dbname", type=str)
def explore(database, database_name):
    scope_vars = Interactive(database, database_name).setup()
    IPython.start_ipython(argv=[], user_ns=scope_vars)


@cli.command(help="parse static analysis output and save to disk")
@option(
    "--database",
    type=Choice(["memory", "sqlite"]),
    default="memory",
    help="database engine to use",
)
@option("--database-name", "--dbname", type=str)
@option("--run-kind", type=str)
@option("--repository", type=str)
@option("--branch", type=str)
@option("--commit-hash", type=str)
@option("--job-id", type=str)
@option("--differential-id", type=int)
@option(
    "--previous-issue-handles",
    type=Path(exists=True),
    help=(
        "file containing list of issue handles to compare INPUT_FILE to "
        "(preferred over --previous-input)"
    ),
)
@option(
    "--previous-input",
    type=Path(exists=True),
    help="static analysis output to compare INPUT_FILE to",
)
@option(
    "--linemap",
    type=Path(exists=True),
    help="json file mapping new locations to old locations",
)
@argument("input_file", type=Path(exists=True))
def analyze(
    database,
    database_name,
    run_kind,
    repository,
    branch,
    commit_hash,
    job_id,
    differential_id,
    previous_issue_handles,
    previous_input,
    linemap,
    input_file,
):
    # Store all options in the right places
    summary_blob = {
        "run_kind": run_kind,
        "compress": lambda x: x,
        "repository": repository,
        "branch": branch,
        "commit_hash": commit_hash,
        "old_linemap_file": linemap,
    }

    if job_id is None and differential_id is not None:
        job_id = "user_input_" + str(differential_id)
    summary_blob["job_id"] = job_id

    if previous_issue_handles:
        summary_blob["previous_issue_handles"] = AnalysisOutput.from_file(
            previous_issue_handles
        )
    elif previous_input:
        previous_input = AnalysisOutput.from_file(previous_input)

    # Construct pipeline
    input_files = (AnalysisOutput.from_file(input_file), previous_input)
    pipeline_steps = [
        Parser(),
        ModelGenerator(),
        TrimTraceGraph(),
        DatabaseSaver(
            DB(database, database_name, assertions=True), PrimaryKeyGenerator()
        ),
    ]
    pipeline = Pipeline(pipeline_steps)
    pipeline.run(input_files, summary_blob)


if __name__ == "__main__":
    cli()
