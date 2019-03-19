import logging

import click
from sapp.cli_lib import commands, common_options
from sapp.context import Context
from sapp.db import DBType
from sapp.diagnostics.cli import diagnostics


logger = logging.getLogger("sapp")


@common_options
@click.pass_context
def cli(
    ctx: click.Context, repository: str, database_engine: DBType, database_name: str
):
    ctx.obj = Context(
        repository=repository,
        database_engine=database_engine,
        database_name=database_name,
    )
    logger.debug(f"Context: {ctx.obj}")


for command in commands:
    cli.add_command(command)
cli.add_command(diagnostics)

if __name__ == "__main__":
    cli()
