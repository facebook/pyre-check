import logging

import click

from .cli_lib import commands, common_options
from .context import Context
from .db import DB, DBType
from .lint import lint
from .pysa_taint_parser import Parser


logger = logging.getLogger("sapp")


@common_options
@click.option(
    "--database-engine",
    "--database",
    type=click.Choice([DBType.SQLITE, DBType.MEMORY]),
    default=DBType.SQLITE,
    help="database engine to use",
)
@click.pass_context
def cli(
    ctx: click.Context, repository: str, database_name: str, database_engine: DBType
):
    ctx.obj = Context(
        repository=repository,
        database=DB(database_engine, database_name, assertions=True),
        parser_class=Parser,
    )
    logger.debug(f"Context: {ctx.obj}")


for command in commands:
    cli.add_command(command)
cli.add_command(lint)

if __name__ == "__main__":
    cli()
