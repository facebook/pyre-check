#!/usr/bin/env python3
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os

import click

from .cli_lib import commands, common_options
from .context import Context
from .db import DB, DBType
from .lint import lint
from .pipeline.pysa_taint_parser import Parser


LOG: logging.Logger = logging.getLogger("sapp")


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
    ctx: click.Context, repository: str, database_name: str, database_engine: str
) -> None:
    ctx.obj = Context(
        repository=repository,
        database=DB(
            database_engine, os.path.expanduser(database_name), assertions=True
        ),
        parser_class=Parser,
    )
    LOG.debug(f"Context: {ctx.obj}")


for command in commands:
    cli.add_command(command)
cli.add_command(lint)

if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s [%(levelname)s] %(message)s")
    cli()
