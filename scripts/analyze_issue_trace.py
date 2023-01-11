# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Optional, TextIO

import click


@click.command()
@click.option(
    "-o",
    "--output-file",
    default=None,
    type=click.File("w", lazy=True),
    help="The file to write complete program traces to.",
)
@click.option(
    "-e",
    "--entrypoints-file",
    default=None,
    type=click.File("r"),
    help="The file to read entrypoints from. If not provided, entrypoints can be entered in interactive mode.",
)
@click.option("--debug", "-d", is_flag=True, default=False)
@click.argument("call_graph_file", type=click.File("r"))
def main(
    output_file: Optional[TextIO],
    entrypoints_file: Optional[TextIO],
    debug: bool,
    call_graph_file: TextIO,
) -> None:
    """
    Analyze a Pysa trace from the given CALL_GRAPH_FILE.
    """


if __name__ == "__main__":
    main()
