# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from collections import defaultdict
from typing import cast, Dict, List, Optional, Set, TextIO

import click


class CallGraph:
    call_graph: Dict[str, Set[str]]

    def __init__(self, call_graph: object) -> None:
        self.validate_call_graph(call_graph)
        self.call_graph = self.json_to_call_graph(
            cast(Dict[str, List[str]], call_graph)
        )

    @staticmethod
    def json_to_call_graph(call_graph: Dict[str, List[str]]) -> Dict[str, Set[str]]:
        nodes = defaultdict(lambda: set())

        for caller, callees in call_graph.items():
            nodes[caller] = set(callees)

            # skip self calls, since they're irrelevant for trace purposes
            nodes[caller] -= {caller}

        return nodes

    @staticmethod
    def validate_call_graph(call_graph: object) -> None:
        if not isinstance(call_graph, Dict):
            raise ValueError(
                f"Call graph structure in call graph file is not a JSON dict: {type(call_graph)}"
            )

        for caller, callees in call_graph.items():
            if not isinstance(callees, list):
                raise ValueError(
                    f"Value for {caller} in top-level dictionary is not a list of callees: {type(callees)} -> {str(callees)}"
                )

            for callee in callees:
                if not isinstance(callee, str):
                    raise ValueError(
                        f"Callee in callee list for {caller} is not a str: {type(callee)} -> {str(callee)}"
                    )


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
