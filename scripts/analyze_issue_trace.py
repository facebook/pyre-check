# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from collections import defaultdict, deque
from typing import cast, Collection, Deque, Dict, List, Optional, Set, TextIO

import click
from typing_extensions import TypeAlias


Trace: TypeAlias = List[str]


class CallGraph:
    call_graph: Dict[str, Set[str]]
    dependency_graph: Dict[str, Set[str]]

    def __init__(self, call_graph: object) -> None:
        self.validate_call_graph(call_graph)
        self.call_graph = self.json_to_call_graph(
            cast(Dict[str, List[str]], call_graph)
        )
        self.dependency_graph = self.create_dependency_graph(self.call_graph)

    @staticmethod
    def json_to_call_graph(call_graph: Dict[str, List[str]]) -> Dict[str, Set[str]]:
        nodes = defaultdict(lambda: set())

        for caller, callees in call_graph.items():
            nodes[caller] = set(callees)

            # skip self calls, since they're irrelevant for trace purposes
            nodes[caller] -= {caller}

        return nodes

    @staticmethod
    def create_dependency_graph(call_graph: Dict[str, Set[str]]) -> Dict[str, Set[str]]:
        nodes = defaultdict(lambda: set())

        for caller, callees in call_graph.items():
            for callee in callees:
                nodes[callee].add(caller)

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

    @staticmethod
    def node_path_to_str(node_path: Trace) -> str:
        return " -> ".join(node_path)

    def callees(self, caller: str) -> Set[str]:
        return self.call_graph[caller]

    def find_shortest_trace_to_entrypoint(
        self, start_call: str, entrypoints: Collection[str]
    ) -> Optional[Trace]:
        if start_call in entrypoints:
            return [start_call]

        queue: Deque[Trace] = deque([[start_call]])
        visited = set()

        while queue:
            current_node_path = queue.popleft()
            current_node = current_node_path[-1]

            for caller in self.dependency_graph[current_node]:
                if caller in visited:
                    continue
                visited.add(caller)

                next_node_path = current_node_path + [caller]

                if caller in entrypoints:
                    return next_node_path

                queue.append(next_node_path)

        return None


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
@click.argument("call_graph_file", type=click.File("r"))
def main(
    output_file: Optional[TextIO],
    entrypoints_file: Optional[TextIO],
    call_graph_file: TextIO,
) -> None:
    """
    Analyze a Pysa trace from the given CALL_GRAPH_FILE.
    """


if __name__ == "__main__":
    main()
