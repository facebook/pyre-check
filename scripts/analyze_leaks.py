# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import sys
import subprocess
from collections import defaultdict, deque
from typing import cast, Collection, Deque, Dict, List, Optional, Set, TextIO

import click
from typing_extensions import TypeAlias


Trace: TypeAlias = List[str]


class CallGraph:
    call_graph: Dict[str, Set[str]]
    dependency_graph: Dict[str, Set[str]]
    entrypoints: Set[str]

    def __init__(self, call_graph: object, entrypoints: object) -> None:
        call_graph = self.validate_call_graph(call_graph)
        self.call_graph = self.json_to_call_graph(call_graph)
        self.dependency_graph = self.create_dependency_graph(self.call_graph)

        self.entrypoints = self.validate_and_get_entrypoints(
            entrypoints, set(self.call_graph)
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
    def create_dependency_graph(call_graph: Dict[str, Set[str]]) -> Dict[str, Set[str]]:
        nodes = defaultdict(lambda: set())

        for caller, callees in call_graph.items():
            for callee in callees:
                nodes[callee].add(caller)

        return nodes

    @staticmethod
    def validate_json_list(json_list: object, from_file: str, level: str) -> None:
        if not isinstance(json_list, list):
            raise ValueError(
                f"Expected {level} value in {from_file} file to be a list, got: {type(json_list)}"
            )

        for i, value in enumerate(json_list):
            if not isinstance(value, str):
                raise ValueError(
                    f"Expected {level} list value in {from_file} at position {i} to be a string, \
                    got: {type(value)}: {value}"
                )

    @staticmethod
    def validate_call_graph(call_graph: object) -> Dict[str, List[str]]:
        result: Dict[str, List[str]] = defaultdict(lambda: list())
        if not isinstance(call_graph, Dict):
            raise ValueError(
                f"Call graph structure in call graph file is not a JSON dict: {type(call_graph)}"
            )

        for caller, callees in call_graph.items():
            if not isinstance(callees, list):
                raise ValueError(
                    f"Expected value for caller {caller} to be list of callers with location, got {type(callees)}: {callees}"
                )
            for callee in callees:
                if isinstance(callee, str):
                    result[caller].append(callee)
                elif isinstance(callee, dict):
                    target = callee.get("target", None)
                    direct_target = callee.get("direct_target", None)
                    if target is None and direct_target is None:
                        raise ValueError(
                            f"Expected callee dict to have key `target` or `direct_target`: {callee}"
                        )
                    if target and isinstance(target, str):
                        result[caller].append(target)
                    elif direct_target and isinstance(direct_target, str):
                        result[caller].append(direct_target)
                    else:
                        target_type = (
                            type(target) if target is not None else type(direct_target)
                        )
                        raise ValueError(
                            f"Expected callee dict to have key `target` or `direct_target` with type str, got {target_type}"
                        )
                else:
                    raise ValueError(
                        f"Expected value for individual callee to be a string or dict of callee with location, got {type(callee)}: {callee}"
                    )
        return result

    @staticmethod
    def validate_and_get_entrypoints(
        entrypoints_json: object, known_callers: Set[str]
    ) -> Set[str]:
        entrypoints = set()
        CallGraph.validate_json_list(entrypoints_json, "ENTRYPOINTS_FILE", "top level")

        for entrypoint in cast(List[str], entrypoints_json):
            if entrypoint in known_callers:
                entrypoints.add(entrypoint)
                continue

            # if the entrypoint is not found in the call graph, then try truncating the last part of the qualified name and retry
            parent_function = ".".join(entrypoint.split(".")[:-1])
            if parent_function in known_callers:
                entrypoints.add(parent_function)
            else:
                print(
                    f"Unknown entrypoint {entrypoint} and parent function {parent_function}, skipping...",
                    file=sys.stderr,
                )
        return entrypoints

    @staticmethod
    def node_path_to_str(node_path: Trace) -> str:
        return " -> ".join(node_path)

    def callees(self, caller: str) -> Set[str]:
        return self.call_graph[caller]

    def find_shortest_trace_to_entrypoint(self, start_call: str) -> Optional[Trace]:
        if start_call in self.entrypoints:
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

                if caller in self.entrypoints:
                    return next_node_path

                queue.append(next_node_path)

        return []

    def find_traces_for_callees(
        self, callees: Collection[str]
    ) -> Dict[str, Optional[Trace]]:
        result: Dict[str, Optional[Trace]] = {}
        for callee in callees:
            if callee in self.dependency_graph and callee not in result:
                result[callee] = self.find_shortest_trace_to_entrypoint(callee)
            elif callee not in result:
                result[callee] = None
        return result

    def get_all_callees(self) -> Set[str]:
        transitive_callees = set()
        stack = list(self.entrypoints)

        while stack:
            callable = stack.pop()
            if callable in transitive_callees:
                continue
            transitive_callees.add(callable)
            if callable in self.call_graph:
                stack += self.call_graph[callable]

        return transitive_callees

    @staticmethod
    def prepare_issues_for_query(callees: Collection[str]) -> str:
        single_callee_query = [f"global_leaks({callee})" for callee in callees]
        return "batch(" + ", ".join(single_callee_query) + ")"

    @staticmethod
    def analyze_pyre_query_results(pyre_stdout: str) -> Dict[str, List[object]]:
        results = {"global_leaks": [], "errors": []}
        try:
            pyre_results = json.loads(pyre_stdout)
            if not isinstance(pyre_results, dict):
                raise RuntimeError(
                    f"Expected dict for Pyre query results, got {type(pyre_results)}: {pyre_results}"
                )
            if "response" not in pyre_results:
                raise RuntimeError(
                    "`response` key not in Pyre query results", pyre_results
                )
            if not isinstance(pyre_results["response"], list):
                response = pyre_results["response"]
                raise RuntimeError(
                    f"Expected response value type to be list, got {type(response)}: {response}"
                )
            for query_response in pyre_results["response"]:
                if not isinstance(query_response, dict):
                    raise RuntimeError(
                        f"Expected dict for pyre response list type, got {type(query_response)}: {query_response}"
                    )
                elif "error" in query_response:
                    results["errors"].append(query_response["error"])
                elif (
                    "response" in query_response
                    and "errors" in query_response["response"]
                ):
                    results["global_leaks"] += query_response["response"]["errors"]
                else:
                    raise RuntimeError(
                        "Unexpected response from Pyre query", query_response
                    )

            return results
        except json.JSONDecodeError as e:
            raise RuntimeError("Pyre query returned non-JSON response") from e

    def find_issues(self) -> Dict[str, List[object]]:
        all_callables = self.get_all_callees()
        query = self.prepare_issues_for_query(all_callables)

        pyre_process = subprocess.run(
            ["pyre", "-n", "query", query], capture_output=True, text=True
        )

        if pyre_process.returncode != 0:
            raise Exception("Pyre returned nonzero exit code", pyre_process.stderr)

        return self.analyze_pyre_query_results(pyre_process.stdout)


def load_json_from_file(file_handle: TextIO, file_name: str) -> object:
    try:
        return json.load(file_handle)
    except json.JSONDecodeError as e:
        raise ValueError(f"Error loading {file_name} as JSON") from e


@click.group()
def analyze() -> None:
    """
    Performs analyses over Pyre's results using a call graph and list of entrypoints.
    """
    pass


@analyze.command()
@click.argument("call_graph_file", type=click.File("r"))
@click.argument("entrypoints_file", type=click.File("r"))
def leaks(call_graph_file: TextIO, entrypoints_file: TextIO) -> None:
    """
    Find global leaks for the given entrypoints and their transitive callees.

    The output of this script will be a JSON object containing two keys:
    - `global_leaks`: any global leaks that are returned from `pyre query "global_leaks(...)"` for
        callables checked.
    - `errors`: any errors that occurred during the analysis, for example, a definition not
        found for a callable

    CALL_GRAPH_FILE: a file containing either:
      - a JSON dict mapping caller qualified paths to a list of callee qualified paths (can be
        return from `pyre analyze --dump-call-graph ...`)
      - a JSON dict mapping caller qualified paths to a list of callee
        objects returned from `pyre query "dump_call_graph()"`
    ENTRYPOINTS_FILE: a file containing a JSON list of qualified paths for entrypoints
    """
    pass


@analyze.command()
@click.argument("issues_file", type=click.File("r"))
@click.argument("call_graph_file", type=click.File("r"))
@click.argument("entrypoints_file", type=click.File("r"))
def trace(
    issues_file: TextIO,
    call_graph_file: TextIO,
    entrypoints_file: TextIO,
) -> None:
    """
    Get a list of traces from callable to entrypoint.

    The output of this script will be a JSON object mapping a callee to a list of strings
    representing the path from the callee to an entrypoint. The values of the output object
    will be one of the following:
    - List[str]: the path from the callee to the entrypoint
    - empty List: no path mapping the callee to any entrypoint
    - None: the callee given is not present in the dependency graph

    ISSUES_FILE: a file containing a JSON list of callee strings to find traces for
    CALL_GRAPH_FILE: a file containing a JSON dict mapping caller strings to a list of callee strings
    ENTRYPOINTS_FILE: a file containing a JSON list of caller strings, which represent entrypoints
      transitive callees will be found
    """
    # TODO (T141832117): consume method override information to perform traces on call graphs
    #   with overrides

    issues = load_json_from_file(issues_file, "ISSUES_FILE")
    call_graph_data = load_json_from_file(call_graph_file, "CALL_GRAPH_FILE")
    entrypoints = load_json_from_file(entrypoints_file, "ENTRYPOINTS_FILE")

    call_graph = CallGraph(call_graph_data, entrypoints)

    CallGraph.validate_json_list(issues, "ISSUES_FILE", "top level")
    found_paths = call_graph.find_traces_for_callees(cast(List[str], issues))

    print(json.dumps(found_paths))


if __name__ == "__main__":
    analyze()
