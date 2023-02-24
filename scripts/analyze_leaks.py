# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import abc
import json
import sys
import os
from collections import defaultdict, deque
from typing import (
    cast,
    Collection,
    Deque,
    Dict,
    List,
    Optional,
    Set,
    TextIO,
    Union,
    Tuple,
)
from pathlib import Path

from ..client import (
    daemon_socket,
    find_directories,
    identifiers,
)
from ..client.commands import daemon_query
from ..client.language_server import connections

import click
from typing_extensions import TypeAlias


Trace: TypeAlias = List[str]
JSON = Union[Dict[str, "JSON"], List["JSON"], str, int, float, bool, None]
DEFAULT_WORKING_DIRECTORY: str = os.getcwd()


class InputFormat(abc.ABC):
    call_graph: Dict[str, JSON]

    def __init__(self, call_graph: JSON) -> None:
        if not isinstance(call_graph, dict):
            raise ValueError(
                f"Call graph structure in call graph file is not a JSON dict: {type(call_graph)}"
            )

        self.call_graph = call_graph

    @abc.abstractmethod
    def validate_callees(self, callees: List[JSON]) -> Set[str]:
        ...

    def to_call_graph(self) -> Dict[str, Set[str]]:
        result = {}
        call_graph = self.call_graph

        for caller, callees in call_graph.items():
            if not isinstance(callees, list):
                raise ValueError(
                    f"Expected value for caller {caller} to be list of callers with location, got {type(callees)}: {callees}"
                )
            result[caller] = self.validate_callees(callees) - {caller}
        return result

    def get_keys(self) -> Set[str]:
        return set(self.call_graph)


class PysaCallGraphInputFormat(InputFormat):
    def validate_callees(self, callees: List[JSON]) -> Set[str]:
        result = set()
        for callee in callees:
            if not isinstance(callee, str):
                raise ValueError(
                    f"Expected value for individual callee to be a string, got {type(callee)}: {callee}"
                )
            result.add(callee)
        return result


class PyreCallGraphInputFormat(InputFormat):
    def __init__(self, call_graph: JSON) -> None:
        super().__init__(call_graph)
        if "response" in self.call_graph:
            response = self.call_graph["response"]
            if not isinstance(response, dict):
                raise ValueError(
                    f"PyreCallGraphInputFormat expected call graph to have type dict for response key, got {type(response)}: {response}"
                )
            self.call_graph: Dict[str, JSON] = response

    def validate_callees(self, callees: List[JSON]) -> Set[str]:
        result = set()
        for callee in callees:
            if not isinstance(callee, dict):
                raise ValueError(
                    f"Expected value for individual callee to be a dict of callee with location, got {type(callee)}: {callee}"
                )
            if "target" not in callee and "direct_target" not in callee:
                raise ValueError(
                    f"Expected callee dict to have key `target` or `direct_target`: {callee}"
                )
            if "target" in callee and isinstance(callee["target"], str):
                result.add(callee["target"])
            elif "direct_target" in callee and isinstance(callee["direct_target"], str):
                result.add(callee["direct_target"])
            else:
                target_type = (
                    type(callee["target"])
                    if "target" in callee
                    else type(callee["direct_target"])
                )
                raise ValueError(
                    f"Expected callee dict to have key `target` or `direct_target` with type str, got {target_type}"
                )
        return result


class Entrypoints:
    entrypoints: Set[str]

    def __init__(self, entrypoints_json: JSON, known_callers: Set[str]) -> None:
        self.entrypoints = set()

        validate_json_list(entrypoints_json, "ENTRYPOINTS_FILE", "top-level")

        for entrypoint in cast(List[str], entrypoints_json):
            if entrypoint in known_callers:
                self.entrypoints.add(entrypoint)
                continue

            # if the entrypoint is not found in the call graph, then try truncating the last part of the qualified name and retry
            parent_function = ".".join(entrypoint.split(".")[:-1])
            if parent_function in known_callers:
                self.entrypoints.add(parent_function)
            else:
                print(
                    f"Unknown entrypoint {entrypoint} and parent function {parent_function}, skipping...",
                    file=sys.stderr,
                )


class DependencyGraph:
    dependency_graph: Dict[str, Set[str]]
    entrypoints: Entrypoints

    def __init__(self, input_call_graph: InputFormat, entrypoints: Entrypoints) -> None:
        self.entrypoints = entrypoints
        self.dependency_graph = defaultdict(lambda: set())
        call_graph = input_call_graph.to_call_graph()

        for caller, callees in call_graph.items():
            for callee in callees:
                if caller == callee:
                    # skip self-references
                    continue
                self.dependency_graph[callee].add(caller)

    def find_traces_for_callees(
        self, callees: Collection[str]
    ) -> Dict[str, Optional[Trace]]:
        result = {}
        for callee in callees:
            if callee in self.dependency_graph and callee not in result:
                result[callee] = self.find_shortest_trace_to_entrypoint(callee)
            elif callee not in result:
                result[callee] = None
        return result

    def find_shortest_trace_to_entrypoint(self, start_call: str) -> Optional[Trace]:
        if start_call in self.entrypoints.entrypoints:
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

                if caller in self.entrypoints.entrypoints:
                    return next_node_path

                queue.append(next_node_path)

        return []

    @staticmethod
    def node_path_to_str(node_path: Trace) -> str:
        return " -> ".join(node_path)


class CallGraph:
    call_graph: Dict[str, Set[str]]
    entrypoints: Entrypoints

    def __init__(self, call_graph: InputFormat, entrypoints: Entrypoints) -> None:
        self.call_graph = call_graph.to_call_graph()
        self.entrypoints = entrypoints

    def get_transitive_callees_and_traces(self) -> Dict[str, Trace]:
        transitive_callees = {}
        queue: Deque[Tuple[str, Trace]] = deque(
            [(entrypoint, [entrypoint]) for entrypoint in self.entrypoints.entrypoints]
        )

        while queue:
            callable, trace = queue.popleft()
            if callable in transitive_callees:
                continue
            transitive_callees[callable] = trace
            if callable in self.call_graph:
                queue += [
                    (next_callable, trace + [next_callable])
                    for next_callable in self.call_graph[callable]
                ]

        return transitive_callees

    @staticmethod
    def prepare_issues_for_query(callees: Collection[str]) -> str:
        single_callee_query = [f"global_leaks({callee})" for callee in callees]
        return "batch(" + ", ".join(single_callee_query) + ")"

    @staticmethod
    def analyze_pyre_query_results(pyre_results: object) -> Dict[str, List[object]]:
        results = {"global_leaks": [], "errors": []}
        if not isinstance(pyre_results, dict):
            raise RuntimeError(
                f"Expected dict for Pyre query results, got {type(pyre_results)}: {pyre_results}"
            )
        if "response" not in pyre_results:
            raise RuntimeError("`response` key not in Pyre query results", pyre_results)
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
                "response" in query_response and "errors" in query_response["response"]
            ):
                results["global_leaks"] += query_response["response"]["errors"]
            else:
                raise RuntimeError(
                    "Unexpected response from Pyre query", query_response
                )

        return results

    def find_issues(self, search_start_path: Path) -> Dict[str, List[object]]:
        all_callables = set(self.get_transitive_callees_and_traces())
        query_str = self.prepare_issues_for_query(all_callables)

        project_root = find_directories.find_global_and_local_root(search_start_path)
        if not project_root:
            raise ValueError(
                f"Given project path {search_start_path} is not in a Pyre project"
            )

        local_relative_path = (
            str(project_root.local_root.relative_to(project_root.global_root))
            if project_root.local_root
            else None
        )

        project_identifier = identifiers.get_project_identifier(
            project_root.global_root, local_relative_path
        )

        socket_path = daemon_socket.get_socket_path(
            project_identifier,
            flavor=identifiers.PyreFlavor.CLASSIC,
        )

        try:
            response = daemon_query.execute_query(socket_path, query_str)
            return self.analyze_pyre_query_results(response.payload)
        except connections.ConnectionFailure as e:
            raise RuntimeError(
                "A running Pyre server is required for queries to be responded. "
                "Please run `pyre` first to set up a server."
            ) from e


def validate_json_list(json_list: JSON, from_file: str, level: str) -> None:
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


def load_json_from_file(file_handle: TextIO, file_name: str) -> JSON:
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
@click.option(
    "--call_graph_kind",
    type=click.Choice(["pyre", "pysa"], case_sensitive=False),
    default="pyre",
    help="The format of the call_graph_file, see CALL_GRAPH_FILE for more info.",
)
@click.option(
    "--project-path",
    type=str,
    default=DEFAULT_WORKING_DIRECTORY,
    help="The path to the project in which global leaks will be searched for. \
    The given directory or parent directory must have a global .pyre_configuration. \
    Default: current directory.",
)
def leaks(
    call_graph_file: TextIO,
    entrypoints_file: TextIO,
    call_graph_kind: str,
    project_path: str,
) -> None:
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
    call_graph_data = load_json_from_file(call_graph_file, "CALL_GRAPH_FILE")
    entrypoints_json = load_json_from_file(entrypoints_file, "ENTRYPOINTS_FILE")

    input_format = (
        PyreCallGraphInputFormat(call_graph_data)
        if call_graph_kind == "pyre"
        else PysaCallGraphInputFormat(call_graph_data)
    )

    entrypoints = Entrypoints(entrypoints_json, input_format.get_keys())

    call_graph = CallGraph(input_format, entrypoints)

    issues = call_graph.find_issues(Path(project_path))
    print(json.dumps(issues))


@analyze.command()
@click.argument("issues_file", type=click.File("r"))
@click.argument("call_graph_file", type=click.File("r"))
@click.argument("entrypoints_file", type=click.File("r"))
@click.option(
    "--call_graph_kind",
    type=click.Choice(["pyre", "pysa"], case_sensitive=False),
    default="pyre",
    help="The format of the call_graph_file, see CALL_GRAPH_FILE for more info.",
)
def trace(
    issues_file: TextIO,
    call_graph_file: TextIO,
    entrypoints_file: TextIO,
    call_graph_kind: str,
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
    entrypoints_json = load_json_from_file(entrypoints_file, "ENTRYPOINTS_FILE")

    input_format = (
        PyreCallGraphInputFormat(call_graph_data)
        if call_graph_kind == "pyre"
        else PysaCallGraphInputFormat(call_graph_data)
    )

    entrypoints = Entrypoints(entrypoints_json, input_format.get_keys())

    dependency_graph = DependencyGraph(input_format, entrypoints)

    validate_json_list(issues, "ISSUES_FILE", "top level")
    found_paths = dependency_graph.find_traces_for_callees(cast(List[str], issues))

    print(json.dumps(found_paths))


if __name__ == "__main__":
    analyze()
