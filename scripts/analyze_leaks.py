# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import abc
import enum
import json
import keyword
import os
import sys
from collections import defaultdict, deque
from dataclasses import dataclass
from pathlib import Path
from typing import (
    cast,
    Collection,
    Deque,
    Dict,
    List,
    Optional,
    Set,
    TextIO,
    Tuple,
    Union,
)

import click
from typing_extensions import TypeAlias

from ..client import daemon_socket, find_directories, identifiers
from ..client.commands import daemon_query
from ..client.language_server import connections


Trace: TypeAlias = List[str]
JSON = Union[Dict[str, "JSON"], List["JSON"], str, int, float, bool, None]
DEFAULT_WORKING_DIRECTORY: str = os.getcwd()


class InputFormat(abc.ABC):
    call_graph: Dict[str, Set[str]]
    original_call_graph: Dict[str, JSON]

    def __init__(self, call_graph: JSON) -> None:
        self.original_call_graph = self.validate_top_level_dict(call_graph)
        self.call_graph = self._to_call_graph()

    @staticmethod
    def validate_top_level_dict(call_graph: JSON) -> Dict[str, JSON]:
        if not isinstance(call_graph, dict):
            raise ValueError(
                f"Call graph structure in call graph file is not a JSON dict: {type(call_graph)}"
            )
        return call_graph

    @abc.abstractmethod
    def extract_callee(self, callee: JSON) -> str:
        ...

    def validate_callees(self, callees: List[JSON]) -> Set[str]:
        return {self.extract_callee(callee) for callee in callees}

    def _to_call_graph(self) -> Dict[str, Set[str]]:
        result = {}
        call_graph = self.original_call_graph

        for caller, callees in call_graph.items():
            if not isinstance(callees, list):
                raise ValueError(
                    f"Expected value for caller {caller} to be list of callers with location, got {type(callees)}: {callees}"
                )
            formatted_qualifier = self.extract_caller(caller)
            result[formatted_qualifier] = self.validate_callees(callees) - {
                formatted_qualifier
            }
        return result

    def get_keys(self) -> Set[str]:
        return set(self.call_graph)

    @abc.abstractmethod
    def extract_caller(self, qualifier: str) -> str:
        ...


class PysaCallGraphInputFormat(InputFormat):
    def extract_callee(self, callee: JSON) -> str:
        if not isinstance(callee, str):
            raise ValueError(
                f"Expected value for individual callee to be a string, got {type(callee)}: {callee}"
            )
        return callee

    def extract_caller(self, qualifier: str) -> str:
        return qualifier


class PyreCallGraphInputFormat(InputFormat):
    def __init__(self, call_graph: JSON) -> None:
        self.original_call_graph = self.validate_top_level_dict(call_graph)
        if "response" in self.original_call_graph:
            response = self.original_call_graph["response"]
            if not isinstance(response, dict):
                raise ValueError(
                    f"PyreCallGraphInputFormat expected call graph to have type dict for response key, got {type(response)}: {response}"
                )
            self.original_call_graph: Dict[str, JSON] = response
        self.call_graph: Dict[str, Set[str]] = self._to_call_graph()

    def extract_callee(self, callee: JSON) -> str:
        if not isinstance(callee, dict):
            raise ValueError(
                f"Expected value for individual callee to be a dict of callee with location, got {type(callee)}: {callee}"
            )
        if "target" not in callee and "direct_target" not in callee:
            raise ValueError(
                f"Expected callee dict to have key `target` or `direct_target`: {callee}"
            )

        target = callee.get("target")
        direct_target = callee.get("direct_target")
        if target and isinstance(target, str):
            return target
        elif direct_target and isinstance(direct_target, str):
            return direct_target
        else:
            target_type = (
                type(callee["target"])
                if "target" in callee
                else type(callee["direct_target"])
            )
            raise ValueError(
                f"Expected callee dict to have key `target` or `direct_target` with type str, got {target_type}"
            )

    def extract_caller(self, qualifier: str) -> str:
        return qualifier


class DynamicCallGraphInputFormat(InputFormat):
    def extract_caller(self, qualifier: str) -> str:
        return self.format_qualifier(qualifier)

    @staticmethod
    def format_qualifier(qualifier: str) -> str:
        qualifier = qualifier.replace("<locals>.", "")
        split = qualifier.split(":")
        if len(split) != 2:
            return qualifier
        module_qualifier, callable = split
        return f"{module_qualifier}.{callable}"

    def extract_callee(self, callee: JSON) -> str:
        if not isinstance(callee, str):
            raise ValueError(
                f"Expected value for individual callee to be a string, got {type(callee)}: {callee}"
            )
        mapped_qualifier = self.format_qualifier(callee)
        return mapped_qualifier


class UnionCallGraphFormat(InputFormat):
    def __init__(self) -> None:
        self.call_graph: Dict[str, Set[str]] = defaultdict(set)

    def extract_callee(self, callee: JSON) -> str:
        if not isinstance(callee, str):
            raise ValueError(
                f"Expected value for individual callee to be a string, got {type(callee)}: {callee}"
            )
        return callee

    def extract_caller(self, qualifier: str) -> str:
        return qualifier

    def union_call_graph(self, call_graph: Dict[str, Set[str]]) -> None:
        if self.call_graph:
            for k, v in call_graph.items():
                self.call_graph[k] |= v
        else:
            self.call_graph = defaultdict(set, call_graph)


class InputType(enum.Enum):
    PYSA = PysaCallGraphInputFormat
    PYRE = PyreCallGraphInputFormat
    DYNAMIC = DynamicCallGraphInputFormat

    @staticmethod
    def members() -> List[str]:
        return [input_type.name for input_type in InputType]


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
        call_graph = input_call_graph.call_graph

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


@dataclass(frozen=True)
class LeakAnalysisScriptError:
    error_message: str
    bad_value: JSON

    def to_json(self) -> JSON:
        return {"error_message": self.error_message, "bad_value": self.bad_value}


@dataclass(frozen=True)
class LeakAnalysisResult:
    global_leaks: List[Dict[str, JSON]]
    query_errors: List[JSON]
    script_errors: List[LeakAnalysisScriptError]

    def _script_errors_to_json(self) -> List[JSON]:
        return [script_error.to_json() for script_error in self.script_errors]

    def to_json(self) -> str:
        return json.dumps(
            {
                "global_leaks": self.global_leaks,
                "query_errors": self.query_errors,
                "script_errors": self._script_errors_to_json(),
            }
        )


class CallGraph:
    call_graph: Dict[str, Set[str]]
    entrypoints: Entrypoints

    def __init__(self, call_graph: InputFormat, entrypoints: Entrypoints) -> None:
        self.call_graph = call_graph.call_graph
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
    def is_valid_callee(callee: str) -> bool:
        components = callee.strip().split(".")
        is_valid_callee = all(component.isidentifier() and not keyword.iskeyword(component) for component in components)
        return is_valid_callee

    @staticmethod
    def prepare_issues_for_query(callees: Collection[str]) -> str:
        single_callee_query = [f"global_leaks({callee})" for callee in callees if CallGraph.is_valid_callee(callee)]
        return "batch(" + ", ".join(single_callee_query) + ")"

    @staticmethod
    def collect_pyre_query_results(pyre_results: object) -> LeakAnalysisResult:
        global_leaks: List[Dict[str, JSON]] = []
        query_errors: List[JSON] = []
        script_errors: List[LeakAnalysisScriptError] = []
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
                script_errors.append(
                    LeakAnalysisScriptError(
                        error_message=f"Expected dict for pyre response list type, got {type(query_response)}",
                        bad_value=query_response,
                    )
                )
            elif "error" in query_response:
                query_errors.append(query_response["error"])
            elif (
                "response" in query_response and "errors" in query_response["response"]
            ):
                global_leaks += query_response["response"]["errors"]
            else:
                script_errors.append(
                    LeakAnalysisScriptError(
                        error_message="Unexpected single query response from Pyre",
                        bad_value=query_response,
                    )
                )

        return LeakAnalysisResult(
            global_leaks=global_leaks,
            query_errors=query_errors,
            script_errors=script_errors,
        )

    @staticmethod
    def attach_trace_to_query_results(
        pyre_results: LeakAnalysisResult, callables_and_traces: Dict[str, Trace]
    ) -> None:
        for issue in pyre_results.global_leaks:
            if "define" not in issue:
                pyre_results.script_errors.append(
                    LeakAnalysisScriptError(
                        error_message="Key `define` not present in global leak result, skipping trace",
                        bad_value=issue,
                    )
                )
                continue

            define = issue["define"]
            if define not in callables_and_traces:
                pyre_results.script_errors.append(
                    LeakAnalysisScriptError(
                        error_message="Define not known in analyzed callables, skipping trace",
                        bad_value=issue,
                    )
                )
                continue

            trace = callables_and_traces[define]
            issue["trace"] = cast(JSON, trace)

    def find_issues(self, search_start_path: Path) -> LeakAnalysisResult:
        all_callables = self.get_transitive_callees_and_traces()
        query_str = self.prepare_issues_for_query(all_callables.keys())

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
            collected_results = self.collect_pyre_query_results(response.payload)
            self.attach_trace_to_query_results(collected_results, all_callables)
            return collected_results
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
@click.option("--call-graph-kind-and-path", type=(click.Choice(InputType.members(), case_sensitive=False), click.File("r")), multiple=True, required=True)
@click.argument("entrypoints_file", type=click.File("r"))
@click.option(
    "--project-path",
    type=str,
    default=DEFAULT_WORKING_DIRECTORY,
    help="The path to the project in which global leaks will be searched for. \
    The given directory or parent directory must have a global .pyre_configuration. \
    Default: current directory.",
)
def leaks(
    call_graph_kind_and_path: Tuple[Tuple[str, TextIO], ...],
    entrypoints_file: TextIO,
    project_path: str,
) -> None:
    """
    Find global leaks for the given entrypoints and their transitive callees.

    The output of this script will be a JSON object containing three keys:
    - `global_leaks`: any global leaks that are returned from `pyre query "global_leaks(...)"` for
        callables checked.
    - `query_errors`: any errors that occurred during pyre's analysis, for example, no qualifier found
    - `script_errors`: any errors that occurred during the analysis, for example, a definition not
        found for a callable

    CALL_GRAPH_KIND_AND_PATH: a tuple of the following form (KIND, PATH) where
      - KIND is a string specifying the format type of the call graph e.g. pyre/pysa/dynanmic
      - PATH points to a JSON file which is a dict mapping caller qualified paths to a list of callee qualified paths (e.g. can be
        return from `pyre analyze --dump-call-graph ...` or `pyre query "dump_call_graph()"`)
    ENTRYPOINTS_FILE: a file containing a JSON list of qualified paths for entrypoints

    Example usage: ./analyze_leaks.py -- leaks <ENTRYPOINTS_FILE> --call-graph-kind-and-path <KIND1> <CALL_GRAPH_1> --call-graph-kind-and-path <KIND2> <CALL_GRAPH2>
    """
    entrypoints_json = load_json_from_file(entrypoints_file, "ENTRYPOINTS_FILE")
    input_format = UnionCallGraphFormat()
    for call_graph_kind, call_graph_file in call_graph_kind_and_path:
        call_graph_data = load_json_from_file(call_graph_file, "CALL_GRAPH_FILE")

        current_input_format_type = InputType[call_graph_kind.upper()].value
        current_input_format = current_input_format_type(call_graph_data)
        input_format.union_call_graph(current_input_format.call_graph)


    entrypoints = Entrypoints(entrypoints_json, input_format.get_keys())

    call_graph = CallGraph(input_format, entrypoints)

    issues = call_graph.find_issues(Path(project_path))
    print(issues.to_json())


@analyze.command()
@click.argument("issues_file", type=click.File("r"))
@click.argument("call_graph_file", type=click.File("r"))
@click.argument("entrypoints_file", type=click.File("r"))
@click.option(
    "--call-graph-kind",
    type=click.Choice(InputType.members(), case_sensitive=False),
    default="PYRE",
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

    input_format_type = InputType[call_graph_kind.upper()].value
    input_format = input_format_type(call_graph_data)

    entrypoints = Entrypoints(entrypoints_json, input_format.get_keys())

    dependency_graph = DependencyGraph(input_format, entrypoints)

    validate_json_list(issues, "ISSUES_FILE", "top level")
    found_paths = dependency_graph.find_traces_for_callees(cast(List[str], issues))

    print(json.dumps(found_paths))


if __name__ == "__main__":
    analyze()
