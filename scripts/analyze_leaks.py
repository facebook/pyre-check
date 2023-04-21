# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import keyword
import os
from dataclasses import dataclass
from pathlib import Path
from typing import cast, Dict, List, TextIO, Tuple

import click

from ..client import daemon_socket, find_directories, identifiers
from ..client.commands import daemon_query
from ..client.language_server import connections

from .callgraph_utilities import (
    CallGraph,
    DependencyGraph,
    Entrypoints,
    get_union_callgraph_format,
    InputType,
    JSON,
    load_json_from_file,
    Trace,
)


DEFAULT_WORKING_DIRECTORY: str = os.getcwd()


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


def is_valid_callee(callee: str) -> bool:
    components = callee.strip().split(".")
    is_valid_callee = all(
        component.isidentifier() and not keyword.iskeyword(component)
        for component in components
    )
    return is_valid_callee


def prepare_issues_for_query(callees: List[str]) -> str:
    return "global_leaks(" + ", ".join(filter(is_valid_callee, callees)) + ")"


def collect_pyre_query_results(pyre_results: object) -> LeakAnalysisResult:
    script_errors: List[LeakAnalysisScriptError] = []
    if not isinstance(pyre_results, dict):
        raise RuntimeError(
            f"Expected dict for Pyre query results, got {type(pyre_results)}: {pyre_results}"
        )
    response = pyre_results.get("response")
    if not response:
        raise RuntimeError("`response` key not in Pyre query results", pyre_results)
    if not isinstance(pyre_results["response"], dict):
        raise RuntimeError(
            f"Expected response value type to be list, got {type(response)}: {response}"
        )

    global_leaks = response.get("global_leaks")
    if global_leaks is None:
        script_errors.append(
            LeakAnalysisScriptError(
                error_message="Expected `global_leaks` key to be present in response",
                bad_value=response,
            )
        )
        global_leaks = []
    elif not isinstance(global_leaks, list):
        script_errors.append(
            LeakAnalysisScriptError(
                error_message="Expected `global_leaks` to be a list of error JSON objects",
                bad_value=global_leaks,
            )
        )
        global_leaks = []

    query_errors = response.get("query_errors")
    if query_errors is None:
        script_errors.append(
            LeakAnalysisScriptError(
                error_message="Expected `query_errors` key to be present in response",
                bad_value=response,
            )
        )
        query_errors = []
    elif not isinstance(query_errors, list):
        script_errors.append(
            LeakAnalysisScriptError(
                error_message="Expected `query_errors` to be a list of error JSON objects",
                bad_value=query_errors,
            )
        )
        query_errors = []

    return LeakAnalysisResult(
        global_leaks=global_leaks,
        query_errors=query_errors,
        script_errors=script_errors,
    )


def find_issues(callees: List[str], search_start_path: Path) -> LeakAnalysisResult:
    query_str = prepare_issues_for_query(callees)
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
        collected_results = collect_pyre_query_results(response.payload)
        return collected_results
    except connections.ConnectionFailure as e:
        raise RuntimeError(
            "A running Pyre server is required for queries to be responded. "
            "Please run `pyre` first to set up a server."
        ) from e


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


@click.group()
def analyze() -> None:
    """
    Performs analyses over Pyre's results using a call graph and list of entrypoints.
    """
    pass


@analyze.command()
@click.argument("callables_file", type=click.File("r"))
@click.option(
    "--project-path",
    type=str,
    default=DEFAULT_WORKING_DIRECTORY,
    help="The path to the project in which global leaks will be searched for. \
    The given directory or parent directory must have a global .pyre_configuration. \
    Default: current directory.",
)
def callable_leaks(
    callables_file: TextIO,
    project_path: str,
) -> None:
    """
    Run local global leak analysis per callable given in the callables_file.

    The output of this script will be a JSON object containing three keys:
    - `global_leaks`: any global leaks that are returned from `pyre query "global_leaks(...)"` for
        callable checked.
    - `query_errors`: any errors that occurred during pyre's analysis, for example, no qualifier found
    - `script_errors`: any errors that occurred during the analysis, for example, a definition not
        found for a callable

    CALLABLES_FILE: a file containing a JSON list of fully qualified paths of callables

    Example usage: ./analyze_leaks.py -- callable-leaks <CALLABLES_FILE>
    """
    callables = load_json_from_file(callables_file, "CALLABLES_FILE")
    validate_json_list(callables, "CALLABLES_FILE", "top level")
    issues = find_issues(cast(List[str], callables), Path(project_path))
    print(issues.to_json())


@analyze.command()
@click.option(
    "--call-graph-kind-and-path",
    type=(click.Choice(InputType.members(), case_sensitive=False), click.File("r")),
    multiple=True,
    required=True,
)
@click.argument("entrypoints_file", type=click.File("r"))
@click.option(
    "--project-path",
    type=str,
    default=DEFAULT_WORKING_DIRECTORY,
    help="The path to the project in which global leaks will be searched for. \
    The given directory or parent directory must have a global .pyre_configuration. \
    Default: current directory.",
)
def entrypoint_leaks(
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

    Example usage: ./analyze_leaks.py -- entrypoint-leaks <ENTRYPOINTS_FILE> --call-graph-kind-and-path <KIND1> <CALL_GRAPH_1> --call-graph-kind-and-path <KIND2> <CALL_GRAPH2>
    """
    entrypoints_json = load_json_from_file(entrypoints_file, "ENTRYPOINTS_FILE")
    validate_json_list(entrypoints_json, "ENTRYPOINTS_FILE", "top-level")

    input_format = get_union_callgraph_format(call_graph_kind_and_path)

    entrypoints = Entrypoints(entrypoints_json, input_format.get_keys())

    call_graph = CallGraph(input_format, entrypoints)

    all_callables = call_graph.get_transitive_callees_and_traces()
    issues = find_issues(list(all_callables.keys()), Path(project_path))
    attach_trace_to_query_results(issues, all_callables)
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
    validate_json_list(entrypoints_json, "ENTRYPOINTS_FILE", "top-level")

    input_format_type = InputType[call_graph_kind.upper()].value
    input_format = input_format_type(call_graph_data)

    entrypoints = Entrypoints(entrypoints_json, input_format.get_keys())

    dependency_graph = DependencyGraph(input_format, entrypoints)

    validate_json_list(issues, "ISSUES_FILE", "top level")
    found_paths = dependency_graph.find_traces_for_callees(cast(List[str], issues))

    print(json.dumps(found_paths))


if __name__ == "__main__":
    analyze()
