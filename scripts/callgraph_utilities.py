# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import abc
import enum
import json
import sys
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
    Tuple,
    Union,
)

from typing_extensions import TypeAlias


Trace: TypeAlias = List[str]
JSON = Union[Dict[str, "JSON"], List["JSON"], str, int, float, bool, None]

def load_json_from_file(file_handle: TextIO, file_name: str) -> JSON:
    try:
        return json.load(file_handle)
    except json.JSONDecodeError as e:
        raise ValueError(f"Error loading {file_name} as JSON") from e

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

def get_union_callgraph_format(call_graph_kind_and_path: Tuple[Tuple[str, TextIO], ...]) -> UnionCallGraphFormat:
    union_call_graph_format = UnionCallGraphFormat()
    for call_graph_kind, call_graph_file in call_graph_kind_and_path:
        call_graph_data = load_json_from_file(call_graph_file, "CALL_GRAPH_FILE")

        current_input_format_type = InputType[call_graph_kind.upper()].value
        current_input_format = current_input_format_type(call_graph_data)
        union_call_graph_format.union_call_graph(current_input_format.call_graph)
    return union_call_graph_format
