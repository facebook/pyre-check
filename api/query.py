# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
from dataclasses import dataclass
from functools import lru_cache
from itertools import islice
from typing import Any, Dict, Generator, Iterable, List, NamedTuple, Optional, TypeVar

from .connection import PyreConnection, PyreQueryResult


LOG: logging.Logger = logging.getLogger(__name__)


T = TypeVar("T")


class Attributes(NamedTuple):
    name: str
    annotation: Optional[str]
    kind: str
    final: bool


class DefineParameter(NamedTuple):
    name: str
    annotation: str


class Define(NamedTuple):
    name: str
    parameters: List[DefineParameter]
    return_annotation: str

    def get_class_name(self) -> str:
        return ".".join(self.name.split(".")[:-1])

    def get_method_name(self) -> str:
        return self.name.split(".")[-1]


class Position(NamedTuple):
    line: int
    column: int


class Location(NamedTuple):
    path: str
    start: Position
    stop: Position


@dataclass(frozen=True)
class Annotation:
    type_name: str
    start: Position
    stop: Position


class CallGraphTarget:
    def __init__(self, call: Dict[str, Any]) -> None:
        self.target: str = ""
        if "target" in call:
            self.target = call["target"]
        else:
            self.target = call["direct_target"]
        self.kind: str = call["kind"]
        self.locations: List[Location] = [
            _parse_location(location) for location in call["locations"]
        ]

    def __eq__(self, other: "CallGraphTarget") -> bool:
        return (
            self.target == other.target
            and self.kind == other.kind
            and self.locations == other.locations
        )


class ClassHierarchy:
    def __init__(self, hierarchy: Dict[str, List[str]]) -> None:
        self.hierarchy = hierarchy

    # Poor man's cached property.
    @property
    @lru_cache(maxsize=1)
    def reverse_hierarchy(self) -> Dict[str, List[str]]:
        reversed_mapping = {}
        # In order to distinguish between missing types and types
        # with no subclasses, we initialize everything to [] for known keys.
        for key in self.hierarchy:
            reversed_mapping[key] = []
        for key, values in self.hierarchy.items():
            for value in values:
                reversed_mapping[value].append(key)
        return reversed_mapping

    def subclasses(self, class_name: str) -> List[str]:
        return self.reverse_hierarchy.get(class_name, [])

    def superclasses(self, class_name: str) -> List[str]:
        return self.hierarchy.get(class_name, [])


@dataclass
class PyreCache:
    class_hierarchy: Optional[ClassHierarchy] = None


class InvalidModel(NamedTuple):
    fully_qualified_name: str
    path: Optional[str]
    line: int
    column: int
    stop_line: int
    stop_column: int
    full_error_message: str


def _defines(pyre_connection: PyreConnection, modules: Iterable[str]) -> List[Define]:
    query = "defines({})".format(",".join(modules))
    result = pyre_connection.query_server(query)
    return [
        Define(
            name=element["name"],
            parameters=[
                DefineParameter(
                    name=parameter["name"], annotation=parameter["annotation"]
                )
                for parameter in element["parameters"]
            ],
            return_annotation=element["return_annotation"],
        )
        for element in result["response"]
    ]


def defines(
    pyre_connection: PyreConnection,
    modules: Iterable[str],
    batch_size: Optional[int] = None,
) -> List[Define]:
    modules = list(modules)
    if batch_size is None:
        return _defines(pyre_connection, modules)
    if batch_size <= 0:
        raise ValueError(
            "batch_size must a positive integer, provided: `{}`".format(batch_size)
        )
    found_defines: List[Define] = []
    module_chunks = [
        modules[index : index + batch_size]
        for index in range(0, len(modules), batch_size)
    ]
    for modules in module_chunks:
        found_defines.extend(_defines(pyre_connection, modules))
    return found_defines


def get_class_hierarchy(pyre_connection: PyreConnection) -> ClassHierarchy:
    result = pyre_connection.query_server("dump_class_hierarchy()")

    return ClassHierarchy(
        {
            key: edges
            for annotation_and_edges in result["response"]
            for key, edges in annotation_and_edges.items()
        }
    )


def get_cached_class_hierarchy(
    pyre_connection: PyreConnection, pyre_cache: Optional[PyreCache]
) -> ClassHierarchy:
    cached_class_hierarchy = (
        pyre_cache.class_hierarchy if pyre_cache is not None else None
    )
    if cached_class_hierarchy is not None:
        return cached_class_hierarchy

    class_hierarchy = get_class_hierarchy(pyre_connection)

    if pyre_cache is not None:
        pyre_cache.class_hierarchy = class_hierarchy

    return class_hierarchy


def _annotations_per_file(data: PyreQueryResult) -> Dict[str, List[Annotation]]:
    def make_position(mapping: Dict[str, int]) -> Position:
        return Position(column=mapping["column"], line=mapping["line"])

    return {
        response["response"][0]["path"]: [
            Annotation(
                locations_and_annotations["annotation"],
                make_position(locations_and_annotations["location"]["start"]),
                make_position(locations_and_annotations["location"]["stop"]),
            )
            for locations_and_annotations in response["response"][0]["types"]
        ]
        for response in data["response"]
        if "response" in response
    }


def get_types(
    pyre_connection: PyreConnection, *paths: str
) -> Dict[str, List[Annotation]]:
    types_sequence = ",".join([f"types('{path}')" for path in paths])
    result = pyre_connection.query_server(f"batch({types_sequence})")

    return _annotations_per_file(result)


def get_superclasses(pyre_connection: PyreConnection, class_name: str) -> List[str]:
    query = f"superclasses({class_name})"
    result = pyre_connection.query_server(query)
    return result["response"][0][class_name]


def _get_batch(
    iterable: Iterable[T], batch_size: Optional[int]
) -> Generator[Iterable[T], None, None]:
    if not batch_size:
        yield iterable
    elif batch_size <= 0:
        raise ValueError(
            "batch_size must a positive integer, provided: `{}`".format(batch_size)
        )
    else:
        iterator = iter(iterable)
        batch = list(islice(iterator, batch_size))
        while batch:
            yield batch
            batch = list(islice(iterator, batch_size))


def _get_attributes(
    pyre_connection: PyreConnection, class_name: str
) -> List[Attributes]:
    query = f"attributes({class_name})"
    response = pyre_connection.query_server(query)["response"]
    return [
        Attributes(
            name=attribute["name"],
            annotation=attribute["annotation"],
            kind=attribute["kind"],
            final=attribute["final"],
        )
        for attribute in response["attributes"]
    ]


def get_attributes(
    pyre_connection: PyreConnection,
    class_names: Iterable[str],
    batch_size: Optional[int] = None,
) -> Dict[str, List[Attributes]]:
    all_responses = {}
    for batch in _get_batch(class_names, batch_size):
        query = "batch({})".format(", ".join([f"attributes({name})" for name in batch]))
        responses = pyre_connection.query_server(query)["response"]
        for class_name, response in zip(batch, responses):
            if "response" in response:
                all_responses[class_name] = [
                    Attributes(
                        name=attribute["name"],
                        annotation=attribute["annotation"],
                        kind=attribute["kind"],
                        final=attribute["final"],
                    )
                    for attribute in response["response"]["attributes"]
                ]
            else:
                LOG.warn(
                    f"Error resolving query for `{class_name=}` in get_attributes `{response=}`"
                )
                all_responses[class_name] = []
    return all_responses


def get_call_graph(
    pyre_connection: PyreConnection,
) -> Optional[Dict[str, List[CallGraphTarget]]]:
    response = pyre_connection.query_server("dump_call_graph()")["response"]
    call_graph = {}

    for function, calls in response.items():
        call_graph[function] = [CallGraphTarget(call) for call in calls]
    return call_graph


def _parse_location(location_json: Dict[str, Any]) -> Location:
    return Location(
        path=location_json["path"],
        start=_parse_position(location_json["start"]),
        stop=_parse_position(location_json["stop"]),
    )


def _parse_position(position_json: Dict[str, Any]) -> Position:
    return Position(line=position_json["line"], column=position_json["column"])


def get_invalid_taint_models(
    pyre_connection: PyreConnection,
) -> List[InvalidModel]:
    errors: List[InvalidModel] = []
    response = pyre_connection.query_server("validate_taint_models()")
    if "response" in response and "errors" in response["response"]:
        found_errors = response["response"]["errors"]
        for error in found_errors:
            errors.append(
                InvalidModel(
                    full_error_message=error["description"],
                    path=error["path"],
                    line=error["line"],
                    column=error["column"],
                    stop_line=error["stop_line"],
                    stop_column=error["stop_column"],
                    fully_qualified_name="",
                )
            )
    return errors
