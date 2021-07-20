# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import re
from functools import lru_cache
from itertools import islice
from typing import Any, Dict, Generator, Iterable, List, NamedTuple, Optional, TypeVar

from .connection import LOG, PyreConnection, PyreQueryError


T = TypeVar("T")


class Attributes(NamedTuple):
    name: str
    annotation: Optional[str]


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


class Type(NamedTuple):
    location: Dict[str, Position]
    annotation: str

    def extract_function_model(self) -> str:
        functions = re.findall(
            "(?<=typing.Callable\().*?(?=\))", self.annotation)
        params = re.findall("(?<=Named\().*?(?=,)", self.annotation)
        # if selected position is not a function
        if not functions:
            raise NotImplementedError(
                "Selected position is not of type Callable")
        model = "def {}({}):".format(functions[0], ", ".join(params))
        return model


class Types(NamedTuple):
    path: str
    types: List[Type]


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

    hierarchy = {
        key: edges
        for annotation_and_edges in result["response"]
        for key, edges in annotation_and_edges.items()
    }
    return ClassHierarchy(hierarchy)


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
        Attributes(name=attribute["name"], annotation=attribute["annotation"])
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
        all_responses.update(
            {
                class_name: [
                    Attributes(
                        name=attribute["name"], annotation=attribute["annotation"]
                    )
                    for attribute in response["response"]["attributes"]
                ]
                for class_name, response in zip(batch, responses)
            }
        )
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


def types(pyre_connection: PyreConnection, paths: Iterable[str]) -> List[Types]:
    query = "types('{}')".format(",".join(paths))
    result = pyre_connection.query_server(query)

    return [
        Types(
            path=module_result["path"],
            types=[
                Type(
                    location={
                        "start": Position(
                            line=element["location"]["start"]["line"],
                            column=element["location"]["start"]["column"],
                        ),
                        "stop": Position(
                            line=element["location"]["stop"]["line"],
                            column=element["location"]["stop"]["column"],
                        ),
                    },
                    annotation=element["annotation"],
                )
                for element in module_result["types"]
            ],
        )
        for module_result in result["response"]
    ]
