from functools import lru_cache
from typing import Any, Dict, Iterable, List, NamedTuple, Optional

from .connection_api import PyreConnection


class DefineParameter(NamedTuple):
    name: str
    annotation: str


class Define(NamedTuple):
    name: str
    parameters: List[DefineParameter]
    return_annotation: str


class Position(NamedTuple):
    line: int
    column: int


class Location(NamedTuple):
    path: str
    start: Position
    stop: Position


class CallGraphTarget(NamedTuple):
    target: str
    # We might want to turn this into an enum in the future.
    kind: str
    locations: List[Location]


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

    def subclasses(self, class_name: str) -> Optional[List[str]]:
        return self.reverse_hierarchy.get(class_name)

    def superclasses(self, class_name: str) -> Optional[List[str]]:
        return self.hierarchy.get(class_name)


def defines(pyre_connection: PyreConnection, modules: Iterable[str]) -> List[Define]:
    query = "defines({})".format(",".join(modules))
    result = pyre_connection.query_server(query)
    if result is None or "response" not in result:
        return []
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


def get_class_hierarchy(pyre_connection: PyreConnection) -> Optional[ClassHierarchy]:
    result = pyre_connection.query_server("dump_class_hierarchy()")
    if result is None or "response" not in result:
        return None
    hierarchy = {
        key: edges
        for annotation_and_edges in result["response"]
        for key, edges in annotation_and_edges.items()
    }
    return ClassHierarchy(hierarchy)


def get_superclasses(pyre_connection: PyreConnection, class_name: str) -> List[str]:
    query = f"superclasses({class_name})"
    result = pyre_connection.query_server(query)
    if result is None or "response" not in result:
        return []
    return result["response"]["superclasses"]


def get_attributes(pyre_connection: PyreConnection, class_name: str) -> List[str]:
    query = f"attributes({class_name})"
    result = pyre_connection.query_server(query)
    if result is None or "response" not in result:
        return []
    return [attribute["name"] for attribute in result["response"]["attributes"]]


def get_call_graph(
    pyre_connection: PyreConnection
) -> Optional[Dict[str, List[CallGraphTarget]]]:
    result = pyre_connection.query_server("dump_call_graph()")
    if result is None or "response" not in result:
        return None
    call_graph = {}
    for function, calls in result["response"].items():
        call_graph[function] = [
            CallGraphTarget(
                target=call["target"],
                kind=call["kind"],
                locations=[_parse_location(location) for location in call["locations"]],
            )
            for call in calls
        ]
    return call_graph


def _parse_location(location_json: Dict[str, Any]) -> Location:
    return Location(
        path=location_json["path"],
        start=_parse_position(location_json["start"]),
        stop=_parse_position(location_json["stop"]),
    )


def _parse_position(position_json: Dict[str, Any]) -> Position:
    return Position(line=position_json["line"], column=position_json["column"])
