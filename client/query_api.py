from collections import ChainMap
from typing import Dict, Iterable, List, NamedTuple, Optional

from .connection_api import PyreConnection


class DefineParameter(NamedTuple):
    name: str
    annotation: str


class Define(NamedTuple):
    name: str
    parameters: List[DefineParameter]
    return_annotation: str


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


def get_class_hierarchy(
    pyre_connection: PyreConnection
) -> Optional[Dict[str, List[str]]]:
    result = pyre_connection.query_server("dump_class_hierarchy()")
    if result is None or "response" not in result:
        return None
    return dict(ChainMap(*result["response"]))
