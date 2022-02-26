from pathlib import Path
import re
from . import query as query_v2
from typing import (
    Dict,
    Iterable,
    List,
    NamedTuple,
)


class Position(NamedTuple):
    line: int
    column: int

    def __eq__(self, other) -> bool:
        return self.line == other.line and other.column == self.column

    def __gt__(self, other) -> bool:
        return self.line > other.line or (
            self.line == other.line and self.column > other.column
        )

    def __lt__(self, other) -> bool:
        return self.line < other.line or (
            self.line == other.line and self.column < other.column
        )


class Location(NamedTuple):
    path: str
    start: Position
    stop: Position


class Type(NamedTuple):
    location: Dict[str, Position]
    annotation: str

    def extract_function_model(self) -> str:
        functions = re.findall(r"(?<=typing.Callable\().*?(?=\))", self.annotation)
        params = re.findall(r"(?<=Named\().*?(?=,)", self.annotation)
        # if selected position is not a function
        if not functions:
            raise NotImplementedError("Selected position is not of type Callable")
        model = "def {}({}): ...".format(functions[0], ", ".join(params))
        return model


class Types(NamedTuple):
    path: str
    types: List[Type]


def types(socket_path: Path, module_paths: Iterable[str]) -> List[Types]:

    query = "types('{}')".format(",".join(module_paths))
    response_json = query_v2.query_server(socket_path, query).payload

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
        for module_result in response_json["response"]
    ]
