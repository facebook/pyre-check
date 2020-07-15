from enum import Enum, auto
from typing import NamedTuple, Optional


class ArgumentKind(Enum):
    ARG = auto()
    VARARG = auto()
    KWARG = auto()


class Parameter(NamedTuple):
    name: str
    annotation: Optional[str]
    kind: ArgumentKind

    def __eq__(self, other: "Parameter") -> bool:
        if not isinstance(other, self.__class__):
            return False
        return self.name == other.name
