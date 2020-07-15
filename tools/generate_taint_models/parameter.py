from enum import Enum, auto
from typing import NamedTuple, Optional


class Parameter(NamedTuple):
    class Kind(Enum):
        ARG = auto()
        VARARG = auto()
        KWARG = auto()

    name: str
    annotation: Optional[str]
    kind: Kind

    def __eq__(self, other: "Parameter") -> bool:
        if not isinstance(other, self.__class__):
            return False
        return self.name == other.name
