# pyre-unsafe
from typing import Any, Generic, TypeVar

from . import GenericMeta


_T1 = TypeVar("_T1")
_T2 = TypeVar("_T2")


class ParameterSpecificationComponentMeta(type):
    def __getitem__(cls, __tparams) -> object:
        return Any


class Map(Generic[_T1, _T2]):
    pass


class Concatenate(metaclass=GenericMeta):
    pass


class PositionalArgumentsOf(metaclass=ParameterSpecificationComponentMeta):
    pass


class KeywordArgumentsOf(metaclass=ParameterSpecificationComponentMeta):
    pass
