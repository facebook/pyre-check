# This a list of known errors, in some cases that people are working on.

from typing import Callable, Generic, ParamSpec, TypeAlias, TypeVar, Unpack

# Steven: Generic, Protocol
_T = TypeVar("_T")


class BaseClass(Generic[_T]):
    pass


# Sam: Callable, ParamSpec, Unpack
_P = ParamSpec("_P")


def complex_callable(func: Callable[_P, bool], *args: Unpack[tuple[str, str]]):
    pass


# Rebecca: Generic Type Aliases
MyList: TypeAlias = list[_T]
x: MyList[int]

# Danny: Operators
operators = []
operators += [True, 2 > 3]
