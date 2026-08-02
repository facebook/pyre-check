from typing import Any, Generic, Tuple, TypeVar, overload

from pyre_extensions import Add, ListVariadic, Product
from pyre_extensions.type_variable_operators import Concatenate as Cat
from typing_extensions import Literal

A = TypeVar("A", bound=int)
B = TypeVar("B", bound=int)
C = TypeVar("C", bound=int)
D = TypeVar("D", bound=int)

Shape = ListVariadic("Shape")
Ts = ListVariadic("Ts")

class Tensor(Generic[Shape]):
    def __init__(self, *shape: Shape): ...
    def flatten(
        self: "Tensor[Cat[A,Ts]]", start: Literal[1]
    ) -> Tensor[A, Product[Ts]]: ...
    def __add__(self: "Tensor[Shape]", other: Tensor[Shape]) -> Tensor[Shape]: ...
