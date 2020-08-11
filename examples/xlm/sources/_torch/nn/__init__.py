from typing import Generic, TypeVar

from torch.nn import Linear


A = TypeVar("A", bound=int)
B = TypeVar("B", bound=int)


class Linear(Generic[A, B], Linear):
    pass
