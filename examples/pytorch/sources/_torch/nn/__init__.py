from typing import Generic, TypeVar

from torch.nn import Linear


D1 = TypeVar("D1")
D2 = TypeVar("D2")
N = TypeVar("N")


class Linear(Generic[N, D1, D2], Linear):
    pass
