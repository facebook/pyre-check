# pyre-strict

from typing import TypeVar

from _torch import DType, Tensor
from typing_extensions import Literal

N = TypeVar("N")

def smooth_l1_loss(
    refy: Tensor[DType, N, Literal[1]], y: Tensor[DType, N, Literal[1]]
) -> Tensor[DType, Literal[1]]: ...
