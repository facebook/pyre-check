# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import Generic, List, Tuple, TypeVar

from _torch import DType, Tensor
from typing_extensions import Literal

D1 = TypeVar("D1")
D2 = TypeVar("D2")
N = TypeVar("N")

class Linear(Generic[DType, D1, D2]):
    def __init__(self, d1: int, d2: int) -> None: ...
    # reaaaaallly not sure about that one :D
    def __call__(self, x: Tensor[DType, [N, D1]]) -> Tensor[DType, [N, D2]]: ...
    def zero_grad(self) -> None: ...
    # not sure if 1 or D2
    bias: Tensor[DType, [Literal[1]]]

    weight: Tensor[DType, [D2, D1]]

    # is it D2, D1 or just 1 , D1 ?
    # is it D2 or just 1 ?
    def parameters(self) -> Tuple[Tensor[DType, [D2, D1]], Tensor[DType, [D2]]]: ...
