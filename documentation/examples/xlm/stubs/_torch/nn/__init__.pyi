# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Generic, TypeVar

from _torch import Tensor
from pyre_extensions import TypeVarTuple
from pyre_extensions.type_variable_operators import Concatenate
from typing_extensions import Literal

A = TypeVar("A", bound=int)
B = TypeVar("B", bound=int)
N = TypeVar("N", bound=int)
Shape = TypeVarTuple("Shape")

class Linear(Generic[A, B]):
    def __init__(self, a: A, b: B): ...
    def __call__(
        self, a: Tensor[Concatenate[N, Shape, A]]
    ) -> Tensor[Concatenate[N, Shape, B]]: ...
