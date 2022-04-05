#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import Generic, List, overload, Sequence, Tuple, TypeVar, Union

from pyre_extensions import TypeVarTuple
from pyre_extensions.type_variable_operators import Concatenate
from typing_extensions import Literal

# New Variadic Type Variadic, a place-holder for an unknown number of
# variables
Shape = TypeVarTuple("Shape")

# ad-hoc dtypes for testing purpose
DType = TypeVar("DType", int, float)
int32 = int
float32 = float

# Minimal Tensor stub
class Tensor(Generic[DType, Shape]):
    data: "Tensor[DType, Shape]"
    grad: "Tensor[DType, Shape]"

    # inplace add
    def add_(self, other: "Tensor[DType, Shape]") -> None: ...
    # Add two tensors of the exact same shape
    @overload
    def __add__(self, other: "Tensor[DType, Shape]") -> "Tensor[DType, Shape]": ...
    @overload
    def __add__(self, other: DType) -> "Tensor[DType, Shape]": ...
    def __mul__(self, other: DType) -> "Tensor[DType, Shape]": ...
    def __rmul__(self, other: DType) -> "Tensor[DType, Shape]": ...
    def __pow__(self, other: int) -> "Tensor[DType, Shape]": ...
    def __neg__(self) -> "Tensor[DType, Shape]": ...
    def __getitem__(self, pos: int) -> DType: ...
    @overload
    def view(self, x: Literal[-1]) -> Sequence[DType]: ...
    def size(self, x: int) -> int: ...
    def backward(self) -> None: ...
    # This one should raise if the shape is not [] or [1]
    def item(self) -> DType: ...

A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")

# multiply two 2D tensors with matching dimensions
def mm(
    left: Tensor[DType, [A, B]], right: Tensor[DType, [B, C]]
) -> Tensor[DType, [A, C]]: ...
@overload
def unsqueeze(
    tensor: Tensor[DType, Shape], pos: Literal[0]
) -> Tensor[DType, Concatenate[Literal[1], Shape]]: ...
@overload
def unsqueeze(
    tensor: Tensor[DType, Concatenate[A, Shape]], pos: Literal[1]
) -> Tensor[DType, Concatenate[A, Literal[1], Shape]]: ...
def randn(*args: Shape) -> Tensor[float32, Shape]: ...

# pyre-ignore
def cat(l, n): ...
