#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from _torch import float32, int32, mm, Tensor
from typing_extensions import Literal


# Lets create some actual dimensions
D1 = Literal[42]
D2 = Literal[16]
D3 = Literal[64]


# and a dummy one without a real value (could be an unknown Batch Size)
class T:
    pass


T1: Tensor[int32, [D1, D2]] = Tensor()
T2: Tensor[int32, [D2, D3]] = Tensor()
T3: Tensor[int32, [T, T]] = Tensor()
T4: Tensor[float32, [D1, D2]] = Tensor()

# T1 + T1 is correctly typed
T1p1: Tensor[int32, [D1, D2]] = T1 + T1

# T1 * T2 is correctly typed
T1m2: Tensor[int32, [D1, D3]] = mm(T1, T2)


def incorrects() -> None:
    # T1 + T2 is incorrectly typed, due to different dimensions in the tensors
    # pyre-fixme[58]: `+` is not supported for operand types `Tensor[int, [int,
    # int]]` and `Tensor[int, [int, int]]`.
    Err1 = T1 + T2  # noqa
    # T1 * T3 is incorrectly typed
    # pyre-fixme[6]: Expected `Tensor[DType, B, C]` for 2nd param but got
    #  `Tensor[int, T, T]`.
    Err2 = mm(T1, T3)  # noqa
    # T1 + T4 is incorrectly type (dtype)
    # pyre-fixme[58]: `+` is not supported for operand types `Tensor[int, [int,
    # int]]` and `Tensor[float, [int, int]]`.
    Err3 = T1 + T3  # noqa


# Variadics allow to use tensor of multiple shape with the same function
# Here is a usage of add with a Tensor of different shape, still works !
Tx: Tensor[int32, [D1, D1, D1, D1, D2]] = Tensor()
Txpx: Tensor[int32, [D1, D1, D1, D1, D2]] = Tx + Tx
