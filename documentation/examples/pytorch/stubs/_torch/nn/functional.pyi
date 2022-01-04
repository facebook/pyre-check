# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import TypeVar

from _torch import DType, Tensor
from typing_extensions import Literal

N = TypeVar("N")

def smooth_l1_loss(
    refy: Tensor[DType, [N, Literal[1]]], y: Tensor[DType, [N, Literal[1]]]
) -> Tensor[DType, [Literal[1]]]: ...
