# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from _torch import Tensor
from pyre_extensions import TypeVarTuple

Shape = TypeVarTuple("Shape")

def softmax(t: Tensor[Shape], dim: int) -> Tensor[Shape]: ...
def dropout(t: Tensor[Shape], p, training) -> Tensor[Shape]: ...
