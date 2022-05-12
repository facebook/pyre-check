#!/usr/bin/env python
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from __future__ import print_function

from itertools import count
from typing import Sequence, Tuple, TypeVar

import _torch as torch
import _torch.nn.functional as F
from _torch import float32, Tensor
from typing_extensions import Literal


DType = TypeVar("DType", int, float)

POLY_DEGREE: int = 4
D1 = Literal[1]
D4 = Literal[4]
D32 = Literal[32]

W_target: Tensor[float32, [D4, D1]] = torch.randn(POLY_DEGREE, 1) * 5
b_target: Tensor[float32, [D1]] = torch.randn(1) * 5

N = TypeVar("N")


def make_features(x: Tensor[DType, [N]]) -> Tensor[DType, [N, D4]]:
    """Builds features i.e. a matrix with columns [x, x^2, x^3, x^4]."""
    # x = x.unsqueeze(1)
    x2 = torch.unsqueeze(x, 1)
    # return torch.cat([x ** i for i in range(1, POLY_DEGREE+1)], 1)
    r: Tensor[DType, [N, D4]] = torch.cat(
        [x2**i for i in range(1, POLY_DEGREE + 1)], 1
    )
    return r


def f(x: Tensor[float32, [N, D4]]) -> Tensor[float32, [N, D1]]:
    """Approximated function."""
    return torch.mm(x, W_target) + b_target.item()


T = TypeVar("T")


def poly_desc(W: Sequence[T], b: Tensor[DType, [D1]]) -> str:
    """Creates a string description of a polynomial."""
    result = "y = "
    for i, w in enumerate(W):
        result += "{:+.2f} x^{} ".format(w, len(W) - i)
    result += "{:+.2f}".format(b[0])
    return result


# Need IntVars for the original function.
# I'll hardcode this batch size for this demo
# def get_batch(batch_size=32):
def get_batch() -> Tuple[Tensor[float32, [D32, D4]], Tensor[float32, [D32, D1]]]:
    """Builds a batch i.e. (x, f(x)) pair."""
    batch_size = 32
    random: Tensor[float32, [D32]] = torch.randn(batch_size)
    x = make_features(random)
    y = f(x)
    return x, y


# Define model
fc: torch.nn.Linear[float32, D4, D1] = torch.nn.Linear(W_target.size(0), 1)

final_index: int = 0
loss: float32

for batch_idx in count(1):
    # Get data
    batch_x, batch_y = get_batch()

    # Reset gradients
    fc.zero_grad()

    # Forward pass
    output = F.smooth_l1_loss(fc(batch_x), batch_y)
    loss = output.item()

    # Backward pass
    output.backward()

    # The next loop is problematic:
    # returns a generator of tensors: param[0] is Tensor[1, 4]
    # param[1] is Tensor[1]
    # so the `param` is a Union which messes up with `add_`

    # Apply gradients
    param1, param2 = fc.parameters()
    param1.data.add_(-0.1 * param1.grad.data)
    param2.data.add_(-0.1 * param2.grad.data)

    # Stop criterion
    if loss < 1e-3:
        final_index = batch_idx
        break

print("Loss: {:.6f} after {} batches".format(loss, final_index))
print("==> Learned function:\t" + poly_desc(fc.weight.view(-1), fc.bias))
print("==> Actual function:\t" + poly_desc(W_target.view(-1), b_target))
