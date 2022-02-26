#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import TypeVar

import torch
from pyre_extensions import Generic, TypeVarTuple
from torch import cat, mm, randn, unsqueeze  # noqa

from .nn import Linear  # noqa


# New Variadic Type Variadic, a place-holder for an unknown number of
# variables
Shape = TypeVarTuple("Shape")

# ad-hoc dtypes for testing purpose
DType = TypeVar("DType", int, float)
int32 = int
float32 = float


# Minimal Tensor stub
class Tensor(Generic[DType, Shape], torch.Tensor):
    pass
