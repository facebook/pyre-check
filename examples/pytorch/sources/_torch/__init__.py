#!/usr/bin/env python3
# pyre-strict

from typing import TypeVar

import torch
from pyre_extensions import Generic, ListVariadic
from pyre_extensions.type_variable_operators import Concatenate
from torch import cat, mm, randn, unsqueeze  # noqa

from .nn import Linear  # noqa


# New Variadic Type Variadic, a place-holder for an unknown number of
# variables
Shape = ListVariadic("Shape")

# ad-hoc dtypes for testing purpose
DType = TypeVar("DType", int, float)
int32 = int
float32 = float


# Minimal Tensor stub
class Tensor(Generic[DType, Shape], torch.Tensor):
    pass
