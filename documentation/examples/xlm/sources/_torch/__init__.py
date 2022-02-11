#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import torch
from pyre_extensions import Generic, TypeVarTuple


Shape = TypeVarTuple("Shape")


class Tensor(Generic[Shape], torch.Tensor):
    pass
