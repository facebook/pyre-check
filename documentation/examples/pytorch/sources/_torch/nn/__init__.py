# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Generic, TypeVar

from torch.nn import Linear


D1 = TypeVar("D1")
D2 = TypeVar("D2")
N = TypeVar("N")


class Linear(Generic[N, D1, D2], Linear):
    pass
