# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Generic, TypeVar

from torch.nn import Linear


A = TypeVar("A", bound=int)
B = TypeVar("B", bound=int)


class Linear(Generic[A, B], Linear):
    pass
