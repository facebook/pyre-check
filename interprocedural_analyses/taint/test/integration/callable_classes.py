# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Protocol


class P:
    def __call__(self, arg: str) -> str:
        ...


def returns_p() -> P:
    ...


p: P = returns_p()


def foo() -> str:
    return p("a")
