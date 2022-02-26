#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Protocol


class MyProtocol(Protocol):
    def foo(self) -> int:
        ...


class Implements:
    def foo(self) -> int:
        return 1


def expects_my_protocol(x: MyProtocol) -> None:
    pass


def should_not_error(x: Implements) -> None:
    expects_my_protocol(x)
