#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


class Base:
    def foo(self, x: int) -> None:
        pass


class Derived:
    def foo(self, x: str) -> None:
        pass
