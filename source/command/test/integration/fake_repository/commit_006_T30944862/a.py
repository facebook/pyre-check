#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
class Base:
    def foo(self, x: int) -> None:
        pass


# This class inherits incorrectly from its parent.
class Derived(Base):
    def foo(self, x: str) -> None:
        pass


class Standalone:
    pass
