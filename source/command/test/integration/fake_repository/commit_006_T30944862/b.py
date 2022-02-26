#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from a import Derived


# This class inherits incorrectly from its parent.
class AnotherDerived(Derived):
    def foo(self, x: int) -> None:
        pass
