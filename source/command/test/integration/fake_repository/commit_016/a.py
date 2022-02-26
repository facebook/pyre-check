# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Test: Add pyre-strict (change mode)
#!/usr/bin/env python3

from typing import Any


def foo(x: Any) -> str:
    return x
