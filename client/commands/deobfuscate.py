# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import List

from . import Check


class Deobfuscate(Check):
    NAME = "deobfuscate"  # type: str

    def _flags(self) -> List[str]:
        flags = super()._flags()
        flags.extend(["-additional-check", "deobfuscation"])
        return flags
