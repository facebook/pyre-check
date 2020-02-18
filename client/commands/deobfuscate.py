# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
from typing import List

from . import Check


class Deobfuscate(Check):
    NAME = "deobfuscate"
    # TODO(T62143503): Re-enable deobfuscate via command-line checks.
    HIDDEN = True

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        deobfuscate = parser.add_parser(cls.NAME)
        deobfuscate.set_defaults(command=cls)

    def _flags(self) -> List[str]:
        flags = super()._flags()
        flags.extend(["-additional-check", "deobfuscation"])
        return flags
