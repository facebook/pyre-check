# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import enum
from typing import Optional


class SearchStrategy(str, enum.Enum):
    NONE: str = "none"
    ALL: str = "all"
    PEP561: str = "pep561"

    def __str__(self) -> str:
        return self.value

    @staticmethod
    def from_string(input: str) -> "Optional[SearchStrategy]":
        try:
            return next(item for item in SearchStrategy if item.value == input)
        except StopIteration:
            return None
