# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module specifies how to configure the Python version in Pyre.
"""


import dataclasses

from . import exceptions


@dataclasses.dataclass(frozen=True)
class PythonVersion:
    major: int
    minor: int = 0
    micro: int = 0

    @staticmethod
    def from_string(input: str) -> "PythonVersion":
        try:
            splits = input.split(".")
            if len(splits) == 1:
                return PythonVersion(major=int(splits[0]))
            elif len(splits) == 2:
                return PythonVersion(major=int(splits[0]), minor=int(splits[1]))
            elif len(splits) == 3:
                return PythonVersion(
                    major=int(splits[0]), minor=int(splits[1]), micro=int(splits[2])
                )
            raise exceptions.InvalidPythonVersion(
                "Version string is expected to have the form of 'X.Y.Z' but got "
                + f"'{input}'"
            )
        except ValueError as error:
            raise exceptions.InvalidPythonVersion(str(error))

    def to_string(self) -> str:
        return f"{self.major}.{self.minor}.{self.micro}"
