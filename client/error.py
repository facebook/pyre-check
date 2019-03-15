# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import sys
from typing import Any

from . import is_capable_terminal
from .log import Color, Format


class Error:
    ignore_error = False  # type: bool
    external_to_global_root = False  # type: bool

    def __init__(
        self,
        ignore_error: bool = False,
        external_to_global_root: bool = False,
        **error: Any
        # note: adding a comma after `Any` above creates a SyntaxError
    ) -> None:
        self.line = error["line"]  # type: int
        self.column = error["column"]  # type: int
        self.path = error["path"]  # type: str
        self.code = error["code"]  # type: int
        self.name = error["name"]  # type: str
        self.description = error["description"]  # type: str
        self.long_description = error.get("long_description", "")  # type: str
        self.concise_description = error.get("concise_description", "")  # type: str
        self.inference = error["inference"]  # type: str
        self.ignore_error = ignore_error  # type: bool
        self.external_to_global_root = external_to_global_root  # type: bool

    def __repr__(self) -> str:
        if is_capable_terminal(file=sys.stdout):
            key = self._key_with_color()
        else:
            key = self.__key()
        return key + " " + self.description

    def __key(self) -> str:
        return self.path + ":" + str(self.line) + ":" + str(self.column)

    def _key_with_color(self) -> str:
        return (
            Color.RED
            + self.path
            + Format.CLEAR
            + ":"
            + Color.YELLOW
            + str(self.line)
            + Format.CLEAR
            + ":"
            + Color.YELLOW
            + str(self.column)
            + Format.CLEAR
        )

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Error):
            return False
        return self.__key() == other.__key()

    def __lt__(self, other: object) -> bool:
        if not isinstance(other, Error):
            return False
        return self.__key() < other.__key()

    def __hash__(self) -> int:
        return hash(self.__key())

    def is_ignored(self) -> bool:
        return self.ignore_error

    def is_external_to_global_root(self) -> bool:
        return self.external_to_global_root
