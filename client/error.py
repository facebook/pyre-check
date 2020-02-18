# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import sys
from typing import Any, Dict

from . import is_capable_terminal
from .log import Color, Format


class Error:
    ignore_error: bool = False
    external_to_global_root: bool = False

    def __init__(
        self,
        error: Dict[str, Any],
        ignore_error: bool = False,
        external_to_global_root: bool = False,
    ) -> None:
        self.line: int = error["line"]
        self.column: int = error["column"]
        self.path: str = error["path"]
        self.code: int = error["code"]
        self.name: str = error["name"]
        self.description: str = error["description"]
        self.long_description: str = error.get("long_description", "")
        self.concise_description: str = error.get("concise_description", "")
        self.inference: str = error["inference"]
        self.ignore_error: bool = ignore_error or error.get("ignore_error", False)
        self.external_to_global_root: bool = external_to_global_root

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
