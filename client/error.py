# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import dataclasses
import json
import logging
import sys
from typing import Any, Dict, Sequence

import click

from . import command_arguments, log, terminal
from .log import Color, Format


LOG: logging.Logger = logging.getLogger(__name__)


class ErrorParsingFailure(Exception):
    pass


@dataclasses.dataclass(frozen=True)
class Error:
    line: int
    column: int
    path: str
    code: int
    name: str
    description: str
    long_description: str = ""
    concise_description: str = ""

    @staticmethod
    def from_json(error_json: Dict[str, Any]) -> "Error":
        try:
            return Error(
                line=error_json["line"],
                column=error_json["column"],
                path=error_json["path"],
                code=error_json["code"],
                name=error_json["name"],
                description=error_json["description"],
                long_description=error_json.get("long_description", ""),
                concise_description=error_json.get("concise_description", ""),
            )
        except KeyError as key_error:
            message = f"Missing field from error json: {key_error}"
            raise ErrorParsingFailure(message) from key_error
        except TypeError as type_error:
            message = f"Field type mismatch: {type_error}"
            raise ErrorParsingFailure(message) from type_error

    @staticmethod
    def from_string(error_string: str) -> "Error":
        try:
            return Error.from_json(json.loads(error_string))
        except json.JSONDecodeError as decode_error:
            message = f"Cannot parse JSON: {decode_error}"
            raise ErrorParsingFailure(message) from decode_error

    def to_json(self) -> Dict[str, Any]:
        return dataclasses.asdict(self)

    def to_text(self) -> str:
        path = click.style(self.path, fg="red")
        line = click.style(str(self.line), fg="yellow")
        column = click.style(str(self.column), fg="yellow")
        return f"{path}:{line}:{column} {self.description}"


def print_errors(errors: Sequence[Error], output: str) -> None:
    length = len(errors)
    if length != 0:
        suffix = "s" if length > 1 else ""
        LOG.error(f"Found {length} type error{suffix}!")
    else:
        LOG.log(log.SUCCESS, "No type errors found")

    if output == command_arguments.TEXT:
        log.stdout.write("\n".join([error.to_text() for error in errors]))
    else:
        log.stdout.write(json.dumps([error.to_json() for error in errors]))


class LegacyError:
    error: Error
    inference: str
    ignore_error: bool = False

    def __init__(
        self,
        error: Error,
        inference: str,
        ignore_error: bool,
    ) -> None:
        self.error = error
        self.inference = inference
        self.ignore_error = ignore_error

    @staticmethod
    def create(
        error: Dict[str, Any],
        ignore_error: bool = False,
    ) -> "LegacyError":
        return LegacyError(
            error=Error.from_json(error),
            inference=error["inference"],
            ignore_error=ignore_error or error.get("ignore_error", False),
        )

    def with_path(self, path: str) -> "LegacyError":
        return LegacyError(
            error=dataclasses.replace(self.error, path=path),
            inference=self.inference,
            ignore_error=self.ignore_error,
        )

    def __repr__(self) -> str:
        if terminal.is_capable(file=sys.stdout):
            key = self._key_with_color()
        else:
            key = self.__key()
        return key + " " + self.error.description

    def __key(self) -> str:
        return (
            self.error.path + ":" + str(self.error.line) + ":" + str(self.error.column)
        )

    def _key_with_color(self) -> str:
        return (
            Color.RED
            + self.error.path
            + Format.CLEAR
            + ":"
            + Color.YELLOW
            + str(self.error.line)
            + Format.CLEAR
            + ":"
            + Color.YELLOW
            + str(self.error.column)
            + Format.CLEAR
        )

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, LegacyError):
            return False
        return self.__key() == other.__key()

    def __lt__(self, other: object) -> bool:
        if not isinstance(other, LegacyError):
            return False
        return self.__key() < other.__key()

    def __hash__(self) -> int:
        return hash(self.__key())

    def is_ignored(self) -> bool:
        return self.ignore_error
