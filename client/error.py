# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import dataclasses
import json
import logging
import os
import sys
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Union

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
    stop_line: int
    stop_column: int
    path: Path
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
                stop_line=error_json["stop_line"],
                stop_column=error_json["stop_column"],
                path=Path(error_json["path"]),
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

    def relativize_path(self, against: Path) -> "Error":
        relativized_path = Path(os.path.relpath(str(self.path), str(against)))
        return Error(
            line=self.line,
            column=self.column,
            stop_line=self.stop_line,
            stop_column=self.stop_column,
            path=relativized_path,
            code=self.code,
            name=self.name,
            description=self.description,
            long_description=self.long_description,
            concise_description=self.concise_description,
        )

    def to_json(self) -> Dict[str, Any]:
        return {
            "line": self.line,
            "column": self.column,
            "stop_line": self.stop_line,
            "stop_column": self.stop_column,
            "path": str(self.path),
            "code": self.code,
            "name": self.name,
            "description": self.description,
            "long_description": self.long_description,
            "concise_description": self.concise_description,
        }

    def to_text(self) -> str:
        path = click.style(str(self.path), fg="red")
        line = click.style(str(self.line), fg="yellow")
        column = click.style(str(self.column), fg="yellow")
        return f"{path}:{line}:{column} {self.description}"

    def to_sarif(self) -> Dict[str, Any]:
        return {
            "ruleId": "PYRE-ERROR-" + str(self.code),
            "level": "error",
            "message": {"text": self.description},
            "locations": [
                {
                    "physicalLocation": {
                        "artifactLocation": {
                            "uri": str(self.path),
                        },
                        "region": {
                            "startLine": self.line,
                            "startColumn": self.column + 1,
                            "endLine": self.stop_line,
                            "endColumn": self.stop_column,
                        },
                    },
                },
            ],
        }

    def get_sarif_rule(self) -> Dict[str, Any]:
        return {
            "id": "PYRE-ERROR-" + str(self.code),
            "name": self.name.title().replace(" ", ""),
            "shortDescription": {"text": self.name},
            "helpUri": "https://www.pyre-check.org",
            "help": {"text": self.name},
        }


class LegacyError:
    error: Error
    ignore_error: bool = False

    def __init__(
        self,
        error: Error,
        ignore_error: bool,
    ) -> None:
        self.error = error
        self.ignore_error = ignore_error

    @staticmethod
    def create(
        error: Dict[str, Any],
        ignore_error: bool = False,
    ) -> "LegacyError":
        return LegacyError(
            error=Error.from_json(error),
            ignore_error=ignore_error or error.get("ignore_error", False),
        )

    def with_path(self, path: str) -> "LegacyError":
        return LegacyError(
            error=dataclasses.replace(self.error, path=path),
            ignore_error=self.ignore_error,
        )

    def __repr__(self) -> str:
        if terminal.is_capable(file=sys.stdout):
            key = self._key_with_color()
        else:
            key = self.__key()
        return key + " " + self.error.description

    def __key(self) -> str:
        return f"{self.error.path}:{self.error.line}:{self.error.column}"

    def _key_with_color(self) -> str:
        return (
            Color.RED
            + str(self.error.path)
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

    def to_json(self) -> Dict[str, Any]:
        error_mapping = self.error.to_json()
        error_mapping["ignore_error"] = self.ignore_error
        return error_mapping

    def to_text(self) -> str:
        path = click.style(str(self.error.path), fg="red")
        line = click.style(str(self.error.line), fg="yellow")
        column = click.style(str(self.error.column), fg="yellow")
        return f"{path}:{line}:{column} {self.error.description}"

    def to_sarif(self) -> Dict[str, Any]:
        return self.error.to_sarif()

    def get_sarif_rule(self) -> Dict[str, Any]:
        return self.error.get_sarif_rule()


@dataclasses.dataclass(frozen=True)
class TaintConfigurationError:
    path: Optional[Path]
    description: str
    code: int

    @staticmethod
    def from_json(error_json: Dict[str, Any]) -> "TaintConfigurationError":
        try:
            return TaintConfigurationError(
                path=Path(error_json["path"])
                if error_json["path"] is not None
                else None,
                description=error_json["description"],
                code=error_json["code"],
            )
        except KeyError as key_error:
            message = f"Missing field from error json: {key_error}"
            raise ErrorParsingFailure(message) from key_error
        except TypeError as type_error:
            message = f"Field type mismatch: {type_error}"
            raise ErrorParsingFailure(message) from type_error

    @staticmethod
    def from_string(error_string: str) -> "TaintConfigurationError":
        try:
            return TaintConfigurationError.from_json(json.loads(error_string))
        except json.JSONDecodeError as decode_error:
            message = f"Cannot parse JSON: {decode_error}"
            raise ErrorParsingFailure(message) from decode_error

    def to_json(self) -> Dict[str, Any]:
        return {
            "path": str(self.path) if self.path is not None else None,
            "description": self.description,
            "code": self.code,
        }

    def to_text(self) -> str:
        path = click.style(str(self.path or "?"), fg="red")
        return f"{path} {self.description}"

    def to_sarif(self) -> Dict[str, Any]:
        return {
            "ruleId": "PYRE-TAINT-CONFIGURATION-ERROR-" + str(self.code)
            if self.code is not None
            else "PYRE-TAINT-CONFIGURATION-ERROR-MDL",
            "level": "error",
            "message": {"text": self.description},
            "locations": [
                {
                    "physicalLocation": {
                        "artifactLocation": {
                            "uri": str(self.path) if self.path is not None else None,
                        },
                        "region": {
                            "startLine": 0,
                            "startColumn": 0,
                            "endLine": 0,
                            "endColumn": 1,
                        },
                    },
                },
            ],
        }

    def get_sarif_rule(self) -> Dict[str, Any]:
        return {
            "id": "PYRE-TAINT-CONFIGURATION-ERROR-" + str(self.code)
            if self.code is not None
            else "PYRE-TAINT-CONFIGURATION-ERROR-MDL",
            "name": "TaintConfigurationError",
            "shortDescription": {"text": "Taint configuration error"},
            "helpUri": "https://www.pyre-check.org",
            "help": {"text": "Taint Configuration error"},
        }


@dataclasses.dataclass(frozen=True)
class ModelVerificationError:
    line: int
    column: int
    stop_line: int
    stop_column: int
    path: Optional[Path]
    description: str
    code: Optional[int]

    @staticmethod
    def from_json(error_json: Dict[str, Any]) -> "ModelVerificationError":
        try:
            return ModelVerificationError(
                line=error_json["line"],
                column=error_json["column"],
                stop_line=error_json["stop_line"],
                stop_column=error_json["stop_column"],
                path=Path(error_json["path"])
                if error_json["path"] is not None
                else None,
                description=error_json["description"],
                code=error_json.get("code"),
            )
        except KeyError as key_error:
            message = f"Missing field from error json: {key_error}"
            raise ErrorParsingFailure(message) from key_error
        except TypeError as type_error:
            message = f"Field type mismatch: {type_error}"
            raise ErrorParsingFailure(message) from type_error

    @staticmethod
    def from_string(error_string: str) -> "ModelVerificationError":
        try:
            return ModelVerificationError.from_json(json.loads(error_string))
        except json.JSONDecodeError as decode_error:
            message = f"Cannot parse JSON: {decode_error}"
            raise ErrorParsingFailure(message) from decode_error

    def to_json(self) -> Dict[str, Any]:
        return {
            "line": self.line,
            "column": self.column,
            "stop_line": self.stop_line,
            "stop_column": self.stop_column,
            "path": str(self.path) if self.path is not None else None,
            "description": self.description,
            "code": self.code,
        }

    def to_text(self) -> str:
        path = click.style(str(self.path or "?"), fg="red")
        line = click.style(str(self.line), fg="yellow")
        column = click.style(str(self.column), fg="yellow")
        return f"{path}:{line}:{column} {self.description}"

    def to_sarif(self) -> Dict[str, Any]:
        return {
            "ruleId": "PYRE-MODEL-VERIFICATION-ERROR-" + str(self.code)
            if self.code is not None
            else "PYRE-MODEL-VERIFICATION-ERROR-MDL",
            "level": "error",
            "message": {"text": self.description},
            "locations": [
                {
                    "physicalLocation": {
                        "artifactLocation": {
                            "uri": str(self.path) if self.path is not None else None,
                        },
                        "region": {
                            "startLine": self.line,
                            "startColumn": self.column,
                            "endLine": self.stop_line,
                            "endColumn": self.stop_column + 1,
                        },
                    },
                },
            ],
        }

    def get_sarif_rule(self) -> Dict[str, Any]:
        return {
            "id": "PYRE-MODEL-VERIFICATION-ERROR-" + str(self.code)
            if self.code is not None
            else "PYRE-MODEL-VERIFICATION-ERROR-MDL",
            "name": "ModelVerificationError",
            "shortDescription": {"text": "Model verification error"},
            "helpUri": "https://www.pyre-check.org",
            "help": {"text": "Model Verification error"},
        }


def errors_to_sarif(
    errors: Union[
        Sequence[Error],
        Sequence[LegacyError],
        Sequence[TaintConfigurationError],
        Sequence[ModelVerificationError],
    ]
) -> Dict[str, Any]:

    results: List[Dict[str, Any]] = [error.to_sarif() for error in errors]
    rules: List[Dict[str, Any]] = [error.get_sarif_rule() for error in errors]
    return {
        "version": "2.1.0",
        "$schema": "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json",
        "runs": [
            {
                "tool": {
                    "driver": {
                        "name": "Pyre",
                        "informationUri": "https://www.pyre-check.org",
                        # Remove duplicate rules
                        "rules": list({rule["id"]: rule for rule in rules}.values()),
                    }
                },
                "results": results,
            }
        ],
    }


def print_errors(
    errors: Union[
        Sequence[Error],
        Sequence[LegacyError],
        Sequence[TaintConfigurationError],
        Sequence[ModelVerificationError],
    ],
    output: str,
    error_kind: str = "type",
) -> None:
    length = len(errors)
    if length != 0:
        suffix = "s" if length > 1 else ""
        LOG.error(f"Found {length} {error_kind} error{suffix}!")
    else:
        LOG.log(log.SUCCESS, f"No {error_kind} errors found")

    if output == command_arguments.TEXT:
        log.stdout.write("\n".join([error.to_text() for error in errors]))
    elif output == command_arguments.SARIF:
        log.stdout.write(json.dumps(errors_to_sarif(errors)))
    else:
        log.stdout.write(json.dumps([error.to_json() for error in errors]))
