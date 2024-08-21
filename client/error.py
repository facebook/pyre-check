# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module provides client-side represetations for error types that
originate in the daemon: the Error class represents a type error, while
TaintConfigurationError and ModelVerificationError represent problems
validating a Pysa run.
"""

import dataclasses
import json
import logging
import os
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Union

import click

from . import command_arguments, log

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
            concise_description=self.concise_description,
        )

    def with_path(self, path: Path) -> "Error":
        return Error(
            line=self.line,
            column=self.column,
            stop_line=self.stop_line,
            stop_column=self.stop_column,
            path=path,
            code=self.code,
            name=self.name,
            description=self.description,
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


@dataclasses.dataclass(frozen=True)
class TaintConfigurationError:
    path: Optional[Path]
    description: str
    code: int
    start_line: Optional[int]
    start_column: Optional[int]
    stop_line: Optional[int]
    stop_column: Optional[int]

    @staticmethod
    def from_json(error_json: Dict[str, Any]) -> "TaintConfigurationError":
        try:
            error_location = error_json["location"]
            if error_location is not None:
                start_line = error_location["start"]["line"]
                start_column = error_location["start"]["column"]
                stop_line = error_location["stop"]["line"]
                stop_column = error_location["stop"]["column"]
            else:
                start_line = None
                start_column = None
                stop_line = None
                stop_column = None
            return TaintConfigurationError(
                path=(
                    Path(error_json["path"]) if error_json["path"] is not None else None
                ),
                description=error_json["description"],
                code=error_json["code"],
                start_line=start_line,
                start_column=start_column,
                stop_line=stop_line,
                stop_column=stop_column,
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
            "start_line": self.start_line,
            "start_column": self.start_column,
            "stop_line": self.stop_line,
            "stop_column": self.stop_column,
        }

    def to_text(self) -> str:
        path = click.style(str(self.path or "?"), fg="red")
        location = click.style(
            (
                f":{self.start_line}:{self.start_column}"
                if (self.start_line is not None) and (self.start_column is not None)
                else ""
            ),
            fg="red",
        )
        return f"{path}{location} {self.description}"

    def to_sarif(self) -> Dict[str, Any]:
        return {
            "ruleId": (
                "PYRE-TAINT-CONFIGURATION-ERROR-" + str(self.code)
                if self.code is not None
                else "PYRE-TAINT-CONFIGURATION-ERROR-MDL"
            ),
            "level": "error",
            "message": {"text": self.description},
            "locations": [
                {
                    "physicalLocation": {
                        "artifactLocation": {
                            "uri": str(self.path) if self.path is not None else None,
                        },
                        "region": {
                            "startLine": (
                                self.start_line if self.start_line is not None else 0
                            ),
                            "startColumn": (
                                self.start_column
                                if self.start_column is not None
                                else 0
                            ),
                            "endLine": (
                                self.stop_line if self.stop_line is not None else 0
                            ),
                            "endColumn": (
                                self.stop_column if self.stop_column is not None else 1
                            ),
                        },
                    },
                },
            ],
        }

    def get_sarif_rule(self) -> Dict[str, Any]:
        return {
            "id": (
                "PYRE-TAINT-CONFIGURATION-ERROR-" + str(self.code)
                if self.code is not None
                else "PYRE-TAINT-CONFIGURATION-ERROR-MDL"
            ),
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
                path=(
                    Path(error_json["path"]) if error_json["path"] is not None else None
                ),
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
            "ruleId": (
                "PYRE-MODEL-VERIFICATION-ERROR-" + str(self.code)
                if self.code is not None
                else "PYRE-MODEL-VERIFICATION-ERROR-MDL"
            ),
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
            "id": (
                "PYRE-MODEL-VERIFICATION-ERROR-" + str(self.code)
                if self.code is not None
                else "PYRE-MODEL-VERIFICATION-ERROR-MDL"
            ),
            "name": "ModelVerificationError",
            "shortDescription": {"text": "Model verification error"},
            "helpUri": "https://www.pyre-check.org",
            "help": {"text": "Model Verification error"},
        }


def errors_to_sarif(
    errors: Union[
        Sequence[Error],
        Sequence[TaintConfigurationError],
        Sequence[ModelVerificationError],
    ],
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
