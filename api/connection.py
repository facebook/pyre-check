#!/usr/bin/env python3
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import enum
import json
import logging
import subprocess
from pathlib import Path
from types import TracebackType
from typing import Any, Dict, List, NamedTuple, Optional

from typing_extensions import TypedDict


LOG: logging.Logger = logging.getLogger(__name__)


class ExitCode(enum.IntEnum):
    SUCCESS = 0
    FOUND_ERRORS = 1
    # See client/commands/command.py for more exit codes.


# We use NamedTuple instead of dataclasses for Python3.5/6 support.
class PyreCheckResult(NamedTuple):
    exit_code: int
    errors: Optional[List[str]]


# pyre-ignore[33]: We don't have GADT's yet.
class PyreQueryResult(TypedDict):
    response: Any


class PyreQueryError(Exception):
    pass


class PyreConnection:
    def __init__(
        self,
        pyre_directory: Optional[Path] = None,
        pyre_arguments: Optional[List[str]] = None,
    ) -> None:
        self.pyre_directory: Path = (
            pyre_directory if pyre_directory is not None else Path.cwd()
        )
        self.pyre_arguments: List[str] = pyre_arguments or []
        self.server_initialized = False

    def __enter__(self) -> "PyreConnection":
        self.start_server()
        return self

    def __exit__(
        self,
        _type: Optional[BaseException],
        _value: Optional[BaseException],
        _traceback: Optional[TracebackType],
    ) -> None:
        self.stop_server()
        return None

    def add_arguments(self, *arguments: str) -> None:
        self.pyre_arguments += arguments

    def start_server(self) -> PyreCheckResult:
        # incremental will start a new server when needed.
        result = subprocess.run(
            ["pyre", "--noninteractive", *self.pyre_arguments, "incremental"],
            stdout=subprocess.PIPE,
            cwd=str(self.pyre_directory),
        )
        self.server_initialized = True
        return _parse_check_output(result)

    def restart_server(self) -> PyreCheckResult:
        result = _parse_check_output(
            subprocess.run(
                ["pyre", "--noninteractive", *self.pyre_arguments, "restart"],
                stdout=subprocess.PIPE,
                cwd=str(self.pyre_directory),
            )
        )
        self.server_initialized = True
        return result

    def stop_server(self) -> None:
        subprocess.run(
            ["pyre", "--noninteractive", *self.pyre_arguments, "stop"],
            check=True,
            cwd=str(self.pyre_directory),
        )

    @staticmethod
    def _validate_query_response(response: str) -> PyreQueryResult:
        try:
            response = json.loads(response)
        except json.decoder.JSONDecodeError as decode_error:
            raise PyreQueryError(f"`{response} is not valid JSON.") from decode_error
        if "error" in response:
            raise PyreQueryError(response["error"])
        if "response" not in response:
            raise PyreQueryError(
                'The server response is invalid: It does not contain an "error" or'
                f'"response" field. Response: `{response}`."'
            )
        return response

    def query_server(self, query: str) -> PyreQueryResult:
        if not self.server_initialized:
            result = self.start_server()
            if result.exit_code not in (ExitCode.SUCCESS, ExitCode.FOUND_ERRORS):
                raise PyreQueryError(
                    f"Error while starting a pyre server, Pyre exited with a code of {result.exit_code}."
                )

        LOG.debug(f"Running query: `pyre query '{query}'`")
        result = subprocess.run(
            ["pyre", "--noninteractive", *self.pyre_arguments, "query", query],
            stdout=subprocess.PIPE,
            cwd=str(self.pyre_directory),
        )
        if result.returncode != 0:
            raise PyreQueryError(
                f"Error while running query, Pyre exited with a code of {result.returncode}."
            )
        return self._validate_query_response(result.stdout.decode())


def _parse_check_output(
    completed_process: "subprocess.CompletedProcess[bytes]",
) -> PyreCheckResult:
    errors = completed_process.stdout.decode().split()
    return PyreCheckResult(exit_code=completed_process.returncode, errors=errors)
