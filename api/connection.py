#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module contains logic for a Python API to query Pyre
daemons. This API is based on shelling out to the `pyre query`
command.

NOTE: we are in the process of building out a new API based on
socket connections and using the better-typed interface to
code-navigation deamons.
"""

import enum
import json
import logging
import shlex
import subprocess
from pathlib import Path
from types import TracebackType
from typing import Any, List, NamedTuple, Optional

from typing_extensions import TypedDict

LOG: logging.Logger = logging.getLogger(__name__)


class ExitCode(enum.IntEnum):
    SUCCESS = 0
    FOUND_ERRORS = 1
    # See client/commands/commands.py for more exit codes.


# We use NamedTuple instead of dataclasses for Python3.5/6 support.
class PyreCheckResult(NamedTuple):
    exit_code: int
    errors: Optional[List[str]]


# pyre-ignore[33]: We don't have GADT's yet.
class PyreQueryResult(TypedDict):
    response: Any


class Error(Exception):
    pass


class PyreStartError(Error):
    def __init__(self, message: str, exit_code: int) -> None:
        super().__init__(message)
        self.exit_code: int = exit_code


class PyreQueryUnexpectedError(Error):
    pass


class PyreQueryError(Error):
    pass


class PyreConnection:
    def __init__(
        self,
        pyre_directory: Optional[Path] = None,
        pyre_arguments: Optional[List[str]] = None,
        skip_initial_type_check: bool = False,
        wait_on_initialization: bool = False,
        analyze_external_sources: bool = False,
        number_of_buck_threads: Optional[int] = None,
        use_pysa_version: bool = False,
    ) -> None:
        self.pyre_directory: Path = (
            pyre_directory if pyre_directory is not None else Path.cwd()
        )
        self.pyre_arguments: List[str] = pyre_arguments or []
        self.server_initialized = False
        self.skip_initial_type_check = skip_initial_type_check
        self.wait_on_initialization = wait_on_initialization
        self.analyze_external_sources = analyze_external_sources
        self.number_of_buck_threads = number_of_buck_threads
        self.use_pysa_version = use_pysa_version

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
        if self.skip_initial_type_check:
            arguments = [
                "start",
                "--skip-initial-type-check",
            ]
            if self.wait_on_initialization:
                arguments.append("--wait-on-initialization")
            if self.analyze_external_sources:
                arguments.append("--analyze-external-sources")
            if self.number_of_buck_threads:
                arguments.append(
                    f"--number-of-buck-threads={self.number_of_buck_threads}"
                )
            result = self._run_pyre_command(arguments)
            self.server_initialized = True
            return PyreCheckResult(exit_code=result.returncode, errors=None)
        else:
            # incremental will start a new server when needed.
            result = self._run_pyre_command(["incremental"])
            self.server_initialized = True
            return _parse_check_output(result)

    def restart_server(self) -> PyreCheckResult:
        if self.skip_initial_type_check:
            raise ValueError(
                "Restarts are not currently supported when using the "
                + "skip_initial_type_check option."
            )
        result = _parse_check_output(self._run_pyre_command(["restart"]))
        self.server_initialized = True
        return result

    def stop_server(self, ignore_errors: bool = False) -> None:
        if not self.server_initialized:
            return
        self._run_pyre_command(["stop"], check=not ignore_errors)

    @staticmethod
    def _validate_query_response(response: str) -> PyreQueryResult:
        try:
            response = json.loads(response)
        except json.decoder.JSONDecodeError as decode_error:
            raise PyreQueryUnexpectedError(
                f"`{response} is not valid JSON."
            ) from decode_error
        if "error" in response:
            raise PyreQueryError(response["error"])
        if "response" not in response:
            raise PyreQueryUnexpectedError(
                'The server response is invalid: It does not contain an "error" or'
                f'"response" field. Response: `{response}`."'
            )
        return response

    def query_server(self, query: str) -> PyreQueryResult:
        if not self.server_initialized:
            result = self.start_server()
            if result.exit_code not in (ExitCode.SUCCESS, ExitCode.FOUND_ERRORS):
                raise PyreStartError(
                    f"Error while starting a pyre server, Pyre exited with a code of {result.exit_code}.",
                    exit_code=result.exit_code,
                )

        result = self._run_pyre_command(["query", query])
        if result.returncode != 0:
            raise PyreQueryUnexpectedError(
                f"Error while running query, Pyre exited with a code of {result.returncode}."
            )
        return self._validate_query_response(result.stdout.decode())

    def _run_pyre_command(
        self, arguments: List[str], check: bool = False
    ) -> subprocess.CompletedProcess[bytes]:
        command = ["pyre", "--noninteractive"]
        if self.use_pysa_version:
            command.append("--pysa")
        command += self.pyre_arguments
        command += arguments
        LOG.debug(f"Running command: `{' '.join(map(shlex.quote, command))}`")
        return subprocess.run(
            command,
            check=check,
            stdout=subprocess.PIPE,
            cwd=str(self.pyre_directory),
        )


def _parse_check_output(
    completed_process: "subprocess.CompletedProcess[bytes]",
) -> PyreCheckResult:
    errors = completed_process.stdout.decode().split()
    return PyreCheckResult(exit_code=completed_process.returncode, errors=errors)
