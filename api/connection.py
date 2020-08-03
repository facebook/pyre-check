#!/usr/bin/env python3

# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import subprocess
from pathlib import Path
from types import TracebackType
from typing import Any, Dict, List, NamedTuple, Optional


LOG: logging.Logger = logging.getLogger(__name__)


# We use NamedTuple instead of dataclasses for Python3.5/6 support.
class PyreCheckResult(NamedTuple):
    exit_code: int
    errors: Optional[List[str]]


PyreQueryResult = Dict[str, Any]


class PyreConnection:
    def __init__(self, pyre_directory: Optional[Path] = None) -> None:
        self.pyre_directory: Path = (
            pyre_directory if pyre_directory is not None else Path.cwd()
        )
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

    def start_server(self) -> PyreCheckResult:
        subprocess.run(
            ["pyre", "--noninteractive", "start"], cwd=str(self.pyre_directory)
        )
        result = subprocess.run(
            ["pyre", "--noninteractive", "incremental"],
            stdout=subprocess.PIPE,
            cwd=str(self.pyre_directory),
        )
        self.server_initialized = True
        return _parse_check_output(result)

    def restart_server(self) -> PyreCheckResult:
        result = _parse_check_output(
            subprocess.run(
                ["pyre", "--noninteractive", "restart"],
                stdout=subprocess.PIPE,
                cwd=str(self.pyre_directory),
            )
        )
        self.server_initialized = True
        return result

    def stop_server(self) -> None:
        subprocess.run(
            ["pyre", "--noninteractive", "stop"],
            check=True,
            cwd=str(self.pyre_directory),
        )

    def query_server(self, query: str) -> Optional[PyreQueryResult]:
        if not self.server_initialized:
            self.start_server()
        LOG.debug(f"Running query: `pyre query '{query}'`")
        result = subprocess.run(
            ["pyre", "--noninteractive", "query", query],
            stdout=subprocess.PIPE,
            cwd=str(self.pyre_directory),
        )
        if result.returncode != 0:
            return None
        return json.loads(result.stdout.decode())


def _parse_check_output(
    completed_process: "subprocess.CompletedProcess[bytes]",
) -> PyreCheckResult:
    errors = completed_process.stdout.decode().split()
    return PyreCheckResult(exit_code=completed_process.returncode, errors=errors)
