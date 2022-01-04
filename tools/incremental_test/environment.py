# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import subprocess
from abc import ABC, abstractmethod
from dataclasses import dataclass
from pathlib import Path
from typing import Container, Optional


LOG: logging.Logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class CommandOutput:
    return_code: int
    stdout: str
    stderr: str


class EnvironmentException(Exception):
    pass


class Environment(ABC):
    pyre_binary_override: Optional[str] = None
    typeshed_override: Optional[str] = None
    pyre_client_override: Optional[str] = None

    @abstractmethod
    def run(
        self, working_directory: Path, command: str, stdin: Optional[str]
    ) -> CommandOutput:
        ...

    def checked_run(
        self,
        working_directory: Path,
        command: str,
        stdin: Optional[str] = None,
        expected_return_codes: Container[int] = (0,),
    ) -> CommandOutput:
        output = self.run(working_directory, command, stdin)
        if output.return_code not in expected_return_codes:
            message = (
                f'Running command "{command}" '
                f"under {working_directory} "
                f"returns {output.return_code}.\n"
                f"Stdout = {output.stdout}\n"
                f"Stderr = {output.stderr}"
            )
            raise EnvironmentException(message)
        return output


class SubprocessEnvironment(Environment):
    def run(
        self, working_directory: Path, command: str, stdin: Optional[str]
    ) -> CommandOutput:
        LOG.debug(
            f"Invoking subprocess `{command}` at `{working_directory}`"
            f"{' with stdin' if stdin is not None else ''}"
        )
        result = subprocess.run(
            command.split(),
            cwd=working_directory,
            universal_newlines=True,
            input=stdin,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        return CommandOutput(
            return_code=result.returncode, stdout=result.stdout, stderr=result.stderr
        )
