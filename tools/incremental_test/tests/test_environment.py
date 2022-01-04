# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from dataclasses import dataclass
from pathlib import Path
from typing import Callable, List, Optional

from typing_extensions import Final

from ..environment import CommandOutput, Environment


@dataclass
class CommandInput:
    working_directory: Path
    command: str
    stdin: Final[Optional[str]] = None


MockExecuteCallable = Callable[[CommandInput], CommandOutput]


class TestEnvironment(Environment):
    _command_history: List[CommandInput]
    _mock_execute: MockExecuteCallable

    def __init__(self, mock_execute: MockExecuteCallable) -> None:
        self._command_history = []
        self._mock_execute = mock_execute

    @property
    def command_history(self) -> List[CommandInput]:
        return self._command_history

    def run(
        self, working_directory: Path, command: str, stdin: Optional[str]
    ) -> CommandOutput:
        command_input = CommandInput(working_directory, command, stdin)
        self._command_history.append(command_input)
        return self._mock_execute(command_input)
