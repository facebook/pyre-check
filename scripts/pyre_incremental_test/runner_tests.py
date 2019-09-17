# pyre-strict

import json
import unittest
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Callable, List, Optional

from .environment import CommandOutput, Environment
from .runner import InconsistentOutput, PyreError, compare_server_to_full
from .specification import Specification


@dataclass
class CommandInput:
    working_directory: Path
    command: str


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

    def run(self, working_directory: Path, command: str) -> CommandOutput:
        command_input = CommandInput(working_directory, command)
        self._command_history.append(command_input)
        return self._mock_execute(command_input)


class RunnerTest(unittest.TestCase):
    def assert_run(
        self,
        mock_execute: MockExecuteCallable,
        specification: Specification,
        expected_commands: List[CommandInput],
        expected_discrepancy: Optional[InconsistentOutput],
    ) -> None:
        environment = TestEnvironment(mock_execute)
        actual_result = compare_server_to_full(environment, specification)
        self.assertEqual(actual_result.discrepancy, expected_discrepancy)
        actual_commands = environment.command_history
        self.assertEqual(actual_commands, expected_commands)

    def test_basic(self) -> None:
        specification = Specification.from_json(
            {
                "old_state": {
                    "kind": "base",
                    "repository": "old_root",
                    "commit_hash": "old_hash",
                },
                "new_state": {
                    "kind": "base",
                    "repository": "new_root",
                    "commit_hash": "new_hash",
                },
                "pyre_check_pyre_options": "--option1",
                "pyre_start_pyre_options": "--option2",
                "pyre_incremental_pyre_options": "--option3",
            }
        )

        expected_commands = [
            CommandInput(Path("old_root"), "hg update old_hash"),
            CommandInput(Path("old_root"), "pyre --option2 --no-saved-state start"),
            CommandInput(Path("new_root"), "hg update new_hash"),
            CommandInput(
                Path("old_root"),
                "pyre --option3 --output=json --noninteractive incremental",
            ),
            CommandInput(Path("old_root"), "pyre stop"),
            CommandInput(Path("new_root"), "hg update new_hash"),
            CommandInput(
                Path("old_root"), "pyre --option1 --output=json --noninteractive check"
            ),
        ]

        def always_clean_execute(command_input: CommandInput) -> CommandOutput:
            return CommandOutput(return_code=0, stdout="", stderr="")

        self.assert_run(
            mock_execute=always_clean_execute,
            specification=specification,
            expected_commands=expected_commands,
            expected_discrepancy=None,
        )

        def consistent_not_clean_execute(command_input: CommandInput) -> CommandOutput:
            pyre_error = PyreError(
                line=1, column=1, path="test.py", description="Something is wrong"
            )
            if command_input.command.endswith(
                "check"
            ) or command_input.command.endswith("incremental"):
                return CommandOutput(
                    return_code=1, stdout=json.dumps([asdict(pyre_error)]), stderr=""
                )
            else:
                return CommandOutput(return_code=0, stdout="", stderr="")

        self.assert_run(
            mock_execute=consistent_not_clean_execute,
            specification=specification,
            expected_commands=expected_commands,
            expected_discrepancy=None,
        )

        def inconsistent_execute0(command_input: CommandInput) -> CommandOutput:
            pyre_error = PyreError(
                line=1, column=1, path="test.py", description="Something is wrong"
            )
            if command_input.command.endswith("check"):
                return CommandOutput(
                    return_code=1, stdout=json.dumps([asdict(pyre_error)]), stderr=""
                )
            else:
                return CommandOutput(return_code=0, stdout="", stderr="")

        self.assert_run(
            mock_execute=inconsistent_execute0,
            specification=specification,
            expected_commands=expected_commands,
            expected_discrepancy=InconsistentOutput(
                full_check_output=[
                    PyreError(
                        line=1,
                        column=1,
                        path="test.py",
                        description="Something is wrong",
                    )
                ],
                incremental_check_output=[],
            ),
        )

        def inconsistent_execute1(command_input: CommandInput) -> CommandOutput:
            pyre_error0 = PyreError(
                line=1, column=1, path="test.py", description="Something is wrong"
            )
            pyre_error1 = PyreError(
                line=2, column=2, path="test2.py", description="Something else is wrong"
            )
            pyre_error2 = PyreError(
                line=3, column=3, path="test3.py", description="Everything's broken!"
            )
            if command_input.command.endswith("check"):
                return CommandOutput(
                    return_code=1, stdout=json.dumps([asdict(pyre_error0)]), stderr=""
                )
            elif command_input.command.endswith("incremental"):
                return CommandOutput(
                    return_code=1,
                    stdout=json.dumps([asdict(pyre_error1), asdict(pyre_error2)]),
                    stderr="",
                )
            else:
                return CommandOutput(return_code=0, stdout="", stderr="")

        self.assert_run(
            mock_execute=inconsistent_execute1,
            specification=specification,
            expected_commands=expected_commands,
            expected_discrepancy=InconsistentOutput(
                full_check_output=[
                    PyreError(
                        line=1,
                        column=1,
                        path="test.py",
                        description="Something is wrong",
                    )
                ],
                incremental_check_output=[
                    PyreError(
                        line=2,
                        column=2,
                        path="test2.py",
                        description="Something else is wrong",
                    ),
                    PyreError(
                        line=3,
                        column=3,
                        path="test3.py",
                        description="Everything's broken!",
                    ),
                ],
            ),
        )
