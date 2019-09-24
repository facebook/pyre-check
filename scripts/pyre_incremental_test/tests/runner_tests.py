# pyre-strict

import json
import unittest
from dataclasses import asdict
from pathlib import Path
from typing import List, Optional

from ..runner import InconsistentOutput, PyreError, compare_server_to_full
from ..specification import Specification
from .test_environment import (
    CommandInput,
    CommandOutput,
    MockExecuteCallable,
    TestEnvironment,
)


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
                    "kind": "hg",
                    "repository": "old_root",
                    "commit_hash": "old_hash",
                },
                "new_state": {"kind": "hg", "commit_hash": "new_hash"},
                "pyre_check_pyre_options": "--option1",
                "pyre_start_pyre_options": "--option2",
                "pyre_incremental_pyre_options": "--option3",
            }
        )

        expected_commands = [
            CommandInput(Path("old_root"), "hg update old_hash"),
            CommandInput(Path("old_root"), "pyre --option2 --no-saved-state restart"),
            CommandInput(Path("old_root"), "hg update new_hash"),
            CommandInput(
                Path("old_root"),
                "pyre --option3 --output=json --noninteractive incremental",
            ),
            CommandInput(Path("old_root"), "pyre stop"),
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
