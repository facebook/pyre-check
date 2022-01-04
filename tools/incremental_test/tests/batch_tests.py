# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import unittest
from dataclasses import asdict
from typing import ClassVar, Dict, List

from ..batch import run_batch_test
from ..runner import PyreError
from ..specification import Specification
from .test_environment import (
    CommandInput,
    CommandOutput,
    MockExecuteCallable,
    TestEnvironment,
)


class BasicExecute:
    """
    Mock an environment such that:
    - `pyre check` returns error on hash1
    - `pyre check` is clean on hash2
    - `pyre check` crashes on hash3
    - `pyre incremental` always returns cleanly for all commits
    """

    _current_commit: str
    _error_output: CommandOutput

    _clean_output: ClassVar[CommandOutput] = CommandOutput(
        return_code=0, stdout="", stderr=""
    )

    def __init__(self, mock_error: PyreError) -> None:
        self._current_commit = ""
        self._error_output = CommandOutput(
            return_code=1, stdout=json.dumps([asdict(mock_error)]), stderr=""
        )

    def get_check_result(self) -> CommandOutput:
        if self._current_commit == "hash1":
            return self._error_output
        elif self._current_commit == "hash3":
            raise RuntimeError("Intentionally crash the check")
        else:
            return self._clean_output

    def get_incremental_result(self) -> CommandOutput:
        return self._clean_output

    def __call__(self, input: CommandInput) -> CommandOutput:
        command = input.command
        if command.startswith("hg update"):
            new_commit = command.split()[-1]
            self._current_commit = new_commit
            return self._clean_output
        elif "total_shared_memory_size_over_time" in command:
            return CommandOutput(return_code=0, stdout='[["time", 42]]', stderr="")
        elif "cold_start_phases" in command:
            return CommandOutput(return_code=0, stdout="{}", stderr="")
        elif " profile" in command:
            return CommandOutput(return_code=0, stdout="[{}, {}, {}]", stderr="")
        elif command.endswith("check"):
            return self.get_check_result()
        elif command.endswith("incremental"):
            return self.get_incremental_result()
        else:
            return self._clean_output


class RunnerTest(unittest.TestCase):
    def assert_batch_run(
        self,
        mock_execute: MockExecuteCallable,
        specifications: List[Specification],
        expected_output: List[str],
    ) -> None:
        environment = TestEnvironment(mock_execute)
        actual_output = run_batch_test(environment, specifications)
        self.assertEqual(len(actual_output), len(expected_output))
        for actual, expected in zip(actual_output, expected_output):
            self.assertEqual(actual.get_status(), expected)

    def test_basic(self) -> None:
        def create_dummy_state_json(commit_hash: str) -> Dict[str, str]:
            return {"kind": "hg", "repository": "repo", "commit_hash": commit_hash}

        def create_dummy_update_json(commit_hash: str) -> Dict[str, str]:
            return {"kind": "hg", "commit_hash": commit_hash}

        specification0 = Specification.from_json(
            {
                "old_state": create_dummy_state_json("hash0"),
                "new_state": create_dummy_update_json("hash1"),
            }
        )
        specification1 = Specification.from_json(
            {
                "old_state": create_dummy_state_json("hash1"),
                "new_state": create_dummy_update_json("hash2"),
            }
        )
        specification2 = Specification.from_json(
            {
                "old_state": create_dummy_state_json("hash2"),
                "new_state": create_dummy_update_json("hash3"),
            }
        )

        mock_pyre_error = PyreError(
            line=1, column=1, path="test.py", description="Something is wrong"
        )
        self.assert_batch_run(
            BasicExecute(mock_pyre_error),
            [specification0, specification1, specification2],
            ["fail", "pass", "exception"],
        )
