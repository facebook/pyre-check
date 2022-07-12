# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import unittest
from dataclasses import asdict
from pathlib import Path
from typing import List, Optional
from unittest.mock import MagicMock, patch

from ..runner import (
    compare_server_to_full,
    InconsistentOutput,
    PyreError,
    ResultComparison,
)
from ..specification import Specification
from .test_environment import (
    CommandInput,
    CommandOutput,
    MockExecuteCallable,
    TestEnvironment,
)


def mock_stat(_path: str) -> MagicMock:
    stat = MagicMock()
    stat.st_size = 4002
    return stat


mock_temp_file_class: MagicMock = MagicMock()
mock_temp_file_context_manager: MagicMock = mock_temp_file_class.return_value.__enter__
mock_temp_file_context_manager.return_value.name = "tempfile"


class RunnerTest(unittest.TestCase):
    @patch("os.stat", new=mock_stat)
    @patch("tempfile.NamedTemporaryFile", new=mock_temp_file_class)
    def assert_run(
        self,
        mock_execute: MockExecuteCallable,
        specification: Specification,
        expected_commands: List[CommandInput],
        expected_discrepancy: Optional[InconsistentOutput],
        pyre_binary_override: Optional[str] = None,
        typeshed_override: Optional[str] = None,
        pyre_client_override: Optional[str] = None,
    ) -> ResultComparison:
        self.maxDiff = None
        environment = TestEnvironment(mock_execute)
        environment.pyre_binary_override = pyre_binary_override
        environment.typeshed_override = typeshed_override
        environment.pyre_client_override = pyre_client_override
        actual_result = compare_server_to_full(environment, specification)
        self.assertEqual(actual_result.discrepancy, expected_discrepancy)
        actual_commands = environment.command_history
        self.assertEqual(actual_commands, expected_commands)
        return actual_result

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
                "pyre_stop_pyre_options": "--option4",
                "pyre_stop_options": "--option5",
            }
        )

        initial_hash: str = "initial_hash"
        expected_commands = [
            CommandInput(Path("old_root"), "hg whereami"),
            CommandInput(Path("old_root"), "hg update --clean old_hash"),
            CommandInput(
                Path("old_root"),
                "pyre --option2 --no-saved-state --enable-profiling --noninteractive restart",
            ),
            CommandInput(
                Path("old_root"), "pyre profile --profile-output=cold_start_phases"
            ),
            CommandInput(
                Path("old_root"),
                "pyre profile --profile-output=total_shared_memory_size_over_time",
            ),
            CommandInput(Path("old_root"), "pyre query save_server_state('tempfile')"),
            CommandInput(Path("old_root"), "hg update --clean new_hash"),
            CommandInput(
                Path("old_root"), "pyre profile --profile-output=incremental_updates"
            ),
            CommandInput(
                Path("old_root"),
                "pyre --option3 --output=json --noninteractive incremental",
            ),
            CommandInput(Path("old_root"), "pyre --option4 stop --option5"),
            CommandInput(
                Path("old_root"), "pyre --option1 --output=json --noninteractive check"
            ),
            CommandInput(Path("old_root"), f"hg update --clean {initial_hash}"),
        ]

        def always_clean_execute(command_input: CommandInput) -> CommandOutput:
            if command_input.command.startswith("hg whereami"):
                return CommandOutput(return_code=0, stdout=initial_hash, stderr="")
            elif "total_shared_memory_size_over_time" in command_input.command:
                return CommandOutput(return_code=0, stdout='[["time", 42]]', stderr="")
            elif "cold_start_phases" in command_input.command:
                return CommandOutput(return_code=0, stdout="{}", stderr="")
            elif " profile" in command_input.command:
                return CommandOutput(return_code=0, stdout="[{}, {}, {}]", stderr="")
            else:
                return CommandOutput(return_code=0, stdout="", stderr="")

        comparison = self.assert_run(
            mock_execute=always_clean_execute,
            specification=specification,
            expected_commands=expected_commands,
            expected_discrepancy=None,
        )
        cold_start_logs = comparison.profile_logs.cold_start_log
        self.assertEqual(cold_start_logs["heap_size"], 42)
        self.assertEqual(cold_start_logs["saved_state_size"], 4002)

        def consistent_not_clean_execute(command_input: CommandInput) -> CommandOutput:
            pyre_error = PyreError(
                line=1, column=1, path="test.py", description="Something is wrong"
            )
            if command_input.command.startswith("hg whereami"):
                return CommandOutput(return_code=0, stdout=initial_hash, stderr="")
            elif "total_shared_memory_size_over_time" in command_input.command:
                return CommandOutput(return_code=0, stdout='[["time", 42]]', stderr="")
            elif "cold_start_phases" in command_input.command:
                return CommandOutput(return_code=0, stdout="{}", stderr="")
            elif " profile" in command_input.command:
                return CommandOutput(return_code=0, stdout="[{}, {}, {}]", stderr="")
            elif command_input.command.endswith(
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
            if command_input.command.startswith("hg whereami"):
                return CommandOutput(return_code=0, stdout=initial_hash, stderr="")
            elif "total_shared_memory_size_over_time" in command_input.command:
                return CommandOutput(return_code=0, stdout='[["time", 42]]', stderr="")
            elif "cold_start_phases" in command_input.command:
                return CommandOutput(return_code=0, stdout="{}", stderr="")
            elif " profile" in command_input.command:
                return CommandOutput(return_code=0, stdout="[{}, {}, {}]", stderr="")
            elif command_input.command.endswith("check"):
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
            if command_input.command.startswith("hg whereami"):
                return CommandOutput(return_code=0, stdout=initial_hash, stderr="")
            elif "total_shared_memory_size_over_time" in command_input.command:
                return CommandOutput(return_code=0, stdout='[["time", 42]]', stderr="")
            elif "cold_start_phases" in command_input.command:
                return CommandOutput(return_code=0, stdout="{}", stderr="")
            elif " profile" in command_input.command:
                return CommandOutput(return_code=0, stdout="[{}, {}, {}]", stderr="")
            elif command_input.command.endswith("check"):
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
        expected_commands = [
            CommandInput(Path("old_root"), "hg whereami"),
            CommandInput(Path("old_root"), "hg update --clean old_hash"),
            CommandInput(
                Path("old_root"),
                "client --binary bin --typeshed bikeshed --option2 "
                "--no-saved-state --enable-profiling --noninteractive restart",
            ),
            CommandInput(
                Path("old_root"),
                "client --binary bin --typeshed bikeshed profile "
                "--profile-output=cold_start_phases",
            ),
            CommandInput(
                Path("old_root"),
                "client --binary bin --typeshed bikeshed profile "
                "--profile-output=total_shared_memory_size_over_time",
            ),
            CommandInput(
                Path("old_root"),
                "client --binary bin --typeshed bikeshed query "
                "save_server_state('tempfile')",
            ),
            CommandInput(Path("old_root"), "hg update --clean new_hash"),
            CommandInput(
                Path("old_root"),
                "client --binary bin --typeshed bikeshed profile "
                "--profile-output=incremental_updates",
            ),
            CommandInput(
                Path("old_root"),
                "client --binary bin --typeshed bikeshed --option3 "
                "--output=json --noninteractive incremental",
            ),
            CommandInput(
                Path("old_root"),
                "client --binary bin --typeshed bikeshed --option4 stop --option5",
            ),
            CommandInput(
                Path("old_root"),
                "client --binary bin --typeshed bikeshed --option1 "
                "--output=json --noninteractive check",
            ),
            CommandInput(Path("old_root"), f"hg update --clean {initial_hash}"),
        ]

        self.assert_run(
            mock_execute=always_clean_execute,
            specification=specification,
            expected_commands=expected_commands,
            expected_discrepancy=None,
            pyre_binary_override="bin",
            typeshed_override="bikeshed",
            pyre_client_override="client",
        )

    def test_patch(self) -> None:
        patch_content = (
            "diff --git a/client/pyre.py b/client/pyre.py\n"
            "--- a/client/pyre.py\n"
            "+++ b/client/pyre.py\n"
            "@@ -33,6 +33,8 @@\n"
            " from .analysis_directory import AnalysisDirectory\n"
            " from .version import __version__\n"
            "+FOO: int = 42\n"
            "+\n"
            " LOG = logging.getLogger(__name__)  # type: logging.Logger\n"
        )

        specification = Specification.from_json(
            {
                "old_state": {
                    "kind": "hg",
                    "repository": "old_root",
                    "commit_hash": "old_hash",
                },
                "new_state": {
                    "kind": "patch",
                    "patch": patch_content,
                    "patch_flags": "-p1",
                },
            }
        )

        initial_hash: str = "initial_hash"
        expected_commands = [
            CommandInput(Path("old_root"), "hg whereami"),
            CommandInput(Path("old_root"), "hg update --clean old_hash"),
            CommandInput(
                Path("old_root"),
                "pyre  --no-saved-state --enable-profiling --noninteractive restart",
            ),
            CommandInput(
                Path("old_root"), "pyre profile --profile-output=cold_start_phases"
            ),
            CommandInput(
                Path("old_root"),
                "pyre profile --profile-output=total_shared_memory_size_over_time",
            ),
            CommandInput(Path("old_root"), "pyre query save_server_state('tempfile')"),
            CommandInput(Path("old_root"), "patch -p1", patch_content),
            CommandInput(
                Path("old_root"), "pyre profile --profile-output=incremental_updates"
            ),
            CommandInput(
                Path("old_root"), "pyre  --output=json --noninteractive incremental"
            ),
            CommandInput(Path("old_root"), "pyre  stop "),
            CommandInput(
                Path("old_root"), "pyre  --output=json --noninteractive check"
            ),
            CommandInput(Path("old_root"), f"hg update --clean {initial_hash}"),
        ]

        def always_clean_execute(command_input: CommandInput) -> CommandOutput:
            if command_input.command.startswith("hg whereami"):
                return CommandOutput(return_code=0, stdout=initial_hash, stderr="")
            elif "total_shared_memory_size_over_time" in command_input.command:
                return CommandOutput(return_code=0, stdout='[["time", 42]]', stderr="")
            elif "cold_start_phases" in command_input.command:
                return CommandOutput(return_code=0, stdout="{}", stderr="")
            elif " profile" in command_input.command:
                return CommandOutput(return_code=0, stdout="[{}, {}, {}]", stderr="")
            else:
                return CommandOutput(return_code=0, stdout="", stderr="")

        self.assert_run(
            mock_execute=always_clean_execute,
            specification=specification,
            expected_commands=expected_commands,
            expected_discrepancy=None,
        )

    def test_file(self) -> None:
        handle_a = "foo/a.py"
        content_a = "def bar() -> None: ..."
        handle_b = "foo/b.py"
        content_b = "def baz(x: int) -> int: ... "
        handle_c, handle_d = "c.py", "derp/d.py"
        changes = {handle_a: content_a, handle_b: content_b}
        removals = [handle_c, handle_d]

        specification = Specification.from_json(
            {
                "old_state": {
                    "kind": "hg",
                    "repository": "old_root",
                    "commit_hash": "old_hash",
                },
                "new_state": {"kind": "file", "changes": changes, "removals": removals},
            }
        )

        initial_hash = "initial_hash"
        expected_commands = [
            CommandInput(Path("old_root"), "hg whereami"),
            CommandInput(Path("old_root"), "hg update --clean old_hash"),
            CommandInput(
                Path("old_root"),
                "pyre  --no-saved-state --enable-profiling --noninteractive restart",
            ),
            CommandInput(
                Path("old_root"), "pyre profile --profile-output=cold_start_phases"
            ),
            CommandInput(
                Path("old_root"),
                "pyre profile --profile-output=total_shared_memory_size_over_time",
            ),
            CommandInput(Path("old_root"), "pyre query save_server_state('tempfile')"),
            CommandInput(Path("old_root"), "mkdir -p foo"),
            CommandInput(Path("old_root"), f"tee {handle_a}", content_a),
            CommandInput(Path("old_root"), "mkdir -p foo"),
            CommandInput(Path("old_root"), f"tee {handle_b}", content_b),
            CommandInput(Path("old_root"), f"rm -f {handle_c}"),
            CommandInput(Path("old_root"), f"rm -f {handle_d}"),
            CommandInput(
                Path("old_root"), "pyre profile --profile-output=incremental_updates"
            ),
            CommandInput(
                Path("old_root"), "pyre  --output=json --noninteractive incremental"
            ),
            CommandInput(Path("old_root"), "pyre  stop "),
            CommandInput(
                Path("old_root"), "pyre  --output=json --noninteractive check"
            ),
            CommandInput(Path("old_root"), f"hg update --clean {initial_hash}"),
        ]

        # pyre-fixme[53]: Captured variable `initial_hash` is not annotated.
        def always_clean_execute(command_input: CommandInput) -> CommandOutput:
            if command_input.command.startswith("hg whereami"):
                return CommandOutput(return_code=0, stdout=initial_hash, stderr="")
            elif "total_shared_memory_size_over_time" in command_input.command:
                return CommandOutput(return_code=0, stdout='[["time", 42]]', stderr="")
            elif "cold_start_phases" in command_input.command:
                return CommandOutput(return_code=0, stdout="{}", stderr="")
            elif " profile" in command_input.command:
                return CommandOutput(return_code=0, stdout="[{}, {}, {}]", stderr="")
            else:
                return CommandOutput(return_code=0, stdout="", stderr="")

        self.assert_run(
            mock_execute=always_clean_execute,
            specification=specification,
            expected_commands=expected_commands,
            expected_discrepancy=None,
        )

    def test_batch(self) -> None:
        specification = Specification.from_json(
            {
                "old_state": {
                    "kind": "hg",
                    "repository": "old_root",
                    "commit_hash": "old_hash",
                },
                "new_state": {
                    "kind": "batch",
                    "updates": [
                        {"kind": "hg", "commit_hash": "new_hashA"},
                        {"kind": "hg", "commit_hash": "new_hashB"},
                    ],
                },
            }
        )

        initial_hash: str = "initial_hash"
        expected_commands = [
            CommandInput(Path("old_root"), "hg whereami"),
            CommandInput(Path("old_root"), "hg update --clean old_hash"),
            CommandInput(
                Path("old_root"),
                "pyre  --no-saved-state --enable-profiling --noninteractive restart",
            ),
            CommandInput(
                Path("old_root"), "pyre profile --profile-output=cold_start_phases"
            ),
            CommandInput(
                Path("old_root"),
                "pyre profile --profile-output=total_shared_memory_size_over_time",
            ),
            CommandInput(Path("old_root"), "pyre query save_server_state('tempfile')"),
            CommandInput(Path("old_root"), "hg update --clean new_hashA"),
            CommandInput(
                Path("old_root"), "pyre profile --profile-output=incremental_updates"
            ),
            CommandInput(Path("old_root"), "hg update --clean new_hashB"),
            CommandInput(
                Path("old_root"), "pyre profile --profile-output=incremental_updates"
            ),
            CommandInput(
                Path("old_root"), "pyre  --output=json --noninteractive incremental"
            ),
            CommandInput(Path("old_root"), "pyre  stop "),
            CommandInput(
                Path("old_root"), "pyre  --output=json --noninteractive check"
            ),
            CommandInput(Path("old_root"), f"hg update --clean {initial_hash}"),
        ]

        def always_clean_execute(command_input: CommandInput) -> CommandOutput:
            if command_input.command.startswith("hg whereami"):
                return CommandOutput(return_code=0, stdout=initial_hash, stderr="")
            elif "total_shared_memory_size_over_time" in command_input.command:
                return CommandOutput(return_code=0, stdout='[["time", 42]]', stderr="")
            elif "cold_start_phases" in command_input.command:
                return CommandOutput(return_code=0, stdout="{}", stderr="")
            elif " profile" in command_input.command:
                return CommandOutput(return_code=0, stdout="[{}, {}, {}]", stderr="")
            else:
                return CommandOutput(return_code=0, stdout="", stderr="")

        self.assert_run(
            mock_execute=always_clean_execute,
            specification=specification,
            expected_commands=expected_commands,
            expected_discrepancy=None,
        )

    def test_file_state(self) -> None:
        handle_a = "foo/a.py"
        content_a = "def bar() -> None: ..."
        handle_b = "foo/b.py"
        content_b = "def baz(x: int) -> int: ..."
        specification = Specification.from_json(
            {
                "old_state": {
                    "kind": "file",
                    "files": {handle_a: content_a, handle_b: content_b},
                },
                "new_state": {"kind": "file", "removals": [handle_a]},
            }
        )

        expected_commands = [
            CommandInput(Path("."), "mktemp -d"),
            CommandInput(Path("/mock/tmp"), "tee .watchmanconfig", "{}"),
            CommandInput(
                Path("/mock/tmp"),
                "tee .pyre_configuration",
                '{ "source_directories": [ "." ] }',
            ),
            CommandInput(Path("/mock/tmp"), "mkdir -p foo"),
            CommandInput(Path("/mock/tmp"), f"tee {handle_a}", content_a),
            CommandInput(Path("/mock/tmp"), "mkdir -p foo"),
            CommandInput(Path("/mock/tmp"), f"tee {handle_b}", content_b),
            CommandInput(Path("/mock/tmp"), "watchman watch ."),
            CommandInput(
                Path("/mock/tmp"),
                "pyre  --no-saved-state --enable-profiling --noninteractive restart",
            ),
            CommandInput(
                Path("/mock/tmp"), "pyre profile --profile-output=cold_start_phases"
            ),
            CommandInput(
                Path("/mock/tmp"),
                "pyre profile --profile-output=total_shared_memory_size_over_time",
            ),
            CommandInput(Path("/mock/tmp"), "pyre query save_server_state('tempfile')"),
            CommandInput(Path("/mock/tmp"), f"rm -f {handle_a}"),
            CommandInput(
                Path("/mock/tmp"), "pyre profile --profile-output=incremental_updates"
            ),
            CommandInput(
                Path("/mock/tmp"), "pyre  --output=json --noninteractive incremental"
            ),
            CommandInput(Path("/mock/tmp"), "pyre  stop "),
            CommandInput(
                Path("/mock/tmp"), "pyre  --output=json --noninteractive check"
            ),
            CommandInput(Path("/mock/tmp"), "watchman watch-del ."),
            CommandInput(Path("."), "rm -rf /mock/tmp"),
        ]

        def always_clean_execute(command_input: CommandInput) -> CommandOutput:
            if command_input.command.startswith("mktemp"):
                return CommandOutput(return_code=0, stdout="/mock/tmp", stderr="")
            elif "total_shared_memory_size_over_time" in command_input.command:
                return CommandOutput(return_code=0, stdout='[["time", 42]]', stderr="")
            elif "cold_start_phases" in command_input.command:
                return CommandOutput(return_code=0, stdout="{}", stderr="")
            elif " profile" in command_input.command:
                return CommandOutput(return_code=0, stdout="[{}, {}, {}]", stderr="")
            elif "watchman watch" in command_input.command:
                return CommandOutput(return_code=0, stdout="{}", stderr="")
            else:
                return CommandOutput(return_code=0, stdout="", stderr="")

        self.assert_run(
            mock_execute=always_clean_execute,
            specification=specification,
            expected_commands=expected_commands,
            expected_discrepancy=None,
        )

    def test_updated_state(self) -> None:
        specification = Specification.from_json(
            {
                "old_state": {
                    "kind": "updated",
                    "base": {
                        "kind": "hg",
                        "repository": "old_root",
                        "commit_hash": "old_hash",
                    },
                    "updates": [
                        {"kind": "hg", "commit_hash": "new_hashA"},
                        {"kind": "hg", "commit_hash": "new_hashB"},
                    ],
                },
                "new_state": {"kind": "hg", "commit_hash": "new_hashC"},
            }
        )

        initial_hash: str = "initial_hash"
        expected_commands = [
            CommandInput(Path("old_root"), "hg whereami"),
            CommandInput(Path("old_root"), "hg update --clean old_hash"),
            CommandInput(Path("old_root"), "hg update --clean new_hashA"),
            CommandInput(Path("old_root"), "hg update --clean new_hashB"),
            CommandInput(
                Path("old_root"),
                "pyre  --no-saved-state --enable-profiling --noninteractive restart",
            ),
            CommandInput(
                Path("old_root"), "pyre profile --profile-output=cold_start_phases"
            ),
            CommandInput(
                Path("old_root"),
                "pyre profile --profile-output=total_shared_memory_size_over_time",
            ),
            CommandInput(Path("old_root"), "pyre query save_server_state('tempfile')"),
            CommandInput(Path("old_root"), "hg update --clean new_hashC"),
            CommandInput(
                Path("old_root"), "pyre profile --profile-output=incremental_updates"
            ),
            CommandInput(
                Path("old_root"), "pyre  --output=json --noninteractive incremental"
            ),
            CommandInput(Path("old_root"), "pyre  stop "),
            CommandInput(
                Path("old_root"), "pyre  --output=json --noninteractive check"
            ),
            CommandInput(Path("old_root"), f"hg update --clean {initial_hash}"),
        ]

        def always_clean_execute(command_input: CommandInput) -> CommandOutput:
            if command_input.command.startswith("hg whereami"):
                return CommandOutput(return_code=0, stdout=initial_hash, stderr="")
            elif "total_shared_memory_size_over_time" in command_input.command:
                return CommandOutput(return_code=0, stdout='[["time", 42]]', stderr="")
            elif "cold_start_phases" in command_input.command:
                return CommandOutput(return_code=0, stdout="{}", stderr="")
            elif " profile" in command_input.command:
                return CommandOutput(return_code=0, stdout="[{}, {}, {}]", stderr="")
            else:
                return CommandOutput(return_code=0, stdout="", stderr="")

        self.assert_run(
            mock_execute=always_clean_execute,
            specification=specification,
            expected_commands=expected_commands,
            expected_discrepancy=None,
        )
