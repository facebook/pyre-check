# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import argparse
import json
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest.mock import MagicMock, call, mock_open, patch

from .. import errors, postprocess, upgrade, upgrade_core
from ..upgrade import VersionControl


VERSION_CONTROL = VersionControl()


class FixmeAllTest(unittest.TestCase):
    @patch.object(
        upgrade_core.Configuration, "find_project_configuration", return_value=None
    )
    def test_gather_local_configurations(self, _find_project_configuration) -> None:
        process = MagicMock()
        arguments = MagicMock()
        arguments.sandcastle = None
        arguments.push_blocking_only = None

        def configuration_lists_equal(expected_configurations, actual_configurations):
            if len(expected_configurations) != len(actual_configurations):
                print(
                    "Expected {} configurations, got {} configurations".format(
                        len(expected_configurations), len(actual_configurations)
                    )
                )
                return False
            lists_equal = True
            for expected, actual in zip(expected_configurations, actual_configurations):
                if expected.root != actual.root:
                    print(
                        "Expected configuration with root {}, got root {}".format(
                            expected.root, actual.root
                        )
                    )
                    lists_equal = False
                elif expected.targets != actual.targets:
                    print(
                        "Expected configuration with targets {}, got targets {}".format(
                            expected.targets, actual.targets
                        )
                    )
                    lists_equal = False
                elif expected.source_directories != actual.source_directories:
                    print(
                        "Expected configuration with source_directories {}, \
                        got source_directories {}".format(
                            expected.source_directories, actual.source_directories
                        )
                    )
                    lists_equal = False
                elif expected.push_blocking != actual.push_blocking:
                    print(
                        "Expected configuration with push_blocking {}, \
                        got push_blocking {}".format(
                            expected.push_blocking, actual.push_blocking
                        )
                    )
                    lists_equal = False
            return lists_equal

        configurations_string = ""
        process.stdout = configurations_string.encode()
        with patch("subprocess.run", return_value=process):
            configurations = upgrade_core.Configuration.gather_local_configurations(
                arguments
            )
            self.assertEqual([], configurations)

        configurations_string = "path/to/.pyre_configuration.local"
        process.stdout = configurations_string.encode()
        configuration_contents = '{"targets":[]}'
        expected_configurations = [
            upgrade_core.Configuration(
                Path("path/to/.pyre_configuration.local"),
                json.loads(configuration_contents),
            )
        ]
        with patch("subprocess.run", return_value=process) as subprocess_run:
            with patch("builtins.open", mock_open(read_data=configuration_contents)):
                configurations = upgrade_core.Configuration.gather_local_configurations(
                    arguments
                )
                self.assertTrue(
                    configuration_lists_equal(expected_configurations, configurations)
                )
                subprocess_run.assert_called_once_with(
                    ["hg", "files", "--include", r"**\.pyre_configuration.local"],
                    cwd=".",
                    stderr=subprocess.DEVNULL,
                    stdout=subprocess.PIPE,
                )

        configurations_string = (
            "a/.pyre_configuration.local\nb/.pyre_configuration.local\n"
        )
        process.stdout = configurations_string.encode()
        configuration_contents = '{"targets":[],\n"coverage":true}'
        expected_configurations = [
            upgrade_core.Configuration(
                Path("a/.pyre_configuration.local"), json.loads(configuration_contents)
            ),
            upgrade_core.Configuration(
                Path("b/.pyre_configuration.local"), json.loads(configuration_contents)
            ),
        ]
        with patch("subprocess.run", return_value=process):
            with patch("builtins.open", mock_open(read_data=configuration_contents)):
                configurations = upgrade_core.Configuration.gather_local_configurations(
                    arguments
                )
                self.assertTrue(
                    configuration_lists_equal(expected_configurations, configurations)
                )

        arguments.push_blocking_only = True
        configurations_string = (
            "a/.pyre_configuration.local\nb/.pyre_configuration.local\n"
        )
        process.stdout = configurations_string.encode()
        configuration_contents = '{"targets":[],\n"coverage":true}'
        expected_configurations = [
            upgrade_core.Configuration(
                Path("a/.pyre_configuration.local"), json.loads(configuration_contents)
            ),
            upgrade_core.Configuration(
                Path("b/.pyre_configuration.local"), json.loads(configuration_contents)
            ),
        ]
        with patch("subprocess.run", return_value=process):
            with patch("builtins.open", mock_open(read_data=configuration_contents)):
                configurations = upgrade_core.Configuration.gather_local_configurations(
                    arguments
                )
                self.assertEqual([], configurations)

    mock_completed_process = MagicMock()
    mock_completed_process.stdout.decode = MagicMock(return_value="[]")

    @patch("subprocess.call")
    @patch("subprocess.run", return_value=mock_completed_process)
    def test_get_errors(self, run, call) -> None:
        configuration = upgrade_core.Configuration(Path("path"), {})
        configuration.get_errors()
        call.assert_not_called()
        assert run.call_count == 1

        call.reset_mock()
        run.reset_mock()

        configuration.targets = ["//target/..."]
        configuration.get_errors()
        assert call.call_count == 1
        assert run.call_count == 1

    @patch("%s.sort_errors" % upgrade_core.__name__, side_effect=lambda errors: errors)
    @patch("subprocess.run")
    @patch.object(upgrade_core.Configuration, "remove_version")
    @patch.object(upgrade_core.Configuration, "get_errors")
    @patch.object(upgrade_core.Configuration, "gather_local_configurations")
    @patch("%s.errors_from_stdin" % upgrade_core.__name__)
    @patch("%s.run_global_version_update" % upgrade_core.__name__)
    @patch("%s.fix" % upgrade_core.__name__)
    @patch("%s.VersionControl.submit_changes" % upgrade.__name__)
    def test_upgrade_project(
        self,
        submit_changes,
        fix,
        run_global_version_update,
        errors_from_stdin,
        gather,
        get_errors,
        remove_version,
        subprocess,
        sort_errors,
    ) -> None:
        arguments = MagicMock()
        arguments.submit = False
        arguments.sandcastle = None
        arguments.lint = False
        arguments.from_stdin = False
        gather.return_value = []
        upgrade_core.run_fixme_all(arguments, VERSION_CONTROL)
        fix.assert_not_called()
        submit_changes.assert_not_called()

        pyre_errors = [
            {
                "line": 2,
                "column": 4,
                "path": "local.py",
                "code": 7,
                "name": "Kind",
                "concise_description": "Error",
                "inference": {},
                "ignore_error": False,
                "external_to_global_root": False,
            }
        ]
        get_errors.return_value = pyre_errors
        configuration = upgrade_core.Configuration(
            Path("/root/local/.pyre_configuration.local"), {"version": 123}
        )
        configuration.get_path()
        upgrade_core._upgrade_project(
            arguments, configuration, Path("/root"), VERSION_CONTROL
        )
        run_global_version_update.assert_not_called()
        fix.assert_called_once_with(arguments, pyre_errors)
        submit_changes.assert_called_once_with(
            False, VERSION_CONTROL.commit_message("local")
        )

        # Test with lint
        submit_changes.reset_mock()
        fix.reset_mock()
        arguments.from_stdin = False
        arguments.lint = True
        upgrade_core._upgrade_project(
            arguments, configuration, Path("/root"), VERSION_CONTROL
        )
        errors_from_stdin.assert_not_called()
        run_global_version_update.assert_not_called()
        calls = [call(arguments, pyre_errors), call(arguments, pyre_errors)]
        fix.assert_has_calls(calls)
        submit_changes.assert_called_once_with(
            False, VERSION_CONTROL.commit_message("local")
        )

        # Test with from_stdin and lint
        submit_changes.reset_mock()
        fix.reset_mock()
        get_errors.reset_mock()
        arguments.from_stdin = True
        arguments.lint = True
        errors_from_stdin.return_value = pyre_errors
        get_errors.return_value = pyre_errors
        upgrade_core._upgrade_project(
            arguments, configuration, Path("/root"), VERSION_CONTROL
        )
        # Called in the first round to get initial errors
        errors_from_stdin.assert_called()
        # Called in the second round to get new errors after applying lint.
        get_errors.assert_called_once()
        run_global_version_update.assert_not_called()
        calls = [call(arguments, pyre_errors), call(arguments, pyre_errors)]
        fix.assert_has_calls(calls)
        submit_changes.assert_called_once_with(
            False, VERSION_CONTROL.commit_message("local")
        )

    @patch("%s.sort_errors" % upgrade_core.__name__, side_effect=lambda errors: errors)
    @patch("subprocess.run")
    @patch.object(upgrade_core.Configuration, "gather_local_configurations")
    @patch.object(
        upgrade_core.Configuration, "find_project_configuration", return_value=Path(".")
    )
    @patch.object(upgrade_core.Configuration, "remove_version")
    @patch.object(upgrade_core.Configuration, "get_errors")
    @patch("%s.run_global_version_update" % upgrade_core.__name__)
    @patch("%s.fix" % upgrade_core.__name__)
    @patch("%s.VersionControl.submit_changes" % upgrade.__name__)
    def test_run_fixme_all(
        self,
        submit_changes,
        fix,
        run_global_version_update,
        get_errors,
        remove_version,
        find_configuration,
        gather,
        subprocess,
        sort_errors,
    ) -> None:
        arguments = MagicMock()
        arguments.submit = False
        arguments.lint = False
        arguments.sandcastle = None
        arguments.from_stdin = False
        gather.return_value = [
            upgrade_core.Configuration(
                Path("local/.pyre_configuration.local"), {"version": 123}
            )
        ]
        get_errors.return_value = []
        upgrade_core.run_fixme_all(arguments, VERSION_CONTROL)
        run_global_version_update.assert_not_called()
        fix.assert_not_called()
        submit_changes.assert_called_once_with(
            False, VERSION_CONTROL.commit_message("local")
        )

        fix.reset_mock()
        submit_changes.reset_mock()
        pyre_errors = [
            {
                "line": 2,
                "column": 4,
                "path": "local.py",
                "code": 7,
                "name": "Kind",
                "concise_description": "Error",
                "inference": {},
                "ignore_error": False,
                "external_to_global_root": False,
            }
        ]
        get_errors.return_value = pyre_errors
        upgrade_core.run_fixme_all(arguments, VERSION_CONTROL)
        run_global_version_update.assert_not_called()
        fix.assert_called_once_with(arguments, pyre_errors)
        submit_changes.assert_called_once_with(
            False, VERSION_CONTROL.commit_message("local")
        )

        # Test configuraton with no version set
        fix.reset_mock()
        submit_changes.reset_mock()
        gather.return_value = [
            upgrade_core.Configuration(Path("local/.pyre_configuration.local"), {})
        ]
        upgrade_core.run_fixme_all(arguments, VERSION_CONTROL)
        fix.assert_not_called()
        submit_changes.assert_not_called()

        # Test with given hash
        fix.reset_mock()
        submit_changes.reset_mock()
        gather.return_value = [
            upgrade_core.Configuration(
                Path("local/.pyre_configuration.local"), {"version": 123}
            )
        ]
        arguments.hash = "abc"
        arguments.submit = True
        upgrade_core.run_fixme_all(arguments, VERSION_CONTROL)
        run_global_version_update.assert_called_once_with(arguments, VERSION_CONTROL)
        fix.assert_called_once_with(arguments, pyre_errors)
        submit_changes.assert_called_once_with(
            True, VERSION_CONTROL.commit_message("local")
        )

        # Test with linting
        fix.reset_mock()
        submit_changes.reset_mock()
        run_global_version_update.reset_mock()
        arguments.lint = True
        upgrade_core.run_fixme_all(arguments, VERSION_CONTROL)
        run_global_version_update.assert_called_once_with(arguments, VERSION_CONTROL)
        calls = [call(arguments, pyre_errors), call(arguments, pyre_errors)]
        fix.assert_has_calls(calls)
        submit_changes.assert_called_once_with(
            True, VERSION_CONTROL.commit_message("local")
        )

    def test_preserve_ast(self) -> None:
        mock_arguments = argparse.Namespace()
        mock_arguments.max_line_length = 88
        mock_arguments.truncate = True
        error_map = {7: [{"code": "6", "description": "Foo"}]}
        with tempfile.NamedTemporaryFile(delete=False) as file:
            contents = """
def foo(x: int) -> str:
    return str(x)

def bar(x: str) -> str:
    return f\'\'\'
    first line
    second {foo(x)}
    \'\'\'
"""
            file.write(contents.encode())
            upgrade_core.fix_file(mock_arguments, file.name, error_map)

            file.seek(0)
            updated_contents = file.read().decode()
            file.close()
            self.assertEqual(updated_contents, contents)

        with tempfile.NamedTemporaryFile(delete=False) as file:
            contents = """
def foo(x: int) -> str:
    return str(x)

def bar(x: str) -> str:
     if True \
        and True \
        and foo(x):
         return x
"""
            file.write(contents.encode())
            upgrade_core.fix_file(mock_arguments, file.name, error_map)

            file.seek(0)
            updated_contents = file.read().decode()
            file.close()
            self.assertEqual(updated_contents, contents)

    @patch("subprocess.run")
    @patch.object(upgrade_core.Configuration, "gather_local_configurations")
    @patch.object(upgrade_core.Configuration, "find_project_configuration")
    def test_run_fixme_all_sandcastle(self, find_configuration, gather, run) -> None:
        command_json = """
        {
            "command": "CommandName",
            "args": {"hash": null, "paths": null, "push_blocking_only": null},
            "hash": "repository/hash",
            "priority": 0,
            "user": "unixname",
            "alias": "pyre-upgrade",
            "capabilities": {"type": "type", "vcs": "vcs"},
            "timeout": 18000,
            "oncall": "pyre"
        }
        """

        def generate_sandcastle_command(binary_hash, paths, push_blocking):
            command = json.loads(command_json)
            if binary_hash:
                command["args"]["hash"] = binary_hash
            command["args"]["paths"] = paths
            command["args"]["push_blocking_only"] = push_blocking
            return json.dumps(command).encode()

        arguments = MagicMock()
        arguments.sandcastle = Path("sandcastle.json")
        arguments.push_blocking_only = False
        with patch("builtins.open", mock_open(read_data=command_json)):
            arguments.hash = "abc"
            gather.return_value = [
                upgrade_core.Configuration(
                    Path("a/.pyre_configuration.local"), {"version": 123}
                ),
                upgrade_core.Configuration(
                    Path("b/.pyre_configuration.local"), {"version": 123}
                ),
            ]
            upgrade_core.run_fixme_all(arguments, VERSION_CONTROL)
            find_configuration.assert_not_called()
            run.assert_called_once_with(
                ["scutil", "create"],
                input=generate_sandcastle_command("abc", ["a", "b"], False),
            )

        run.reset_mock()
        with patch("builtins.open", mock_open(read_data=command_json)):
            arguments.hash = None
            gather.return_value = [
                upgrade_core.Configuration(
                    Path("local/.pyre_configuration.local"), {"version": 123}
                )
            ]
            upgrade_core.run_fixme_all(arguments, VERSION_CONTROL)
            find_configuration.assert_not_called()
            run.assert_called_once_with(
                ["scutil", "create"],
                input=generate_sandcastle_command(None, ["local"], False),
            )


class FixmeSingleTest(unittest.TestCase):
    @patch("%s.sort_errors" % upgrade_core.__name__, side_effect=lambda errors: errors)
    @patch("subprocess.run")
    @patch.object(
        upgrade_core.Configuration, "find_project_configuration", return_value=Path(".")
    )
    @patch.object(upgrade_core.Configuration, "remove_version")
    @patch.object(upgrade_core.Configuration, "get_errors")
    @patch("%s.fix" % upgrade_core.__name__)
    @patch("%s.VersionControl.submit_changes" % upgrade.__name__)
    def test_run_fixme_single(
        self,
        submit_changes,
        fix,
        get_errors,
        remove_version,
        find_configuration,
        subprocess,
        sort_errors,
    ) -> None:
        arguments = MagicMock()
        arguments.submit = False
        arguments.sandcastle = None
        arguments.submit = True
        arguments.path = Path("local")
        arguments.from_stdin = False
        arguments.lint = False
        get_errors.return_value = []
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            upgrade_core.run_fixme_single(arguments, VERSION_CONTROL)
            fix.assert_not_called()
            submit_changes.assert_not_called()

        configuration_contents = '{"version": 123}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            upgrade_core.run_fixme_single(arguments, VERSION_CONTROL)
            fix.assert_not_called()
            submit_changes.assert_called_once_with(
                True, VERSION_CONTROL.commit_message("local")
            )

        fix.reset_mock()
        submit_changes.reset_mock()
        pyre_errors = [
            {
                "line": 2,
                "column": 4,
                "path": "local.py",
                "code": 7,
                "name": "Kind",
                "concise_description": "Error",
                "inference": {},
                "ignore_error": False,
                "external_to_global_root": False,
            }
        ]
        get_errors.return_value = pyre_errors
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            upgrade_core.run_fixme_single(arguments, VERSION_CONTROL)
            fix.assert_called_once_with(arguments, pyre_errors)
            submit_changes.assert_called_once_with(
                True, VERSION_CONTROL.commit_message("local")
            )


class FixmeTest(unittest.TestCase):
    @patch("subprocess.run")
    @patch.object(Path, "read_text")
    @patch("%s.errors_from_run" % upgrade_core.__name__)
    @patch("%s.errors_from_stdin" % upgrade_core.__name__)
    @patch("%s.VersionControl.submit_changes" % upgrade.__name__)
    def test_run_fixme(
        self, submit_changes, stdin_errors, run_errors, path_read_text, subprocess
    ) -> None:
        arguments = MagicMock()
        arguments.sandcastle = None
        arguments.comment = None
        arguments.max_line_length = 88
        arguments.run = False
        arguments.truncate = True

        stdin_errors.return_value = []
        run_errors.return_value = []
        upgrade_core.run_fixme(arguments, VERSION_CONTROL)

        # Test single error.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [1]: description",
                }
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = "1\n2"
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            path_write_text.assert_called_once_with(
                "# pyre-fixme[1]: description\n1\n2"
            )

        # Generated files.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [1]: description",
                }
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = "# @" "generated\n1\n2\n"
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            path_write_text.assert_not_called()

        # Test single error with lint.
        arguments.run = True
        arguments.lint = True
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [1]: description",
                }
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = "1\n2"
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            calls = [
                call("# pyre-fixme[1]: description\n1\n2"),
                call("# pyre-fixme[1]: description\n1\n2"),
            ]
            path_write_text.assert_has_calls(calls)
            calls = [
                call(
                    [
                        "arc",
                        "lint",
                        "--never-apply-patches",
                        "--enforce-lint-clean",
                        "--output",
                        "none",
                    ]
                ),
                call().returncode.__bool__(),
                call(["arc", "lint", "--apply-patches", "--output", "none"]),
            ]
            subprocess.assert_has_calls(calls)
        arguments.run = False
        arguments.lint = False

        # Test error with comment.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [1]: description",
                }
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = "1\n2"
            arguments.comment = "T1234"
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            arguments.comment = None
            path_write_text.assert_called_once_with("# pyre-fixme[1]: T1234\n1\n2")

        # Test multiple errors and multiple lines.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [1]: description",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [1]: description",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [2]: description",
                },
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = "1\n2"
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            path_write_text.assert_called_once_with(
                "# pyre-fixme[1]: description\n"
                "1\n"
                "# pyre-fixme[1]: description\n"
                "# pyre-fixme[2]: description\n"
                "2"
            )
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [10]: Description one.",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [11]: Description two.",
                },
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = "1\n2"
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            path_write_text.assert_called_once_with(
                "1\n"
                "# pyre-fixme[10]: Description one.\n"
                "# pyre-fixme[11]: Description two.\n"
                "2"
            )
        arguments.max_line_length = 40
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [1]: Description one.",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [2]: Very long description two.",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [3]: Very long description three.",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [4]: Description four.",
                },
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = "1\n2"
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            path_write_text.assert_called_once_with(
                "1\n"
                "# pyre-fixme[1]: Description one.\n"
                "# pyre-fixme[2]: Very long descriptio...\n"
                "# pyre-fixme[3]: Very long descriptio...\n"
                "# pyre-fixme[4]: Description four.\n"
                "2"
            )
        arguments.max_line_length = 36
        arguments.truncate = False
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [2]: Maximum characters.",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [2]: Too many characters.",
                },
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = "1\n2"
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            path_write_text.assert_called_once_with(
                "# pyre-fixme[2]: Maximum characters.\n"
                "1\n"
                "# pyre-fixme[2]: Too many\n"
                "#  characters.\n"
                "2"
            )

        arguments.max_line_length = 40
        arguments.truncate = False
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [1]: Description one.",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [2]: Very long description two.",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [3]: Very long description three.",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [4]: Description four.",
                },
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = "1\n2"
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            path_write_text.assert_called_once_with(
                "1\n"
                "# pyre-fixme[1]: Description one.\n"
                "# pyre-fixme[2]: Very long\n"
                "#  description two.\n"
                "# pyre-fixme[3]: Very long\n"
                "#  description three.\n"
                "# pyre-fixme[4]: Description four.\n"
                "2"
            )
        arguments.truncate = True

        # Test errors in multiple files.
        arguments.max_line_length = 88
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [1]: description",
                },
                {
                    "path": "other.py",
                    "line": 2,
                    "concise_description": "Error [2]: description",
                },
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = "1\n2"
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            path_write_text.has_calls(
                [
                    call("# pyre-fixme[1]: description\n1\n2"),
                    call("1\n#pyre-fixme[2]: description\n2"),
                ]
            )

        # Test removal of extraneous ignore.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous ignore",
                }
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = "  # pyre-ignore[0]: [1, 2, 3]\n2"
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            arguments.comment = None
            path_write_text.assert_called_once_with("2")

        # Test removal of extraneous ignore.
        with patch.object(Path, "write_text") as path_write_text:
            arguments.max_line_length = 30
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous",
                }
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = (
                "  # pyre-ignore[0]: [1, 2, 3]\n#  continuation comment\n2"
            )
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            arguments.comment = None
            arguments.truncate = True
            path_write_text.assert_called_once_with("2")

        # We don't remove legitimate comments.
        with patch.object(Path, "write_text") as path_write_text:
            arguments.max_line_length = 30
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous",
                }
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = (
                "  # pyre-ignore[0]: [1, 2, 3]\n# user comment\n2"
            )
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            arguments.comment = None
            arguments.truncate = True
            path_write_text.assert_called_once_with("# user comment\n2")

        with patch.object(Path, "write_text") as path_write_text:
            arguments.max_line_length = 30
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous ignore that's "
                    "quite long",
                }
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = (
                "  # pyre-ignore[0]:\n#  comment that doesn't fit on one line\n"
                "# pyre-ignore[1]:\n2"
            )
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            arguments.comment = None
            arguments.truncate = True
            path_write_text.assert_called_once_with("# pyre-ignore[1]:\n2")

        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous ignore",
                }
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = "# pyre-fixme[1]\n# pyre-fixme[2]\n2"
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            arguments.comment = None
            path_write_text.assert_called_once_with("# pyre-fixme[2]\n2")

        # Test removal of extraneous ignore (trailing comment).
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous ignore",
                }
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = "1# pyre-ignore[0]: [1, 2, 3]\n2"
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            arguments.comment = None
            path_write_text.assert_called_once_with("1\n2")

        # Test long lines.
        with patch.object(Path, "write_text") as path_write_text:
            arguments_short = MagicMock()
            arguments_short.comment = None
            arguments_short.max_line_length = 35
            arguments_short.run = False

            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [1]: description one, "
                    "that has a pretty verbose text",
                },
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [2]: description-that-will-not-break-"
                    "even-when-facing-adversities",
                },
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = "line = 1\nline = 2\nline = 3"
            upgrade_core.run_fixme(arguments_short, VERSION_CONTROL)
            path_write_text.assert_called_once_with(
                """# FIXME[1]: description one...
                line = 1
                # FIXME[2]: description-tha...
                line = 2
                line = 3""".replace(
                    "                ", ""
                ).replace(
                    "FIXME", "pyre-fixme"
                )
            )

        # Fall back to normal description for backwards compatibility.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "description": "Error [1]: description",
                    "concise_description": "",
                }
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = "1\n2"
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            path_write_text.assert_called_once_with(
                "# pyre-fixme[1]: description\n1\n2"
            )

        # Ensure that we prefer concise descriptions.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "description": "Error [1]: description",
                    "concise_description": "Error[1]: Concise.",
                }
            ]
            stdin_errors.return_value = pyre_errors
            run_errors.return_value = pyre_errors
            path_read_text.return_value = "1\n2"
            upgrade_core.run_fixme(arguments, VERSION_CONTROL)
            path_write_text.assert_called_once_with("# pyre-fixme[1]: Concise.\n1\n2")


class FixmeTargetsTest(unittest.TestCase):
    @patch("subprocess.run")
    @patch.object(
        upgrade_core.Configuration,
        "find_project_configuration",
        return_value=Path(".pyre_configuration"),
    )
    @patch("%s.run_fixme_targets_file" % upgrade_core.__name__)
    @patch("%s.VersionControl.submit_changes" % upgrade.__name__)
    def test_run_fixme_targets(
        self, submit_changes, fix_file, find_configuration, subprocess
    ) -> None:
        arguments = MagicMock()
        arguments.subdirectory = None
        arguments.no_commit = False
        grep_return = MagicMock()
        grep_return.returncode = 1
        grep_return.stderr = b"stderr"
        subprocess.return_value = grep_return
        upgrade_core.run_fixme_targets(arguments, VERSION_CONTROL)
        fix_file.assert_not_called()
        submit_changes.assert_not_called()

        grep_return.returncode = 0
        grep_return.stdout = b"""
        a/b/TARGETS:name = "derp",
            srcs = ["derp.py"],
            check_types = True,
            check_types_options = "pyre",
        a/b/TARGETS:name = "herp",
            srcs = [],
            check_types = True,
            check_types_options = "pyre",
        a/b/TARGETS:name = "merp",
            srcs = [],
            check_types = True,
            check_types_options = "pyre",
        """
        subprocess.return_value = grep_return
        upgrade_core.run_fixme_targets(arguments, VERSION_CONTROL)
        fix_file.assert_called_once_with(
            arguments, Path("."), "a/b", ["derp", "herp", "merp"]
        )
        submit_changes.assert_called_once_with(
            arguments.submit, VERSION_CONTROL.commit_message(". (TARGETS)")
        )

        # Test subdirectory
        subprocess.reset_mock()
        fix_file.reset_mock()
        submit_changes.reset_mock()
        arguments.subdirectory = "derp"
        upgrade_core.run_fixme_targets(arguments, VERSION_CONTROL)
        subprocess.assert_called_once_with(
            [
                "grep",
                "-RPzo",
                "--include=*TARGETS",
                "(?s)name = ((?!\\n\\s*name).)*check_types ?= ?True"
                + '((?!\\n\\s*name).)*check_types_options ?= ?"[^"]*pyre[^"]*",?',
                Path("derp"),
            ],
            stderr=-1,
            stdout=-1,
        )
        fix_file.assert_called_once_with(
            arguments, Path("."), "a/b", ["derp", "herp", "merp"]
        )
        submit_changes.assert_called_once_with(
            arguments.submit, VERSION_CONTROL.commit_message("derp (TARGETS)")
        )

    @patch("%s.sort_errors" % upgrade_core.__name__, side_effect=lambda errors: errors)
    @patch("subprocess.run")
    @patch("%s.fix" % upgrade_core.__name__)
    def test_run_fixme_targets_file(self, fix, subprocess, sort_errors) -> None:
        arguments = MagicMock()
        arguments.subdirectory = None
        arguments.no_commit = False
        arguments.lint = False
        buck_return = MagicMock()
        buck_return.returncode = 1
        buck_return.stderr = b"stderr"
        subprocess.return_value = buck_return
        upgrade_core.run_fixme_targets_file(
            arguments, Path("."), "a/b", ["derp", "herp"]
        )
        fix.assert_not_called()

        buck_return.returncode = 0
        subprocess.return_value = buck_return
        upgrade_core.run_fixme_targets_file(
            arguments, Path("."), "a/b", ["derp", "herp"]
        )
        fix.assert_not_called()

        buck_return.returncode = 32
        buck_return.stdout = b"""
        Discovering tests
        Running 1 tests
        Started new test run: https://url
              a/b:c-typecheck - a.b.c (c.TypeCheck) 57.547 1/1 (failed)
        Test output:
        > All OK.
        >         WARNING: Invoking pyre through buck TARGETS may...
        >         See `https://wiki/configuration/` to set up Pyre for your project.
        >
        > 	a/b/x.py:278:28 Undefined attribute [16]: `Optional` has no attribute `derp`.
        > 	a/b/x.py:325:41 Undefined attribute [16]: `Optional` has no attribute `herp`.
        > 	a/b/y.py:86:26 Incompatible parameter type [6]: Expected `str` for 1st \
anonymous parameter to call `merp` but got `Optional[str]`.
               a/b:c-typecheck - main 0.000 (passed)
        Finished test run: https://url
        Summary (total time 58.75s):
          PASS: 1
          FAIL: 1
            a/b:c-typecheck - a.b.c (c.TypeCheck)
          SKIP: 0
          FATAL: 0
          TIMEOUT: 0
          OMIT: 0
        """
        subprocess.return_value = buck_return
        expected_errors = [
            {
                "line": 278,
                "column": 28,
                "path": Path("a/b/x.py"),
                "code": "16",
                "description": "Undefined attribute [16]: `Optional` has no "
                + "attribute `derp`.",
                "concise_description": "Undefined attribute [16]: `Optional` has "
                + "no attribute `derp`.",
            },
            {
                "line": 325,
                "column": 41,
                "path": Path("a/b/x.py"),
                "code": "16",
                "description": "Undefined attribute [16]: `Optional` has no "
                + "attribute `herp`.",
                "concise_description": "Undefined attribute [16]: `Optional` has "
                + "no attribute `herp`.",
            },
            {
                "line": 86,
                "column": 26,
                "path": Path("a/b/y.py"),
                "code": "6",
                "description": "Incompatible parameter type [6]: Expected `str` "
                + "for 1st anonymous parameter to call `merp` but got `Optional[str]`.",
                "concise_description": "Incompatible parameter type [6]: Expected "
                + "`str` for 1st anonymous parameter to call `merp` but got "
                + "`Optional[str]`.",
            },
        ]
        upgrade_core.run_fixme_targets_file(
            arguments, Path("."), "a/b", ["derp", "herp"]
        )
        fix.assert_called_once_with(arguments, expected_errors)


class DecodeTest(unittest.TestCase):
    def test_json_to_errors(self) -> None:
        with patch.object(postprocess.LOG, "error") as mock_error:
            self.assertEqual(
                errors.json_to_errors('[{ "key": "value" }]'), [{"key": "value"}]
            )
            mock_error.assert_not_called()
            mock_error.reset_mock()

            self.assertEqual(errors.json_to_errors(None), [])
            mock_error.assert_called_once_with(
                "Recevied no input."
                "If piping from `pyre check` be sure to use `--output=json`."
            )
            mock_error.reset_mock()

            self.assertEqual(errors.json_to_errors('[{ "key": "value" }'), [])
            mock_error.assert_called_once_with(
                "Recevied invalid JSON as input."
                "If piping from `pyre check` be sure to use `--output=json`."
            )
            mock_error.reset_mock()


class UpdateGlobalVersionTest(unittest.TestCase):
    @patch("subprocess.run")
    @patch("%s.VersionControl.submit_changes" % upgrade.__name__)
    @patch.object(
        upgrade_core.Configuration, "find_project_configuration", return_value="/root"
    )
    @patch.object(
        upgrade_core.Configuration,
        "gather_local_configurations",
        return_value=[
            upgrade_core.Configuration(
                Path("/root/a/.pyre_configuration.local"), {"push_blocking": False}
            ),
            upgrade_core.Configuration(
                Path("/root/b/.pyre_configuration.local"), {"push_blocking": True}
            ),
        ],
    )
    @patch("builtins.open")
    def test_run_global_version_update(
        self,
        open_mock,
        gather_local_configurations,
        find_project_configuration,
        submit_changes,
        subprocess,
    ) -> None:
        arguments = MagicMock()
        arguments.submit = False
        arguments.sandcastle = None
        arguments.hash = "abcd"
        arguments.paths = []
        arguments.push_blocking_only = False
        with patch("json.dump") as dump:
            mocks = [
                mock_open(read_data='{"version": "old"}').return_value,
                mock_open(read_data="{}").return_value,
                mock_open(read_data='{"push_blocking": false}').return_value,
                mock_open(read_data="{}").return_value,
                mock_open(read_data='{"push_blocking": true}').return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks

            upgrade_core.run_global_version_update(arguments, VERSION_CONTROL)
            dump.assert_has_calls(
                [
                    call({"version": "abcd"}, mocks[1], indent=2, sort_keys=True),
                    call(
                        {"push_blocking": False, "version": "old"},
                        mocks[3],
                        indent=2,
                        sort_keys=True,
                    ),
                    call(
                        {"push_blocking": True, "version": "old"},
                        mocks[5],
                        indent=2,
                        sort_keys=True,
                    ),
                ]
            )
            submit_changes.assert_called_once_with(
                False,
                VERSION_CONTROL.commit_message(
                    "global configuration",
                    summary_override="Automatic upgrade to hash `abcd`",
                ),
            )
        # Push blocking argument: Since the push blocking only argument is only used
        # when gathering local configurations (mocked here), this is a no-op.
        # Documents it.
        submit_changes.reset_mock()
        arguments.push_blocking_only = True
        arguments.submit = True
        with patch("json.dump") as dump:
            mocks = [
                mock_open(read_data='{"version": "old"}').return_value,
                mock_open(read_data="{}").return_value,
                mock_open(read_data='{"push_blocking": false}').return_value,
                mock_open(read_data="{}").return_value,
                mock_open(read_data='{"push_blocking": true}').return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks

            upgrade_core.run_global_version_update(arguments, VERSION_CONTROL)
            dump.assert_has_calls(
                [
                    call({"version": "abcd"}, mocks[1], indent=2, sort_keys=True),
                    call(
                        {"push_blocking": False, "version": "old"},
                        mocks[3],
                        indent=2,
                        sort_keys=True,
                    ),
                    call(
                        {"push_blocking": True, "version": "old"},
                        mocks[5],
                        indent=2,
                        sort_keys=True,
                    ),
                ]
            )
            submit_changes.assert_called_once_with(
                True,
                VERSION_CONTROL.commit_message(
                    "global configuration",
                    summary_override="Automatic upgrade to hash `abcd`",
                ),
            )

        # paths passed from arguments will override the local configuration list
        # Therefore, we only read the first json configuration.
        subprocess.reset_mock()
        arguments.paths = [Path("foo/bar")]
        arguments.push_blocking_only = False
        arguments.submit = False
        with patch("json.dump") as dump:
            mocks = [
                mock_open(read_data='{"version": "old"}').return_value,
                mock_open(read_data="{}").return_value,
                mock_open(read_data='{"push_blocking": false}').return_value,
                mock_open(read_data="{}").return_value,
                mock_open(read_data='{"push_blocking": true}').return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks

            upgrade_core.run_global_version_update(arguments, VERSION_CONTROL)
            dump.assert_has_calls(
                [call({"version": "abcd"}, mocks[1], indent=2, sort_keys=True)]
            )
            subprocess.assert_has_calls([])


class FilterErrorTest(unittest.TestCase):
    def test_filter_errors(self) -> None:
        arguments = MagicMock()
        arguments.sandcastle = None
        arguments.only_fix_error_code = 44
        pyre_errors = [
            {
                "line": 2,
                "column": 4,
                "path": "local.py",
                "code": 7,
                "name": "Kind",
                "concise_description": "Error",
                "inference": {},
                "ignore_error": False,
                "external_to_global_root": False,
            }
        ]
        self.assertEqual(errors.filter_errors(arguments, pyre_errors), [])

        arguments.only_fix_error_code = 7
        self.assertEqual(errors.filter_errors(arguments, pyre_errors), pyre_errors)

        arguments.only_fix_error_code = None
        self.assertEqual(errors.filter_errors(arguments, pyre_errors), pyre_errors)


class DefaultStrictTest(unittest.TestCase):
    @patch.object(Path, "read_text")
    def test_add_local_unsafe(self, read_text) -> None:
        arguments = MagicMock()
        arguments.sandcastle = None
        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "1\n2"
            upgrade_core.add_local_unsafe(arguments, "local.py")
            path_write_text.assert_called_once_with("# pyre-unsafe\n1\n2")

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "# comment\n# comment\n1"
            upgrade_core.add_local_unsafe(arguments, "local.py")
            path_write_text.assert_called_once_with(
                "# comment\n# comment\n\n# pyre-unsafe\n1"
            )

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "# comment\n# pyre-strict\n1"
            upgrade_core.add_local_unsafe(arguments, "local.py")
            path_write_text.assert_not_called()

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "# comment\n# pyre-ignore-all-errors\n1"
            upgrade_core.add_local_unsafe(arguments, "local.py")
            path_write_text.assert_not_called()

    @patch.object(
        upgrade_core.Configuration, "find_project_configuration", return_value=Path(".")
    )
    @patch.object(upgrade_core.Configuration, "get_directory")
    @patch.object(upgrade_core.Configuration, "add_strict")
    @patch.object(upgrade_core.Configuration, "get_errors")
    @patch("%s.add_local_unsafe" % upgrade_core.__name__)
    @patch("%s.get_lint_status" % upgrade_core.__name__, return_value=0)
    def test_run_strict_default(
        self,
        get_lint_status,
        add_local_unsafe,
        get_errors,
        add_strict,
        get_directory,
        find_configuration,
    ) -> None:
        arguments = MagicMock()
        arguments.sandcastle = None
        arguments.local_configuration = Path("local")
        arguments.log_directory = ".pyre"
        get_errors.return_value = []
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            upgrade_core.run_strict_default(arguments, VERSION_CONTROL)
            add_local_unsafe.assert_not_called()

        add_local_unsafe.reset_mock()
        get_errors.reset_mock()
        pyre_errors = [
            {
                "line": 2,
                "column": 4,
                "path": "local.py",
                "code": 7,
                "name": "Kind",
                "concise_description": "Error",
                "inference": {},
                "ignore_error": False,
                "external_to_global_root": False,
            }
        ]
        get_errors.return_value = pyre_errors
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            upgrade_core.run_strict_default(arguments, VERSION_CONTROL)
            add_local_unsafe.assert_called_once()

        arguments.reset_mock()
        get_errors.return_value = []
        add_local_unsafe.reset_mock()
        get_errors.reset_mock()
        pyre_errors = [
            {
                "line": 2,
                "column": 4,
                "path": "local.py",
                "code": 7,
                "name": "Kind",
                "concise_description": "Error",
                "inference": {},
                "ignore_error": False,
                "external_to_global_root": False,
            }
        ]
        get_errors.return_value = pyre_errors
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            upgrade_core.run_strict_default(arguments, VERSION_CONTROL)
            add_local_unsafe.assert_called_once()
