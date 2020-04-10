# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import subprocess
import tempfile
import textwrap
import unittest
from pathlib import Path
from unittest.mock import MagicMock, call, mock_open, patch

from .. import errors, filesystem, postprocess, upgrade
from ..upgrade import ExternalVersionControl


VERSION_CONTROL = ExternalVersionControl()


class UpgradeAllTest(unittest.TestCase):
    @patch("subprocess.run")
    @patch.object(upgrade.Configuration, "gather_local_configurations")
    @patch.object(upgrade.Configuration, "find_project_configuration")
    def test_run_upgrade_all(self, find_configuration, gather, run) -> None:
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

        def generate_sandcastle_command(binary_hash, paths, push_blocking) -> bytes:
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
                upgrade.Configuration(
                    Path("a/.pyre_configuration.local"), {"version": 123}
                ),
                upgrade.Configuration(
                    Path("b/.pyre_configuration.local"), {"version": 123}
                ),
            ]
            upgrade.run_upgrade_all(arguments, VERSION_CONTROL)
            find_configuration.assert_not_called()
            run.assert_called_once_with(
                ["scutil", "create"],
                input=generate_sandcastle_command("abc", ["a", "b"], False),
            )

        run.reset_mock()
        with patch("builtins.open", mock_open(read_data=command_json)):
            arguments.hash = None
            gather.return_value = [
                upgrade.Configuration(
                    Path("local/.pyre_configuration.local"), {"version": 123}
                )
            ]
            upgrade.run_upgrade_all(arguments, VERSION_CONTROL)
            find_configuration.assert_not_called()
            run.assert_called_once_with(
                ["scutil", "create"],
                input=generate_sandcastle_command(None, ["local"], False),
            )


class FixmeAllTest(unittest.TestCase):
    @patch.object(
        upgrade.Configuration, "find_project_configuration", return_value=None
    )
    def test_gather_local_configurations(self, _find_project_configuration) -> None:
        process = MagicMock()

        def configuration_lists_equal(
            expected_configurations, actual_configurations
        ) -> bool:
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
            configurations = upgrade.Configuration.gather_local_configurations(
                push_blocking_only=False
            )
            self.assertEqual([], configurations)

        configurations_string = "path/to/.pyre_configuration.local"
        process.stdout = configurations_string.encode()
        configuration_contents = '{"targets":[]}'
        expected_configurations = [
            upgrade.Configuration(
                Path("path/to/.pyre_configuration.local"),
                json.loads(configuration_contents),
            )
        ]
        with patch("subprocess.run", return_value=process) as subprocess_run:
            with patch("builtins.open", mock_open(read_data=configuration_contents)):
                configurations = upgrade.Configuration.gather_local_configurations(
                    push_blocking_only=False
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
            upgrade.Configuration(
                Path("a/.pyre_configuration.local"), json.loads(configuration_contents)
            ),
            upgrade.Configuration(
                Path("b/.pyre_configuration.local"), json.loads(configuration_contents)
            ),
        ]
        with patch("subprocess.run", return_value=process):
            with patch("builtins.open", mock_open(read_data=configuration_contents)):
                configurations = upgrade.Configuration.gather_local_configurations(
                    push_blocking_only=False
                )
                self.assertTrue(
                    configuration_lists_equal(expected_configurations, configurations)
                )

        configurations_string = (
            "a/.pyre_configuration.local\nb/.pyre_configuration.local\n"
        )
        process.stdout = configurations_string.encode()
        configuration_contents = '{"targets":[],\n"coverage":true}'
        expected_configurations = [
            upgrade.Configuration(
                Path("a/.pyre_configuration.local"), json.loads(configuration_contents)
            ),
            upgrade.Configuration(
                Path("b/.pyre_configuration.local"), json.loads(configuration_contents)
            ),
        ]
        with patch("subprocess.run", return_value=process):
            with patch("builtins.open", mock_open(read_data=configuration_contents)):
                configurations = upgrade.Configuration.gather_local_configurations(
                    push_blocking_only=True
                )
                self.assertEqual([], configurations)

    mock_completed_process = MagicMock()
    mock_completed_process.stdout.decode = MagicMock(return_value="[]")

    @patch("subprocess.call")
    @patch("subprocess.run", return_value=mock_completed_process)
    def test_get_errors(self, run, call) -> None:
        configuration = upgrade.Configuration(Path("path"), {})
        configuration.get_errors()
        call.assert_not_called()
        assert run.call_count == 1

        call.reset_mock()
        run.reset_mock()

        configuration.targets = ["//target/..."]
        configuration.get_errors()
        assert call.call_count == 1
        assert run.call_count == 1

    @patch("subprocess.run")
    @patch.object(upgrade.Configuration, "write")
    @patch.object(upgrade.Configuration, "remove_version")
    @patch.object(upgrade.Configuration, "get_errors")
    @patch.object(upgrade.Configuration, "gather_local_configurations")
    @patch(f"{upgrade.__name__}.errors_from_stdin")
    @patch(f"{upgrade.__name__}.run_global_version_update")
    @patch(f"{upgrade.__name__}.fix")
    @patch(f"{upgrade.__name__}.ExternalVersionControl.submit_changes")
    def test_upgrade_project(
        self,
        submit_changes,
        fix,
        run_global_version_update,
        errors_from_stdin,
        gather,
        get_errors,
        remove_version,
        configuration_write,
        subprocess,
    ) -> None:
        arguments = MagicMock()
        arguments.submit = False
        arguments.lint = False
        arguments.from_stdin = False
        arguments.upgrade_version = True
        gather.return_value = []
        upgrade.run_fixme_all(arguments, VERSION_CONTROL)
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
        configuration = upgrade.Configuration(
            Path("/root/local/.pyre_configuration.local"), {"version": 123}
        )
        configuration.get_path()
        upgrade._upgrade_project(
            arguments, configuration, Path("/root"), VERSION_CONTROL
        )
        run_global_version_update.assert_not_called()
        fix.assert_called_once_with(
            pyre_errors,
            arguments.comment,
            arguments.max_line_length,
            arguments.truncate,
        )
        submit_changes.assert_called_once_with(
            False, VERSION_CONTROL.commit_message("local")
        )

        # Test with lint
        submit_changes.reset_mock()
        fix.reset_mock()
        arguments.from_stdin = False
        arguments.lint = True
        upgrade._upgrade_project(
            arguments, configuration, Path("/root"), VERSION_CONTROL
        )
        errors_from_stdin.assert_not_called()
        run_global_version_update.assert_not_called()
        calls = [
            call(
                pyre_errors,
                arguments.comment,
                arguments.max_line_length,
                arguments.truncate,
            ),
            call(
                pyre_errors,
                arguments.comment,
                arguments.max_line_length,
                arguments.truncate,
            ),
        ]
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
        upgrade._upgrade_project(
            arguments, configuration, Path("/root"), VERSION_CONTROL
        )
        # Called in the first round to get initial errors
        errors_from_stdin.assert_called()
        # Called in the second round to get new errors after applying lint.
        get_errors.assert_called_once()
        run_global_version_update.assert_not_called()
        calls = [
            call(
                pyre_errors,
                arguments.comment,
                arguments.max_line_length,
                arguments.truncate,
            ),
            call(
                pyre_errors,
                arguments.comment,
                arguments.max_line_length,
                arguments.truncate,
            ),
        ]
        fix.assert_has_calls(calls)
        submit_changes.assert_called_once_with(
            False, VERSION_CONTROL.commit_message("local")
        )

    @patch("subprocess.run")
    @patch.object(upgrade.Configuration, "gather_local_configurations")
    @patch.object(
        upgrade.Configuration, "find_project_configuration", return_value=Path(".")
    )
    @patch.object(upgrade.Configuration, "write")
    @patch.object(upgrade.Configuration, "remove_version")
    @patch.object(upgrade.Configuration, "get_errors")
    @patch(f"{upgrade.__name__}.run_global_version_update")
    @patch(f"{upgrade.__name__}.fix")
    @patch(f"{upgrade.__name__}.ExternalVersionControl.submit_changes")
    def test_run_fixme_all(
        self,
        submit_changes,
        fix,
        run_global_version_update,
        get_errors,
        remove_version,
        configuration_write,
        find_configuration,
        gather,
        subprocess,
    ) -> None:
        arguments = MagicMock()
        arguments.submit = False
        arguments.lint = False
        arguments.from_stdin = False
        arguments.upgrade_version = True
        gather.return_value = [
            upgrade.Configuration(
                Path("local/.pyre_configuration.local"), {"version": 123}
            )
        ]
        get_errors.return_value = []
        upgrade.run_fixme_all(arguments, VERSION_CONTROL)
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
        upgrade.run_fixme_all(arguments, VERSION_CONTROL)
        run_global_version_update.assert_not_called()
        fix.assert_called_once_with(
            pyre_errors,
            arguments.comment,
            arguments.max_line_length,
            arguments.truncate,
        )
        submit_changes.assert_called_once_with(
            False, VERSION_CONTROL.commit_message("local")
        )

        # Test configuraton with no version set
        fix.reset_mock()
        submit_changes.reset_mock()
        gather.return_value = [
            upgrade.Configuration(Path("local/.pyre_configuration.local"), {})
        ]
        upgrade.run_fixme_all(arguments, VERSION_CONTROL)
        fix.assert_not_called()
        submit_changes.assert_not_called()

        arguments.upgrade_version = False
        fix.reset_mock()
        submit_changes.reset_mock()
        upgrade.run_fixme_all(arguments, VERSION_CONTROL)
        fix.assert_called_once_with(
            pyre_errors,
            arguments.comment,
            arguments.max_line_length,
            arguments.truncate,
        )
        submit_changes.assert_called_once_with(
            False, VERSION_CONTROL.commit_message("local")
        )

        # Test with given hash
        arguments.upgrade_version = True
        fix.reset_mock()
        submit_changes.reset_mock()
        gather.return_value = [
            upgrade.Configuration(
                Path("local/.pyre_configuration.local"), {"version": 123}
            )
        ]
        arguments.hash = "abc"
        arguments.submit = True
        upgrade.run_fixme_all(arguments, VERSION_CONTROL)
        run_global_version_update.assert_not_called()
        fix.assert_called_once_with(
            pyre_errors,
            arguments.comment,
            arguments.max_line_length,
            arguments.truncate,
        )
        submit_changes.assert_called_once_with(
            True, VERSION_CONTROL.commit_message("local")
        )

        # Test with linting
        fix.reset_mock()
        submit_changes.reset_mock()
        arguments.lint = True
        upgrade.run_fixme_all(arguments, VERSION_CONTROL)
        run_global_version_update.assert_not_called()
        calls = [
            call(
                pyre_errors,
                arguments.comment,
                arguments.max_line_length,
                arguments.truncate,
            ),
            call(
                pyre_errors,
                arguments.comment,
                arguments.max_line_length,
                arguments.truncate,
            ),
        ]
        fix.assert_has_calls(calls)
        submit_changes.assert_called_once_with(
            True, VERSION_CONTROL.commit_message("local")
        )

    def test_preserve_ast(self) -> None:
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
            contents = textwrap.dedent(contents)
            file.write(contents.encode())
            errors._fix_file(
                file.name,
                error_map,
                custom_comment=None,
                max_line_length=88,
                truncate=True,
            )

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
            contents = textwrap.dedent(contents)
            file.write(contents.encode())
            errors._fix_file(file.name, error_map)

            file.seek(0)
            updated_contents = file.read().decode()
            file.close()
            self.assertEqual(updated_contents, contents)


class FixmeSingleTest(unittest.TestCase):
    @patch("subprocess.run")
    @patch.object(
        upgrade.Configuration, "find_project_configuration", return_value=Path(".")
    )
    @patch.object(upgrade.Configuration, "write")
    @patch.object(upgrade.Configuration, "remove_version")
    @patch.object(upgrade.Configuration, "get_errors")
    @patch(f"{upgrade.__name__}.fix")
    @patch(f"{upgrade.__name__}.ExternalVersionControl.submit_changes")
    def test_run_fixme_single(
        self,
        submit_changes,
        fix,
        get_errors,
        remove_version,
        configuration_write,
        find_configuration,
        subprocess,
    ) -> None:
        arguments = MagicMock()
        arguments.submit = False
        arguments.submit = True
        arguments.path = Path("local")
        arguments.from_stdin = False
        arguments.lint = False
        get_errors.return_value = []
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            upgrade.run_fixme_single(arguments, VERSION_CONTROL)
            fix.assert_not_called()
            submit_changes.assert_not_called()

        configuration_contents = '{"version": 123}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            upgrade.run_fixme_single(arguments, VERSION_CONTROL)
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
            upgrade.run_fixme_single(arguments, VERSION_CONTROL)
            fix.assert_called_once_with(
                pyre_errors,
                arguments.comment,
                arguments.max_line_length,
                arguments.truncate,
            )
            submit_changes.assert_called_once_with(
                True, VERSION_CONTROL.commit_message("local")
            )


class FixmeTest(unittest.TestCase):
    @patch("subprocess.run")
    @patch.object(Path, "read_text")
    @patch(f"{upgrade.__name__}.errors_from_run")
    @patch(f"{upgrade.__name__}.errors_from_stdin")
    @patch(f"{upgrade.__name__}.ExternalVersionControl.submit_changes")
    def test_run_fixme(
        self, submit_changes, stdin_errors, run_errors, path_read_text, subprocess
    ) -> None:
        arguments = MagicMock()
        arguments.comment = None
        arguments.max_line_length = 88
        arguments.run = False
        arguments.truncate = True

        stdin_errors.return_value = errors.Errors.empty()
        run_errors.return_value = errors.Errors.empty()
        upgrade.run_fixme(arguments, VERSION_CONTROL)

        # Test single error.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [1]: description",
                }
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments, VERSION_CONTROL)
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
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "# @" "generated\n1\n2\n"
            upgrade.run_fixme(arguments, VERSION_CONTROL)
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
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            version_control_with_linters = ExternalVersionControl()
            version_control_with_linters.LINTERS_TO_SKIP = ["TESTLINTER"]
            upgrade.run_fixme(arguments, version_control_with_linters)
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
                        "--skip",
                        "TESTLINTER",
                        "--output",
                        "none",
                    ]
                ),
                call().returncode.__bool__(),
                call(
                    [
                        "arc",
                        "lint",
                        "--apply-patches",
                        "--skip",
                        "TESTLINTER",
                        "--output",
                        "none",
                    ]
                ),
            ]
            subprocess.assert_has_calls(calls, any_order=True)
        arguments.run = False
        arguments.lint = False

        # Test error with custom message.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [1]: description",
                }
            ]
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.return_value = errors.Errors(pyre_errors)
            path_read_text.return_value = "1\n2"
            arguments.comment = "T1234"
            upgrade.run_fixme(arguments, VERSION_CONTROL)
            arguments.comment = None
            path_write_text.assert_called_once_with("# pyre-fixme[1]: T1234\n1\n2")

        # Test error with existing comment.
        with patch.object(Path, "write_text") as path_write_text:
            pyre_errors = [
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [1]: description",
                }
            ]
            arguments.comment = None
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "# existing comment\n2"
            upgrade.run_fixme(arguments, VERSION_CONTROL)
            arguments.comment = None
            path_write_text.assert_called_once_with(
                "# existing comment\n# pyre-fixme[1]: description\n2"
            )

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
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments, VERSION_CONTROL)
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
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments, VERSION_CONTROL)
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
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments, VERSION_CONTROL)
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
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments, VERSION_CONTROL)
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
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments, VERSION_CONTROL)
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
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments, VERSION_CONTROL)
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
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "  # pyre-ignore[0]: [1, 2, 3]\n2"
            upgrade.run_fixme(arguments, VERSION_CONTROL)
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
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = (
                "  # pyre-ignore[0]: [1, 2, 3]\n  #  continuation comment\n2"
            )
            upgrade.run_fixme(arguments, VERSION_CONTROL)
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
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = (
                "  # pyre-ignore[0]: [1, 2, 3]\n  # assumed continuation\n2"
            )
            upgrade.run_fixme(arguments, VERSION_CONTROL)
            arguments.comment = None
            arguments.truncate = True
            path_write_text.assert_called_once_with("2")

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
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = (
                "  # pyre-ignore[0]:\n  #  comment that doesn't fit on one line\n"
                "# pyre-ignore[1]:\n2"
            )
            upgrade.run_fixme(arguments, VERSION_CONTROL)
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
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "# pyre-fixme[1]\n# pyre-fixme[2]\n2"
            upgrade.run_fixme(arguments, VERSION_CONTROL)
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
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1# pyre-ignore[0]: [1, 2, 3]\n2"
            upgrade.run_fixme(arguments, VERSION_CONTROL)
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
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "line = 1\nline = 2\nline = 3"
            upgrade.run_fixme(arguments_short, VERSION_CONTROL)
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
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments, VERSION_CONTROL)
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
            stdin_errors.return_value = errors.Errors(pyre_errors)
            run_errors.side_effect = [
                errors.Errors(pyre_errors),
                errors.Errors(pyre_errors),
            ]
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments, VERSION_CONTROL)
            path_write_text.assert_called_once_with("# pyre-fixme[1]: Concise.\n1\n2")


class FixmeTargetsTest(unittest.TestCase):
    @patch("subprocess.run")
    @patch.object(
        upgrade.Configuration,
        "find_project_configuration",
        return_value=Path(".pyre_configuration"),
    )
    @patch(f"{upgrade.__name__}.run_fixme_targets_file")
    @patch(f"{upgrade.__name__}.ExternalVersionControl.submit_changes")
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
        upgrade.run_fixme_targets(arguments, VERSION_CONTROL)
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
            check_types_options = "pyre, strict",
        a/b/TARGETS:name = "merp",
            srcs = [],
            check_types = True,
            check_types_options = "mypy",
        """
        subprocess.return_value = grep_return
        upgrade.run_fixme_targets(arguments, VERSION_CONTROL)
        fix_file.assert_called_once_with(
            arguments, Path("."), "a/b", ["derp", "herp", "merp"], VERSION_CONTROL
        )
        submit_changes.assert_called_once_with(
            arguments.submit, VERSION_CONTROL.commit_message(". (TARGETS)")
        )

        # Test subdirectory
        subprocess.reset_mock()
        fix_file.reset_mock()
        submit_changes.reset_mock()
        arguments.subdirectory = "derp"
        upgrade.run_fixme_targets(arguments, VERSION_CONTROL)
        subprocess.assert_called_once_with(
            [
                "grep",
                "-RPzo",
                "--include=*TARGETS",
                r"(?s)name = ((?!\n\s*name).)*check_types ?=((?!\n\s*name).)*",
                Path("derp"),
            ],
            stderr=-1,
            stdout=-1,
        )
        fix_file.assert_called_once_with(
            arguments, Path("."), "a/b", ["derp", "herp", "merp"], VERSION_CONTROL
        )
        submit_changes.assert_called_once_with(
            arguments.submit, VERSION_CONTROL.commit_message("derp (TARGETS)")
        )

    @patch("subprocess.run")
    @patch(f"{upgrade.__name__}.fix")
    def test_run_fixme_targets_file(self, fix, subprocess) -> None:
        arguments = MagicMock()
        arguments.subdirectory = None
        arguments.no_commit = False
        arguments.lint = False
        buck_return = MagicMock()
        buck_return.returncode = 1
        buck_return.stderr = b"stderr"
        subprocess.return_value = buck_return
        upgrade.run_fixme_targets_file(
            arguments, Path("."), "a/b", ["derp", "herp"], VERSION_CONTROL
        )
        subprocess.assert_called_once_with(
            [
                "buck",
                "test",
                "--show-full-json-output",
                "a/b:derp-pyre-typecheck",
                "a/b:herp-pyre-typecheck",
                "--",
                "--run-disabled",
            ],
            stdout=-1,
            stderr=-1,
        )
        fix.assert_not_called()

        buck_return.returncode = 0
        subprocess.return_value = buck_return
        upgrade.run_fixme_targets_file(
            arguments, Path("."), "a/b", ["derp", "herp"], VERSION_CONTROL
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
        > 	a/b/x.py:278:28 %s
        > 	a/b/x.py:325:41 %s
        > 	a/b/y.py:86:26 %s
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
        """ % (
            b"Undefined attribute [16]: `Optional` has no attribute `derp`.",
            b"Undefined attribute [16]: `Optional` has no attribute `herp`.",
            b"Incompatible parameter type [6]: Expected `str` for 1st positional only "
            + b"parameter to call `merp` but got `Optional[str]`.",
        )
        subprocess.return_value = buck_return
        expected_errors = errors.Errors(
            [
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
                    + "for 1st positional only parameter to call `merp` but got "
                    + "`Optional[str]`.",
                    "concise_description": "Incompatible parameter type [6]: Expected "
                    + "`str` for 1st positional only parameter to call `merp` but got "
                    + "`Optional[str]`.",
                },
            ]
        )
        upgrade.run_fixme_targets_file(
            arguments, Path("."), "a/b", ["derp", "herp"], VERSION_CONTROL
        )
        fix.assert_called_once_with(
            expected_errors,
            arguments.comment,
            arguments.max_line_length,
            arguments.truncate,
        )

        # Test fallback to type check targets with modified names
        subprocess.reset_mock()
        fix.reset_mock()
        failed_buck_return = MagicMock()
        failed_buck_return.returncode = 5
        failed_buck_return.stdout = b""
        buck_query_return = MagicMock()
        buck_query_return.stdout = b"//target/to:retry-pyre-typecheck"
        subprocess.side_effect = [failed_buck_return, buck_query_return, buck_return]
        upgrade.run_fixme_targets_file(
            arguments, Path("."), "a/b", ["derp"], VERSION_CONTROL
        )
        subprocess.assert_has_calls(
            [
                call(
                    [
                        "buck",
                        "test",
                        "--show-full-json-output",
                        "a/b:derp-pyre-typecheck",
                        "--",
                        "--run-disabled",
                    ],
                    stdout=-1,
                    stderr=-1,
                ),
                call(
                    ["buck", "query", "a/b:derp-pyre-typecheck"], stdout=-1, stderr=-1
                ),
                call(
                    [
                        "buck",
                        "test",
                        "--show-full-json-output",
                        "//target/to:retry-pyre-typecheck",
                        "--",
                        "--run-disabled",
                    ],
                    stdout=-1,
                    stderr=-1,
                ),
            ]
        )
        fix.assert_called_once_with(
            expected_errors,
            arguments.comment,
            arguments.max_line_length,
            arguments.truncate,
        )

        subprocess.reset_mock()
        fix.reset_mock()
        failed_buck_return = MagicMock()
        failed_buck_return.returncode = 5
        failed_buck_return.stdout = b""
        buck_query_return = MagicMock()
        buck_query_return.stdout = b""
        buck_query_return.stderr = b"""
        Error in preloading targets. The rule //a/b:derp-pyre-typecheck could \
        not be found.
        Please check the spelling and whether it is one of the 10 targets in \
        /a/b/TARGETS. (1000 bytes)
        2 similar targets in \
        /data/users/szhu/fbsource/fbcode/tools/build/test/TARGETS are:
          //target/to:retry-pyre-typecheck
          //target/to:retry_non_typecheck
        """
        subprocess.side_effect = [failed_buck_return, buck_query_return, buck_return]
        upgrade.run_fixme_targets_file(
            arguments, Path("."), "a/b", ["derp"], VERSION_CONTROL
        )
        subprocess.assert_has_calls(
            [
                call(
                    [
                        "buck",
                        "test",
                        "--show-full-json-output",
                        "a/b:derp-pyre-typecheck",
                        "--",
                        "--run-disabled",
                    ],
                    stdout=-1,
                    stderr=-1,
                ),
                call(
                    ["buck", "query", "a/b:derp-pyre-typecheck"], stdout=-1, stderr=-1
                ),
                call(
                    [
                        "buck",
                        "test",
                        "--show-full-json-output",
                        "//target/to:retry-pyre-typecheck",
                        "--",
                        "--run-disabled",
                    ],
                    stdout=-1,
                    stderr=-1,
                ),
            ]
        )
        fix.assert_called_once_with(
            expected_errors,
            arguments.comment,
            arguments.max_line_length,
            arguments.truncate,
        )


class MigrateTest(unittest.TestCase):
    @patch("subprocess.check_output")
    @patch(f"{upgrade.__name__}.run_fixme_targets")
    def test_run_migrate_targets(self, fix_targets, subprocess) -> None:
        arguments = MagicMock()
        arguments.subdirectory = "subdirectory"
        process = MagicMock()
        process.stdout = "a/TARGETS\nb/TARGETS\n".encode()
        with patch("subprocess.run", return_value=process) as subprocess_run:
            upgrade.run_migrate_targets(arguments, VERSION_CONTROL)
            subprocess.assert_has_calls(
                [
                    call(
                        [
                            "sed",
                            "-i",
                            r'/check_types_options \?= \?"mypy",/d',
                            "subdirectory/a/TARGETS",
                            "subdirectory/b/TARGETS",
                        ]
                    ),
                    call(
                        [
                            "sed",
                            "-i",
                            r's/typing_options \?= \?".*strict",/check_types_options = "strict",/g',
                            "subdirectory/a/TARGETS",
                            "subdirectory/b/TARGETS",
                        ]
                    ),
                    call(
                        [
                            "sed",
                            "-i",
                            r"s/# \?type: \?ignore$//g",
                            "subdirectory/a/TARGETS",
                            "subdirectory/b/TARGETS",
                        ]
                    ),
                ]
            )
            fix_targets.assert_called_once_with(arguments, VERSION_CONTROL)
            subprocess_run.assert_has_calls(
                [
                    call(
                        ["hg", "files", "--include", r"**/TARGETS"],
                        cwd="subdirectory",
                        stderr=-3,
                        stdout=-1,
                    ),
                    call(
                        ["hg", "files", "--include", r"**/*.py"],
                        cwd="subdirectory",
                        stderr=-3,
                        stdout=-1,
                    ),
                ]
            )
            fix_targets.assert_called_once_with(arguments, VERSION_CONTROL)


class TargetsToConfigurationTest(unittest.TestCase):
    @patch("builtins.open")
    @patch(f"{upgrade.__name__}.Configuration.find_project_configuration")
    @patch(f"{upgrade.__name__}.Configuration.find_local_configuration")
    @patch(f"{upgrade.__name__}.find_targets")
    @patch(f"{upgrade.__name__}.get_filesystem")
    @patch(f"{upgrade.__name__}.remove_non_pyre_ignores")
    @patch(f"{upgrade.__name__}.Configuration.get_errors")
    @patch(f"{upgrade.__name__}.add_local_mode")
    @patch(f"{upgrade.__name__}.fix")
    @patch(f"{upgrade.__name__}.get_lint_status")
    def test_run_targets_to_configuration(
        self,
        get_lint_status,
        fix,
        add_local_mode,
        get_errors,
        remove_non_pyre_ignores,
        get_filesystem,
        find_targets,
        find_local_configuration,
        find_project_configuration,
        open_mock,
    ) -> None:
        arguments = MagicMock()
        arguments.subdirectory = "subdirectory"
        arguments.lint = True
        arguments.glob = False
        arguments.fixme_threshold = None
        find_targets.return_value = {
            "subdirectory/a": ["target_one"],
            "subdirectory/b/c": ["target_three", "target_two"],
        }
        filesystem_list = MagicMock()
        filesystem_list.return_value = []
        get_filesystem.list = filesystem_list
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
        get_errors.side_effect = [
            errors.Errors(pyre_errors),
            errors.Errors(pyre_errors),
        ]

        # Do not attempt to create a configuration when no existing project-level
        # configuration is found.
        find_project_configuration.return_value = None
        find_local_configuration.return_value = None
        upgrade.run_targets_to_configuration(arguments, VERSION_CONTROL)
        open_mock.assert_not_called()
        fix.assert_not_called()
        add_local_mode.assert_not_called()

        # Add to existing project configuration if it lives at given subdirectory
        find_project_configuration.return_value = Path(
            "subdirectory/.pyre_configuration"
        )
        configuration_contents = json.dumps(
            {"version": "abc", "search_path": ["stubs"]}
        )
        with patch("json.dump") as dump_mock:
            mocks = [
                mock_open(read_data=configuration_contents).return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks
            upgrade.run_targets_to_configuration(arguments, VERSION_CONTROL)
            expected_configuration_contents = {
                "search_path": ["stubs"],
                "targets": [
                    "subdirectory/a:target_one",
                    "subdirectory/b/c:target_three",
                    "subdirectory/b/c:target_two",
                ],
                "version": "abc",
            }
            open_mock.assert_has_calls(
                [
                    call(Path("subdirectory/.pyre_configuration")),
                    call(Path("subdirectory/.pyre_configuration"), "w"),
                ]
            )
            dump_mock.assert_called_once_with(
                expected_configuration_contents, mocks[1], indent=2, sort_keys=True
            )
            fix.assert_has_calls(
                [
                    call(
                        errors.Errors(pyre_errors),
                        arguments.comment,
                        arguments.max_line_length,
                        arguments.truncate,
                    ),
                    call(
                        errors.Errors(pyre_errors),
                        arguments.comment,
                        arguments.max_line_length,
                        arguments.truncate,
                    ),
                ]
            )
            add_local_mode.assert_not_called()

        # Create local project configuration
        fix.reset_mock()
        open_mock.reset_mock()
        dump_mock.reset_mock()
        get_errors.side_effect = [
            errors.Errors(pyre_errors),
            errors.Errors(pyre_errors),
        ]
        find_project_configuration.return_value = Path(".pyre_configuration")
        arguments.lint = False
        with patch("json.dump") as dump_mock:
            mocks = [
                mock_open(read_data=configuration_contents).return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks
            upgrade.run_targets_to_configuration(arguments, VERSION_CONTROL)
            expected_configuration_contents = {
                "targets": [
                    "subdirectory/a:target_one",
                    "subdirectory/b/c:target_three",
                    "subdirectory/b/c:target_two",
                ],
                "push_blocking": True,
                "strict": True,
            }
            open_mock.assert_has_calls(
                [
                    call(Path(".pyre_configuration")),
                    call(Path("subdirectory/.pyre_configuration.local"), "w"),
                ]
            )
            dump_mock.assert_called_once_with(
                expected_configuration_contents, mocks[1], indent=2, sort_keys=True
            )
            fix.assert_has_calls(
                [
                    call(
                        errors.Errors(pyre_errors),
                        arguments.comment,
                        arguments.max_line_length,
                        arguments.truncate,
                    )
                ]
            )
            add_local_mode.assert_not_called()

        # Add to existing local project configuration
        fix.reset_mock()
        open_mock.reset_mock()
        dump_mock.reset_mock()
        get_errors.side_effect = [
            errors.Errors(pyre_errors),
            errors.Errors(pyre_errors),
        ]
        find_project_configuration.return_value = Path(".pyre_configuration")
        find_local_configuration.return_value = Path(
            "subdirectory/.pyre_configuration.local"
        )
        configuration_contents = json.dumps({"targets": ["existing:target"]})
        with patch("json.dump") as dump_mock:
            mocks = [
                mock_open(read_data=configuration_contents).return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks
            upgrade.run_targets_to_configuration(arguments, VERSION_CONTROL)
            expected_configuration_contents = {
                "targets": [
                    "existing:target",
                    "subdirectory/a:target_one",
                    "subdirectory/b/c:target_three",
                    "subdirectory/b/c:target_two",
                ]
            }
            open_mock.assert_has_calls(
                [
                    call(Path("subdirectory/.pyre_configuration.local")),
                    call(Path("subdirectory/.pyre_configuration.local"), "w"),
                ]
            )
            dump_mock.assert_called_once_with(
                expected_configuration_contents, mocks[1], indent=2, sort_keys=True
            )
        fix.assert_has_calls(
            [
                call(
                    errors.Errors(pyre_errors),
                    arguments.comment,
                    arguments.max_line_length,
                    arguments.truncate,
                )
            ]
        )
        add_local_mode.assert_not_called()

        # Refuse to nest local configurations
        arguments.subdirectory = "nested/subdirectory"
        fix.reset_mock()
        open_mock.reset_mock()
        dump_mock.reset_mock()
        get_errors.side_effect = [
            errors.Errors(pyre_errors),
            errors.Errors(pyre_errors),
        ]
        find_project_configuration.return_value = Path(".pyre_configuration")
        find_local_configuration.return_value = Path(
            "subdirectory/.pyre_configuration.local"
        )
        configuration_contents = json.dumps({"targets": ["existing:target"]})
        with patch("json.dump") as dump_mock:
            mocks = [
                mock_open(read_data=configuration_contents).return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks
            upgrade.run_targets_to_configuration(arguments, VERSION_CONTROL)
            expected_configuration_contents = {
                "targets": [
                    "existing:target",
                    "subdirectory/a:target_one",
                    "subdirectory/b/c:target_three",
                    "subdirectory/b/c:target_two",
                ]
            }
            open_mock.assert_has_calls(
                [
                    call(Path("subdirectory/.pyre_configuration.local")),
                    call(Path("subdirectory/.pyre_configuration.local"), "w"),
                ]
            )
            dump_mock.assert_called_once_with(
                expected_configuration_contents, mocks[1], indent=2, sort_keys=True
            )
        fix.assert_has_calls(
            [
                call(
                    errors.Errors(pyre_errors),
                    arguments.comment,
                    arguments.max_line_length,
                    arguments.truncate,
                )
            ]
        )
        add_local_mode.assert_not_called()

        # Glob target with error threshold set
        fix.reset_mock()
        open_mock.reset_mock()
        dump_mock.reset_mock()
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
            },
            {
                "line": 3,
                "column": 4,
                "path": "local.py",
                "code": 7,
                "name": "Kind",
                "concise_description": "Error",
                "inference": {},
                "ignore_error": False,
                "external_to_global_root": False,
            },
        ]
        get_errors.side_effect = [
            errors.Errors(pyre_errors),
            errors.Errors(pyre_errors),
        ]
        find_local_configuration.return_value = None
        find_project_configuration.return_value = Path(".pyre_configuration")
        configuration_contents = json.dumps(
            {"version": "abc", "search_path": ["stubs"]}
        )
        arguments.subdirectory = "subdirectory"
        arguments.lint = False
        arguments.glob = True
        arguments.fixme_threshold = 1
        with patch("json.dump") as dump_mock:
            mocks = [
                mock_open(read_data=configuration_contents).return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks
            upgrade.run_targets_to_configuration(arguments, VERSION_CONTROL)
            expected_configuration_contents = {
                "targets": ["//subdirectory/..."],
                "push_blocking": True,
                "strict": True,
            }
            open_mock.assert_has_calls(
                [
                    call(Path(".pyre_configuration")),
                    call(Path("subdirectory/.pyre_configuration.local"), "w"),
                ]
            )
            dump_mock.assert_called_once_with(
                expected_configuration_contents, mocks[1], indent=2, sort_keys=True
            )
            fix.assert_not_called()
            add_local_mode.assert_called_once_with(
                "local.py", filesystem.LocalMode.IGNORE
            )

    @patch("subprocess.run")
    @patch("builtins.open")
    @patch(f"{upgrade.__name__}.Configuration.find_project_configuration")
    @patch(f"{upgrade.__name__}.Configuration.find_local_configuration")
    @patch(f"{upgrade.__name__}.find_targets")
    @patch(f"{upgrade.__name__}.get_filesystem")
    @patch(f"{upgrade.__name__}.Configuration.get_errors")
    @patch(f"{upgrade.__name__}.add_local_mode")
    @patch(f"{upgrade.__name__}.fix")
    @patch(f"{upgrade.__name__}.get_lint_status")
    def test_targets_file_cleanup(
        self,
        get_lint_status,
        fix,
        add_local_mode,
        get_errors,
        get_filesystem,
        find_targets,
        find_local_configuration,
        find_project_configuration,
        open_mock,
        subprocess_run,
    ) -> None:
        arguments = MagicMock()
        arguments.subdirectory = "subdirectory"
        find_targets.return_value = {
            "subdirectory/a": ["target_one"],
            "subdirectory/b/c": ["target_two", "target_three"],
        }

        filesystem_list = MagicMock()
        filesystem_list.return_value = []
        get_filesystem.list = filesystem_list

        configuration_contents = json.dumps(
            {"version": "abc", "search_path": ["stubs"]}
        )
        mocks = [
            mock_open(read_data=configuration_contents).return_value,
            mock_open(read_data="{}").return_value,
        ]
        open_mock.side_effect = mocks

        # Do not modify TARGETS when no existing project-level configuration is found.
        find_project_configuration.return_value = None
        find_local_configuration.return_value = None
        upgrade.run_targets_to_configuration(arguments, VERSION_CONTROL)
        subprocess_run.assert_not_called()

        # Clean up typing-related fields from TARGETS.
        subprocess_run.reset_mock()
        find_project_configuration.return_value = Path(
            "subdirectory/.pyre_configuration"
        )
        upgrade.run_targets_to_configuration(arguments, VERSION_CONTROL)
        subprocess_run.assert_has_calls(
            [
                call(
                    [
                        "sed",
                        "-i",
                        "/typing \\?=.*\\|check_types \\?=.*\\|"
                        "check_types_options \\?=.*\\|typing_options \\?=.*/d",
                    ]
                )
            ]
        )

    @patch("subprocess.check_output")
    def test_deduplicate_targets(self, mock_check_output) -> None:
        configuration = upgrade.Configuration(Path("test"), {"targets": ["//a:a"]})
        configuration.deduplicate_targets()
        expected_targets = ["//a:a"]
        self.assertEqual(expected_targets, configuration.targets)

        mock_check_output.side_effect = [b"a", b"b"]
        configuration = upgrade.Configuration(
            Path("test"), {"targets": ["//a/...", "//b/..."]}
        )
        configuration.deduplicate_targets()
        expected_targets = ["//a/...", "//b/..."]
        self.assertEqual(expected_targets, configuration.targets)

        mock_check_output.side_effect = [b"a", b"a"]
        configuration = upgrade.Configuration(
            Path("test"), {"targets": ["//a/...", "//b/..."]}
        )
        configuration.deduplicate_targets()
        expected_targets = ["//a/..."]
        self.assertEqual(expected_targets, configuration.targets)

        mock_check_output.side_effect = [b"a", b"a\nb"]
        configuration = upgrade.Configuration(
            Path("test"), {"targets": ["//a/...", "//b/..."]}
        )
        configuration.deduplicate_targets()
        expected_targets = ["//a/...", "//b/..."]
        self.assertEqual(expected_targets, configuration.targets)

        mock_check_output.side_effect = [b"a", b"//c:c"]
        configuration = upgrade.Configuration(
            Path("test"), {"targets": ["//a/...", "//b/...", "//c:c"]}
        )
        configuration.deduplicate_targets()
        expected_targets = ["//a/...", "//b/..."]
        self.assertEqual(expected_targets, configuration.targets)


class DecodeTest(unittest.TestCase):
    def test_json_to_errors(self) -> None:
        with patch.object(postprocess.LOG, "error") as mock_error:
            self.assertEqual(
                errors.json_to_errors('[{ "path": "test.py", "key": "value" }]'),
                errors.Errors([{"path": "test.py", "key": "value"}]),
            )
            mock_error.assert_not_called()
            mock_error.reset_mock()

            self.assertEqual(errors.json_to_errors(None), errors.Errors([]))
            mock_error.assert_called_once_with(
                "Recevied no input."
                "If piping from `pyre check` be sure to use `--output=json`."
            )
            mock_error.reset_mock()

            self.assertEqual(
                errors.json_to_errors('[{ "path": "test.py", "key": "value" }'),
                errors.Errors([]),
            )
            mock_error.assert_called_once_with(
                "Recevied invalid JSON as input."
                "If piping from `pyre check` be sure to use `--output=json`."
            )
            mock_error.reset_mock()


class UpdateGlobalVersionTest(unittest.TestCase):
    @patch("subprocess.run")
    @patch(f"{upgrade.__name__}.ExternalVersionControl.submit_changes")
    @patch.object(
        upgrade.Configuration, "find_project_configuration", return_value="/root"
    )
    @patch.object(
        upgrade.Configuration,
        "gather_local_configurations",
        return_value=[
            upgrade.Configuration(
                Path("/root/a/.pyre_configuration.local"), {"push_blocking": False}
            ),
            upgrade.Configuration(
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

            upgrade.run_global_version_update(arguments, VERSION_CONTROL)
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
                ignore_failures=True,
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

            upgrade.run_global_version_update(arguments, VERSION_CONTROL)
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
                ignore_failures=True,
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

            upgrade.run_global_version_update(arguments, VERSION_CONTROL)
            dump.assert_has_calls(
                [call({"version": "abcd"}, mocks[1], indent=2, sort_keys=True)]
            )
            subprocess.assert_has_calls([])


class FilterErrorTest(unittest.TestCase):
    def test_filter_errors(self) -> None:
        error7 = {
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
        error0 = {
            "line": 2,
            "column": 2,
            "path": "local.py",
            "code": 0,
            "name": "Unused ignore",
            "concise_description": "Unused ignore",
            "inference": {},
            "ignore_error": False,
            "external_to_global_root": False,
        }
        pyre_errors = [error7, error0]
        self.assertEqual(errors._filter_errors(pyre_errors, 44), [])

        self.assertEqual(errors._filter_errors(pyre_errors, 7), [error7])

        self.assertEqual(errors._filter_errors(pyre_errors, 0), [error0])

        self.assertEqual(errors._filter_errors(pyre_errors, None), [error7, error0])


class DefaultStrictTest(unittest.TestCase):
    @patch.object(Path, "read_text")
    def test_add_local_mode(self, read_text) -> None:
        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "1\n2"
            upgrade.add_local_mode("local.py", upgrade.LocalMode.UNSAFE)
            path_write_text.assert_called_once_with("# pyre-unsafe\n1\n2")

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "# comment\n# comment\n1"
            upgrade.add_local_mode("local.py", upgrade.LocalMode.UNSAFE)
            path_write_text.assert_called_once_with(
                "# comment\n# comment\n\n# pyre-unsafe\n1"
            )

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "# comment\n# pyre-strict\n1"
            upgrade.add_local_mode("local.py", upgrade.LocalMode.UNSAFE)
            path_write_text.assert_not_called()

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "# comment\n# pyre-ignore-all-errors\n1"
            upgrade.add_local_mode("local.py", upgrade.LocalMode.UNSAFE)
            path_write_text.assert_not_called()

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "1\n2"
            upgrade.add_local_mode("local.py", upgrade.LocalMode.STRICT)
            path_write_text.assert_called_once_with("# pyre-strict\n1\n2")

        with patch.object(Path, "write_text") as path_write_text:
            read_text.return_value = "1\n2"
            upgrade.add_local_mode("local.py", upgrade.LocalMode.IGNORE)
            path_write_text.assert_called_once_with("# pyre-ignore-all-errors\n1\n2")

    @patch.object(
        upgrade.Configuration, "find_project_configuration", return_value=Path(".")
    )
    @patch.object(upgrade.Configuration, "get_directory")
    @patch.object(upgrade.Configuration, "write")
    @patch.object(upgrade.Configuration, "add_strict")
    @patch.object(upgrade.Configuration, "get_errors")
    @patch(f"{upgrade.__name__}.add_local_mode")
    @patch(f"{upgrade.__name__}.get_lint_status", return_value=0)
    def test_run_strict_default(
        self,
        get_lint_status,
        add_local_mode,
        get_errors,
        add_strict,
        configuration_write,
        get_directory,
        find_configuration,
    ) -> None:
        arguments = MagicMock()
        arguments.local_configuration = Path("local")
        get_errors.return_value = []
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            upgrade.run_strict_default(arguments, VERSION_CONTROL)
            add_local_mode.assert_not_called()

        add_local_mode.reset_mock()
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
        get_errors.return_value = errors.Errors(pyre_errors)
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            upgrade.run_strict_default(arguments, VERSION_CONTROL)
            add_local_mode.assert_called_once()

        arguments.reset_mock()
        get_errors.return_value = []
        add_local_mode.reset_mock()
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
        get_errors.return_value = errors.Errors(pyre_errors)
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            upgrade.run_strict_default(arguments, VERSION_CONTROL)
            add_local_mode.assert_called_once()
