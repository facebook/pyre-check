# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import json
import pathlib
import subprocess
import unittest
from unittest.mock import MagicMock, call, mock_open, patch

from .. import upgrade


class FixmeAllTest(unittest.TestCase):
    @patch.object(
        upgrade.Configuration, "find_project_configuration", return_value=None
    )
    def test_gather_local_configurations(self, _find_project_configuration) -> None:
        process = MagicMock()
        arguments = MagicMock()
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
            configurations = upgrade.Configuration.gather_local_configurations(
                arguments
            )
            self.assertEqual([], configurations)

        configurations_string = "path/to/.pyre_configuration.local"
        process.stdout = configurations_string.encode()
        configuration_contents = '{"targets":[]}'
        expected_configurations = [
            upgrade.Configuration(
                "path/to/.pyre_configuration.local", json.loads(configuration_contents)
            )
        ]
        with patch("subprocess.run", return_value=process) as subprocess_run:
            with patch("builtins.open", mock_open(read_data=configuration_contents)):
                configurations = upgrade.Configuration.gather_local_configurations(
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
            upgrade.Configuration(
                "a/.pyre_configuration.local", json.loads(configuration_contents)
            ),
            upgrade.Configuration(
                "b/.pyre_configuration.local", json.loads(configuration_contents)
            ),
        ]
        with patch("subprocess.run", return_value=process):
            with patch("builtins.open", mock_open(read_data=configuration_contents)):
                configurations = upgrade.Configuration.gather_local_configurations(
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
            upgrade.Configuration(
                "a/.pyre_configuration.local", json.loads(configuration_contents)
            ),
            upgrade.Configuration(
                "b/.pyre_configuration.local", json.loads(configuration_contents)
            ),
        ]
        with patch("subprocess.run", return_value=process):
            with patch("builtins.open", mock_open(read_data=configuration_contents)):
                configurations = upgrade.Configuration.gather_local_configurations(
                    arguments
                )
                self.assertEqual([], configurations)

    mock_completed_process = MagicMock()
    mock_completed_process.stdout.decode = MagicMock(return_value="[]")

    @patch("subprocess.call")
    @patch("subprocess.run", return_value=mock_completed_process)
    def test_get_errors(self, run, call) -> None:
        configuration = upgrade.Configuration("path", {})
        configuration.get_errors()
        call.assert_not_called()
        assert run.call_count == 1

        call.reset_mock()
        run.reset_mock()

        configuration.targets = ["//target/..."]
        configuration.get_errors()
        assert call.call_count == 1
        assert run.call_count == 1

    @patch("subprocess.call")
    @patch.object(upgrade.Configuration, "remove_version")
    @patch.object(upgrade.Configuration, "get_errors")
    @patch.object(upgrade.Configuration, "gather_local_configurations")
    @patch("%s.errors_from_stdin" % upgrade.__name__)
    @patch("%s.run_global_version_update" % upgrade.__name__)
    @patch("%s.fix" % upgrade.__name__)
    def test_upgrade_project(
        self,
        fix,
        run_global_version_update,
        errors_from_stdin,
        gather,
        get_errors,
        remove_version,
        subprocess,
    ) -> None:
        arguments = MagicMock()
        arguments.lint = False
        arguments.from_stdin = False
        gather.return_value = []
        upgrade.run_fixme_all(arguments)
        fix.assert_not_called()
        subprocess.assert_not_called()

        errors = [
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
        get_errors.return_value = errors
        configuration = upgrade.Configuration(
            "/root/local/.pyre_configuration.local", {"version": 123}
        )
        configuration.get_path()
        upgrade._upgrade_project(arguments, configuration, "/root")
        run_global_version_update.assert_not_called()
        fix.called_once_with(arguments, upgrade.sort_errors(errors))
        subprocess.assert_called_once_with(
            ["hg", "commit", "--message", upgrade._commit_message("local")]
        )

        # Test with lint
        subprocess.reset_mock()
        fix.reset_mock()
        arguments.from_stdin = False
        arguments.lint = True
        upgrade._upgrade_project(arguments, configuration, "/root")
        errors_from_stdin.assert_not_called()
        run_global_version_update.assert_not_called()
        fix.called_once_with(arguments, upgrade.sort_errors(errors))
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
            call().__bool__(),
            call(["arc", "lint", "--apply-patches", "--output", "none"]),
            call(["hg", "commit", "--message", upgrade._commit_message("local")]),
        ]
        subprocess.assert_has_calls(calls)

        # Test with from_stdin and lint
        subprocess.reset_mock()
        fix.reset_mock()
        get_errors.reset_mock()
        arguments.from_stdin = True
        arguments.lint = True
        errors_from_stdin.return_value = errors
        get_errors.return_value = errors
        upgrade._upgrade_project(arguments, configuration, "/root")
        # Called in the first round to get initial errors
        errors_from_stdin.assert_called()
        # Called in the second round to get new errors after applying lint.
        get_errors.assert_called_once()
        run_global_version_update.assert_not_called()
        fix.called_once_with(arguments, upgrade.sort_errors(errors))
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
            call().__bool__(),
            call(["arc", "lint", "--apply-patches", "--output", "none"]),
            call(["hg", "commit", "--message", upgrade._commit_message("local")]),
        ]
        subprocess.assert_has_calls(calls)

    @patch("subprocess.call")
    @patch.object(upgrade.Configuration, "gather_local_configurations")
    @patch.object(upgrade.Configuration, "find_project_configuration", return_value=".")
    @patch.object(upgrade.Configuration, "remove_version")
    @patch.object(upgrade.Configuration, "get_errors")
    @patch("%s.run_global_version_update" % upgrade.__name__)
    @patch("%s.fix" % upgrade.__name__)
    def test_run_fixme_all(
        self,
        fix,
        run_global_version_update,
        get_errors,
        remove_version,
        find_configuration,
        gather,
        subprocess,
    ) -> None:
        arguments = MagicMock()
        arguments.lint = False
        gather.return_value = [
            upgrade.Configuration("local/.pyre_configuration.local", {"version": 123})
        ]
        get_errors.return_value = []
        upgrade.run_fixme_all(arguments)
        run_global_version_update.assert_not_called()
        fix.assert_not_called()
        subprocess.assert_called_once_with(
            ["hg", "commit", "--message", upgrade._commit_message("local")]
        )

        fix.reset_mock()
        subprocess.reset_mock()
        errors = [
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
        get_errors.return_value = errors
        upgrade.run_fixme_all(arguments)
        run_global_version_update.assert_not_called()
        fix.called_once_with(arguments, upgrade.sort_errors(errors))
        subprocess.assert_called_once_with(
            ["hg", "commit", "--message", upgrade._commit_message("local")]
        )

        # Test configuraton with no version set
        fix.reset_mock()
        subprocess.reset_mock()
        gather.return_value = [
            upgrade.Configuration("local/.pyre_configuration.local", {})
        ]
        upgrade.run_fixme_all(arguments)
        fix.assert_not_called()
        subprocess.assert_not_called()

        # Test with given hash
        fix.reset_mock()
        subprocess.reset_mock()
        gather.return_value = [
            upgrade.Configuration("local/.pyre_configuration.local", {"version": 123})
        ]
        arguments.hash = "abc"
        arguments.submit = True
        upgrade.run_fixme_all(arguments)
        run_global_version_update.assert_called_once_with(arguments)
        fix.called_once_with(arguments, upgrade.sort_errors(errors))
        calls = [
            call(["hg", "commit", "--message", upgrade._commit_message("local")]),
            call(["jf", "submit", "--update-fields"]),
        ]
        subprocess.assert_has_calls(calls)

        # Test with linting
        fix.reset_mock()
        subprocess.reset_mock()
        run_global_version_update.reset_mock()
        arguments.lint = True
        upgrade.run_fixme_all(arguments)
        run_global_version_update.assert_called_once_with(arguments)
        fix.called_once_with(arguments, upgrade.sort_errors(errors))
        calls = [
            call(["hg", "commit", "--message", upgrade._commit_message("local")]),
            call(["jf", "submit", "--update-fields"]),
        ]
        subprocess.assert_has_calls(calls)

    @patch.object(pathlib.Path, "read_text")
    @patch.object(pathlib.Path, "write_text")
    def test_preserve_ast(self, path_write, path_read) -> None:
        mock_arguments = argparse.Namespace()
        # pyre-fixme[16]: `Namespace` has no attribute `max_line_length`.
        mock_arguments.max_line_length = 88
        # pyre-fixme[16]: `Namespace` has no attribute `truncate`.
        mock_arguments.truncate = True
        error_map = {7: [{"code": "6", "description": "Foo"}]}
        path_read.return_value = """
def foo(x: int) -> str:
    return str(x)

def bar(x: str) -> str:
    return f\'\'\'
    first line
    second {foo(x)}
    \'\'\'
        """
        upgrade.fix_file(mock_arguments, "test.py", error_map)

        path_write.assert_not_called()

    @patch("subprocess.run")
    @patch.object(upgrade.Configuration, "gather_local_configurations")
    @patch.object(upgrade.Configuration, "find_project_configuration")
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
        arguments.sandcastle = "sandcastle.json"
        arguments.push_blocking_only = False
        with patch("builtins.open", mock_open(read_data=command_json)):
            arguments.hash = "abc"
            gather.return_value = [
                upgrade.Configuration("a/.pyre_configuration.local", {"version": 123}),
                upgrade.Configuration("b/.pyre_configuration.local", {"version": 123}),
            ]
            upgrade.run_fixme_all(arguments)
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
                    "local/.pyre_configuration.local", {"version": 123}
                )
            ]
            upgrade.run_fixme_all(arguments)
            find_configuration.assert_not_called()
            run.assert_called_once_with(
                ["scutil", "create"],
                input=generate_sandcastle_command(None, ["local"], False),
            )


class FixmeSingleTest(unittest.TestCase):
    @patch("subprocess.call")
    @patch.object(upgrade.Configuration, "find_project_configuration", return_value=".")
    @patch.object(upgrade.Configuration, "remove_version")
    @patch.object(upgrade.Configuration, "get_errors")
    @patch("%s.fix" % upgrade.__name__)
    def test_run_fixme_single(
        self, fix, get_errors, remove_version, find_configuration, subprocess
    ) -> None:
        arguments = MagicMock()
        arguments.submit = True
        arguments.path = "local"
        get_errors.return_value = []
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            upgrade.run_fixme_single(arguments)
            fix.assert_not_called()
            subprocess.assert_not_called()

        configuration_contents = '{"version": 123}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            upgrade.run_fixme_single(arguments)
            fix.assert_not_called()
            calls = [
                call(["hg", "commit", "--message", upgrade._commit_message("local")]),
                call(["jf", "submit", "--update-fields"]),
            ]
            subprocess.assert_has_calls(calls)

        fix.reset_mock()
        subprocess.reset_mock()
        errors = [
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
        get_errors.return_value = errors
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            upgrade.run_fixme_single(arguments)
            fix.called_once_with(arguments, upgrade.sort_errors(errors))
            calls = [
                call(["hg", "commit", "--message", upgrade._commit_message("local")]),
                call(["jf", "submit", "--update-fields"]),
            ]
            call.assert_has_calls(calls)


class FixmeTest(unittest.TestCase):
    @patch("subprocess.call")
    @patch.object(pathlib.Path, "read_text")
    @patch("%s.errors_from_run" % upgrade.__name__)
    @patch("%s.errors_from_stdin" % upgrade.__name__)
    def test_run_fixme(
        self, stdin_errors, run_errors, path_read_text, subprocess
    ) -> None:
        arguments = MagicMock()
        arguments.comment = None
        arguments.max_line_length = 88
        arguments.run = False
        arguments.truncate = True

        stdin_errors.return_value = []
        run_errors.return_value = []
        upgrade.run_fixme(arguments)

        # Test single error.
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [1]: description",
                }
            ]
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments)
            path_write_text.assert_called_once_with(
                "# pyre-fixme[1]: description\n1\n2"
            )

        # Generated files.
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            errors = [
                {
                    "path": "path.py",
                    "line": 2,
                    "concise_description": "Error [1]: description",
                }
            ]
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = "# @" "generated\n1\n2\n"
            upgrade.run_fixme(arguments)
            path_write_text.assert_not_called()

        # Test single error with lint.
        arguments.run = True
        arguments.lint = True
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [1]: description",
                }
            ]
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments)
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
                call().__bool__(),
                call(["arc", "lint", "--apply-patches", "--output", "none"]),
            ]
            subprocess.assert_has_calls(calls)
        arguments.run = False
        arguments.lint = False

        # Test error with comment.
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [1]: description",
                }
            ]
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = "1\n2"
            arguments.comment = "T1234"
            upgrade.run_fixme(arguments)
            arguments.comment = None
            path_write_text.assert_called_once_with("# pyre-fixme[1]: T1234\n1\n2")

        # Test multiple errors and multiple lines.
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            errors = [
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
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments)
            path_write_text.assert_called_once_with(
                "# pyre-fixme[1]: description\n"
                "1\n"
                "# pyre-fixme[1]: description\n"
                "# pyre-fixme[2]: description\n"
                "2"
            )
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            errors = [
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
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments)
            path_write_text.assert_called_once_with(
                "1\n"
                "# pyre-fixme[10]: Description one.\n"
                "# pyre-fixme[11]: Description two.\n"
                "2"
            )
        arguments.max_line_length = 40
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            errors = [
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
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments)
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
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            errors = [
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
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments)
            path_write_text.assert_called_once_with(
                "# pyre-fixme[2]: Maximum characters.\n"
                "1\n"
                "# pyre-fixme[2]: Too many\n"
                "#  characters.\n"
                "2"
            )

        arguments.max_line_length = 40
        arguments.truncate = False
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            errors = [
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
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments)
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
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            errors = [
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
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments)
            path_write_text.has_calls(
                [
                    call("# pyre-fixme[1]: description\n1\n2"),
                    call("1\n#pyre-fixme[2]: description\n2"),
                ]
            )

        # Test removal of extraneous ignore.
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous ignore",
                }
            ]
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = "  # pyre-ignore[0]: [1, 2, 3]\n2"
            upgrade.run_fixme(arguments)
            arguments.comment = None
            path_write_text.assert_called_once_with("2")

        # Test removal of extraneous ignore.
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            arguments.max_line_length = 30
            errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous",
                }
            ]
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = (
                "  # pyre-ignore[0]: [1, 2, 3]\n#  continuation comment\n2"
            )
            upgrade.run_fixme(arguments)
            arguments.comment = None
            arguments.truncate = True
            path_write_text.assert_called_once_with("2")

        # We don't remove legitimate comments.
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            arguments.max_line_length = 30
            errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous",
                }
            ]
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = (
                "  # pyre-ignore[0]: [1, 2, 3]\n# user comment\n2"
            )
            upgrade.run_fixme(arguments)
            arguments.comment = None
            arguments.truncate = True
            path_write_text.assert_called_once_with("# user comment\n2")

        with patch.object(pathlib.Path, "write_text") as path_write_text:
            arguments.max_line_length = 30
            errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous ignore that's "
                    "quite long",
                }
            ]
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = (
                "  # pyre-ignore[0]:\n#  comment that doesn't fit on one line\n"
                "# pyre-ignore[1]:\n2"
            )
            upgrade.run_fixme(arguments)
            arguments.comment = None
            arguments.truncate = True
            path_write_text.assert_called_once_with("# pyre-ignore[1]:\n2")

        with patch.object(pathlib.Path, "write_text") as path_write_text:
            errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous ignore",
                }
            ]
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = "# pyre-fixme[1]\n# pyre-fixme[2]\n2"
            upgrade.run_fixme(arguments)
            arguments.comment = None
            path_write_text.assert_called_once_with("# pyre-fixme[2]\n2")

        # Test removal of extraneous ignore (trailing comment).
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "concise_description": "Error [0]: extraneous ignore",
                }
            ]
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = "1# pyre-ignore[0]: [1, 2, 3]\n2"
            upgrade.run_fixme(arguments)
            arguments.comment = None
            path_write_text.assert_called_once_with("1\n2")

        # Test long lines.
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            arguments_short = MagicMock()
            arguments_short.comment = None
            arguments_short.max_line_length = 35
            arguments_short.run = False

            errors = [
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
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = "line = 1\nline = 2\nline = 3"
            upgrade.run_fixme(arguments_short)
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
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "description": "Error [1]: description",
                    "concise_description": "",
                }
            ]
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments)
            path_write_text.assert_called_once_with(
                "# pyre-fixme[1]: description\n1\n2"
            )

        # Ensure that we prefer concise descriptions.
        with patch.object(pathlib.Path, "write_text") as path_write_text:
            errors = [
                {
                    "path": "path.py",
                    "line": 1,
                    "description": "Error [1]: description",
                    "concise_description": "Error[1]: Concise.",
                }
            ]
            stdin_errors.return_value = errors
            run_errors.return_value = errors
            path_read_text.return_value = "1\n2"
            upgrade.run_fixme(arguments)
            path_write_text.assert_called_once_with("# pyre-fixme[1]: Concise.\n1\n2")


class DecodeTest(unittest.TestCase):
    def test_json_to_errors(self) -> None:
        with patch.object(upgrade.LOG, "error") as mock_error:
            self.assertEqual(
                upgrade.json_to_errors('[{ "key": "value" }]'), [{"key": "value"}]
            )
            mock_error.assert_not_called()
            mock_error.reset_mock()

            self.assertEqual(upgrade.json_to_errors(None), [])
            mock_error.assert_called_once_with(
                "Recevied no input."
                "If piping from `pyre check` be sure to use `--output=json`."
            )
            mock_error.reset_mock()

            self.assertEqual(upgrade.json_to_errors('[{ "key": "value" }'), [])
            mock_error.assert_called_once_with(
                "Recevied invalid JSON as input."
                "If piping from `pyre check` be sure to use `--output=json`."
            )
            mock_error.reset_mock()


class UpdateGlobalVersionTest(unittest.TestCase):
    @patch("subprocess.call")
    @patch.object(
        upgrade.Configuration, "find_project_configuration", return_value="/root"
    )
    @patch.object(
        upgrade.Configuration,
        "gather_local_configurations",
        return_value=[
            upgrade.Configuration(
                "/root/a/.pyre_configuration.local", {"push_blocking": False}
            ),
            upgrade.Configuration(
                "/root/b/.pyre_configuration.local", {"push_blocking": True}
            ),
        ],
    )
    @patch("builtins.open")
    def test_run_global_version_update(
        self,
        open_mock,
        gather_local_configurations,
        find_project_configuration,
        subprocess,
    ) -> None:
        arguments = MagicMock()
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

            upgrade.run_global_version_update(arguments)
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
            subprocess.assert_called_once_with(
                [
                    "hg",
                    "commit",
                    "--message",
                    upgrade._commit_message(
                        "global configuration",
                        summary_override="Automatic upgrade to hash `abcd`",
                    ),
                ]
            )
        # Push blocking argument: Since the push blocking only argument is only used when
        # gathering local configurations (mocked here), this is a no-op. Documents it.
        subprocess.reset_mock()
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

            upgrade.run_global_version_update(arguments)
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
            calls = [
                call(
                    [
                        "hg",
                        "commit",
                        "--message",
                        upgrade._commit_message(
                            "global configuration",
                            summary_override="Automatic upgrade to hash `abcd`",
                        ),
                    ]
                ),
                call(["jf", "submit", "--update-fields"]),
            ]
            subprocess.assert_has_calls(calls)

        # paths passed from arguments will override the local configuration list
        # Therefore, we only read the first json configuration.
        subprocess.reset_mock()
        arguments.paths = ["foo/bar"]
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

            upgrade.run_global_version_update(arguments)
            dump.assert_has_calls(
                [call({"version": "abcd"}, mocks[1], indent=2, sort_keys=True)]
            )
            subprocess.assert_has_calls([])


class FilterErrorTest(unittest.TestCase):
    def test_filter_errors(self) -> None:
        arguments = MagicMock()
        arguments.only_fix_error_code = 44
        errors = [
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
        self.assertEqual(upgrade.filter_errors(arguments, errors), [])

        arguments.only_fix_error_code = 7
        self.assertEqual(upgrade.filter_errors(arguments, errors), errors)

        arguments.only_fix_error_code = None
        self.assertEqual(upgrade.filter_errors(arguments, errors), errors)
