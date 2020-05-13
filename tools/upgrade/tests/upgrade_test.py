# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import subprocess
import unittest
from pathlib import Path
from unittest.mock import MagicMock, call, mock_open, patch

from .. import errors, upgrade
from ..commands import command
from ..repository import Repository
from ..upgrade import (
    ErrorSuppressingCommand,
    FixmeAll,
    FixmeSingle,
    FixmeTargets,
    GlobalVersionUpdate,
    ProjectErrorSuppressingCommand,
)


repository = Repository()


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
            return lists_equal

        configurations_string = ""
        process.stdout = configurations_string.encode()
        with patch("subprocess.run", return_value=process):
            configurations = upgrade.Configuration.gather_local_configurations()
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
                configurations = upgrade.Configuration.gather_local_configurations()
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
                configurations = upgrade.Configuration.gather_local_configurations()
                self.assertTrue(
                    configuration_lists_equal(expected_configurations, configurations)
                )

    mock_completed_process = MagicMock()
    mock_completed_process.stdout.decode = MagicMock(return_value="[]")

    @patch("subprocess.run")
    @patch.object(upgrade.Configuration, "write")
    @patch.object(upgrade.Configuration, "remove_version")
    @patch.object(upgrade.Configuration, "get_errors")
    @patch.object(upgrade.Configuration, "gather_local_configurations")
    @patch(f"{command.__name__}.Errors.from_stdin")
    @patch.object(upgrade.GlobalVersionUpdate, "run")
    @patch.object(upgrade.ErrorSuppressingCommand, "_suppress_errors")
    @patch(f"{upgrade.__name__}.Repository.submit_changes")
    @patch(f"{upgrade.__name__}.Repository.format")
    def test_upgrade_project(
        self,
        repository_format,
        submit_changes,
        suppress_errors,
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
        arguments.error_source = "generate"
        arguments.upgrade_version = True
        arguments.no_commit = False
        gather.return_value = []
        FixmeAll(arguments, repository).run()
        suppress_errors.assert_not_called()
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
        ProjectErrorSuppressingCommand(
            arguments, repository
        )._suppress_errors_in_project(configuration, Path("/root"))
        run_global_version_update.assert_not_called()
        suppress_errors.assert_called_once_with(pyre_errors)
        submit_changes.assert_called_once_with(
            commit=True, submit=False, title="Update pyre version for local"
        )

        # Test with lint
        submit_changes.reset_mock()
        suppress_errors.reset_mock()
        arguments.error_source = "generate"
        arguments.lint = True
        ProjectErrorSuppressingCommand(
            arguments, repository
        )._suppress_errors_in_project(configuration, Path("/root"))
        errors_from_stdin.assert_not_called()
        run_global_version_update.assert_not_called()
        calls = [call(pyre_errors), call(pyre_errors)]
        suppress_errors.assert_has_calls(calls)
        submit_changes.assert_called_once_with(
            commit=True, submit=False, title="Update pyre version for local"
        )

        # Test with from_stdin and lint
        repository_format.return_value = True
        submit_changes.reset_mock()
        suppress_errors.reset_mock()
        get_errors.reset_mock()
        arguments.error_source = "stdin"
        arguments.lint = True
        arguments.upgrade_version = False
        errors_from_stdin.return_value = pyre_errors
        get_errors.return_value = pyre_errors
        ProjectErrorSuppressingCommand(
            arguments, repository
        )._suppress_errors_in_project(configuration, Path("/root"))
        # Called in the first round to get initial errors
        errors_from_stdin.assert_called()
        # Called in the second round to get new errors after applying lint.
        get_errors.assert_called_once()
        run_global_version_update.assert_not_called()
        calls = [call(pyre_errors), call(pyre_errors)]
        suppress_errors.assert_has_calls(calls)
        submit_changes.assert_called_once_with(
            commit=True, submit=False, title="Suppress pyre errors for local"
        )

    @patch("subprocess.run")
    @patch.object(upgrade.Configuration, "gather_local_configurations")
    @patch.object(
        upgrade.Configuration, "find_project_configuration", return_value=Path(".")
    )
    @patch.object(upgrade.Configuration, "write")
    @patch.object(upgrade.Configuration, "remove_version")
    @patch.object(upgrade.Configuration, "get_errors")
    @patch.object(upgrade.GlobalVersionUpdate, "run")
    @patch.object(upgrade.ErrorSuppressingCommand, "_suppress_errors")
    @patch(f"{upgrade.__name__}.Repository.submit_changes")
    def test_run_fixme_all(
        self,
        submit_changes,
        suppress_errors,
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
        arguments.error_source = "generate"
        arguments.upgrade_version = True
        arguments.no_commit = False
        gather.return_value = [
            upgrade.Configuration(
                Path("local/.pyre_configuration.local"), {"version": 123}
            )
        ]
        get_errors.return_value = []
        FixmeAll(arguments, repository).run()
        run_global_version_update.assert_not_called()
        suppress_errors.assert_not_called()
        submit_changes.assert_called_once_with(
            commit=True, submit=False, title="Update pyre version for local"
        )

        suppress_errors.reset_mock()
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
        FixmeAll(arguments, repository).run()
        run_global_version_update.assert_not_called()
        suppress_errors.assert_called_once_with(pyre_errors)
        submit_changes.assert_called_once_with(
            commit=True, submit=False, title="Update pyre version for local"
        )

        # Test configuraton with no version set
        suppress_errors.reset_mock()
        submit_changes.reset_mock()
        gather.return_value = [
            upgrade.Configuration(Path("local/.pyre_configuration.local"), {})
        ]
        FixmeAll(arguments, repository).run()
        suppress_errors.assert_not_called()
        submit_changes.assert_not_called()

        arguments.upgrade_version = False
        suppress_errors.reset_mock()
        submit_changes.reset_mock()
        FixmeAll(arguments, repository).run()
        suppress_errors.assert_called_once_with(pyre_errors)
        submit_changes.assert_called_once_with(
            commit=True, submit=False, title="Suppress pyre errors for local"
        )

        # Test with given hash
        arguments.upgrade_version = True
        suppress_errors.reset_mock()
        submit_changes.reset_mock()
        gather.return_value = [
            upgrade.Configuration(
                Path("local/.pyre_configuration.local"), {"version": 123}
            )
        ]
        arguments.hash = "abc"
        arguments.submit = True
        FixmeAll(arguments, repository).run()
        run_global_version_update.assert_not_called()
        suppress_errors.assert_called_once_with(pyre_errors)
        submit_changes.assert_called_once_with(
            commit=True, submit=True, title="Update pyre version for local"
        )


class FixmeSingleTest(unittest.TestCase):
    @patch("subprocess.run")
    @patch.object(
        upgrade.Configuration, "find_project_configuration", return_value=Path(".")
    )
    @patch.object(upgrade.Configuration, "write")
    @patch.object(upgrade.Configuration, "remove_version")
    @patch.object(upgrade.Configuration, "get_errors")
    @patch.object(upgrade.ErrorSuppressingCommand, "_suppress_errors")
    @patch(f"{upgrade.__name__}.Repository.submit_changes")
    def test_run_fixme_single(
        self,
        submit_changes,
        suppress_errors,
        get_errors,
        remove_version,
        configuration_write,
        find_configuration,
        subprocess,
    ) -> None:
        arguments = MagicMock()
        arguments.submit = True
        arguments.path = Path("local")
        arguments.error_source = "generate"
        arguments.lint = False
        arguments.no_commit = False
        get_errors.return_value = []
        configuration_contents = '{"targets":[]}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            FixmeSingle(arguments, repository).run()
            suppress_errors.assert_not_called()
            submit_changes.assert_not_called()

        configuration_contents = '{"version": 123}'
        with patch("builtins.open", mock_open(read_data=configuration_contents)):
            FixmeSingle(arguments, repository).run()
            suppress_errors.assert_not_called()
            submit_changes.assert_called_once_with(
                commit=True, submit=True, title="Update pyre version for local"
            )

        suppress_errors.reset_mock()
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
            FixmeSingle(arguments, repository).run()
            suppress_errors.assert_called_once_with(pyre_errors)
            submit_changes.assert_called_once_with(
                commit=True, submit=True, title="Update pyre version for local"
            )


class FixmeTargetsTest(unittest.TestCase):
    @patch("subprocess.run")
    @patch.object(
        upgrade.Configuration,
        "find_project_configuration",
        return_value=Path(".pyre_configuration"),
    )
    @patch.object(upgrade.FixmeTargets, "_run_fixme_targets_file")
    @patch(f"{upgrade.__name__}.Repository.submit_changes")
    def test_fixme_targets(
        self, submit_changes, fix_file, find_configuration, subprocess
    ) -> None:
        arguments = MagicMock()
        arguments.subdirectory = None
        arguments.no_commit = False
        grep_return = MagicMock()
        grep_return.returncode = 1
        grep_return.stderr = b"stderr"
        subprocess.return_value = grep_return
        FixmeTargets(arguments, repository).run()
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
        FixmeTargets(arguments, repository).run()
        fix_file.assert_called_once_with(Path("."), "a/b", ["derp", "herp", "merp"])
        submit_changes.assert_called_once_with(
            commit=True,
            submit=arguments.submit,
            title="Upgrade pyre version for . (TARGETS)",
        )

        # Test subdirectory
        subprocess.reset_mock()
        fix_file.reset_mock()
        submit_changes.reset_mock()
        arguments.subdirectory = "derp"
        FixmeTargets(arguments, repository).run()
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
        fix_file.assert_called_once_with(Path("."), "a/b", ["derp", "herp", "merp"])
        submit_changes.assert_called_once_with(
            commit=True,
            submit=arguments.submit,
            title="Upgrade pyre version for derp (TARGETS)",
        )

    @patch("subprocess.run")
    @patch.object(upgrade.ErrorSuppressingCommand, "_suppress_errors")
    def test_run_fixme_targets_file(self, suppress_errors, subprocess) -> None:
        arguments = MagicMock()
        arguments.subdirectory = None
        arguments.no_commit = False
        arguments.lint = False
        buck_return = MagicMock()
        buck_return.returncode = 1
        buck_return.stderr = b"stderr"
        subprocess.return_value = buck_return
        FixmeTargets(arguments, repository)._run_fixme_targets_file(
            Path("."), "a/b", ["derp", "herp"]
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
        suppress_errors.assert_not_called()

        buck_return.returncode = 0
        subprocess.return_value = buck_return
        FixmeTargets(arguments, repository)._run_fixme_targets_file(
            Path("."), "a/b", ["derp", "herp"]
        )
        suppress_errors.assert_not_called()

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
        FixmeTargets(arguments, repository)._run_fixme_targets_file(
            Path("."), "a/b", ["derp", "herp"]
        )
        suppress_errors.assert_called_once_with(expected_errors)

        # Test fallback to type check targets with modified names
        subprocess.reset_mock()
        suppress_errors.reset_mock()
        failed_buck_return = MagicMock()
        failed_buck_return.returncode = 5
        failed_buck_return.stdout = b""
        buck_query_return = MagicMock()
        buck_query_return.stdout = b"//target/to:retry-pyre-typecheck"
        subprocess.side_effect = [failed_buck_return, buck_query_return, buck_return]
        FixmeTargets(arguments, repository)._run_fixme_targets_file(
            Path("."), "a/b", ["derp"]
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
        suppress_errors.assert_called_once_with(expected_errors)

        subprocess.reset_mock()
        suppress_errors.reset_mock()
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
        FixmeTargets(arguments, repository)._run_fixme_targets_file(
            Path("."), "a/b", ["derp"]
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
        suppress_errors.assert_called_once_with(expected_errors)


class UpdateGlobalVersionTest(unittest.TestCase):
    @patch("subprocess.run")
    @patch(f"{upgrade.__name__}.Repository.submit_changes")
    @patch.object(
        upgrade.Configuration, "find_project_configuration", return_value="/root"
    )
    @patch.object(
        upgrade.Configuration,
        "gather_local_configurations",
        return_value=[
            upgrade.Configuration(
                Path("/root/a/.pyre_configuration.local"), {"use_buck_builder": False}
            ),
            upgrade.Configuration(
                Path("/root/b/.pyre_configuration.local"), {"use_buck_builder": True}
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
        with patch("json.dump") as dump:
            mocks = [
                mock_open(read_data='{"version": "old"}').return_value,
                mock_open(read_data="{}").return_value,
                mock_open(read_data='{"use_buck_builder": false}').return_value,
                mock_open(read_data="{}").return_value,
                mock_open(read_data='{"use_buck_builder": true}').return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks

            GlobalVersionUpdate(arguments, repository).run()
            dump.assert_has_calls(
                [
                    call({"version": "abcd"}, mocks[1], indent=2, sort_keys=True),
                    call(
                        {"use_buck_builder": False, "version": "old"},
                        mocks[3],
                        indent=2,
                        sort_keys=True,
                    ),
                    call(
                        {"use_buck_builder": True, "version": "old"},
                        mocks[5],
                        indent=2,
                        sort_keys=True,
                    ),
                ]
            )
            submit_changes.assert_called_once_with(
                commit=True,
                submit=False,
                title="Update pyre global configuration version",
                summary="Automatic upgrade to hash `abcd`",
                ignore_failures=True,
            )

        # paths passed from arguments will override the local configuration list
        # Therefore, we only read the first json configuration.
        subprocess.reset_mock()
        arguments.paths = [Path("foo/bar")]
        arguments.submit = False
        with patch("json.dump") as dump:
            mocks = [
                mock_open(read_data='{"version": "old"}').return_value,
                mock_open(read_data="{}").return_value,
                mock_open(read_data="{}").return_value,
                mock_open(read_data="{}").return_value,
                mock_open(read_data="{}").return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks

            GlobalVersionUpdate(arguments, repository).run()
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
