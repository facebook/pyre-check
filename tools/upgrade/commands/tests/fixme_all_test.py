# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import subprocess
import unittest
from pathlib import Path
from unittest.mock import MagicMock, call, mock_open, patch

from ... import upgrade
from ...repository import Repository
from .. import command
from ..command import ErrorSource, ProjectErrorSuppressingCommand
from ..fixme_all import Configuration, ErrorSuppressingCommand, FixmeAll


repository = Repository()


class FixmeAllTest(unittest.TestCase):
    @patch.object(Configuration, "find_project_configuration", return_value=None)
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
            configurations = Configuration.gather_local_configurations()
            self.assertEqual([], configurations)

        configurations_string = "path/to/.pyre_configuration.local"
        process.stdout = configurations_string.encode()
        configuration_contents = '{"targets":[]}'
        expected_configurations = [
            Configuration(
                Path("path/to/.pyre_configuration.local"),
                json.loads(configuration_contents),
            )
        ]
        with patch("subprocess.run", return_value=process) as subprocess_run:
            with patch("builtins.open", mock_open(read_data=configuration_contents)):
                configurations = Configuration.gather_local_configurations()
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
            Configuration(
                Path("a/.pyre_configuration.local"), json.loads(configuration_contents)
            ),
            Configuration(
                Path("b/.pyre_configuration.local"), json.loads(configuration_contents)
            ),
        ]
        with patch("subprocess.run", return_value=process):
            with patch("builtins.open", mock_open(read_data=configuration_contents)):
                configurations = Configuration.gather_local_configurations()
                self.assertTrue(
                    configuration_lists_equal(expected_configurations, configurations)
                )

    mock_completed_process = MagicMock()
    mock_completed_process.stdout.decode = MagicMock(return_value="[]")

    @patch("subprocess.run")
    @patch.object(Configuration, "write")
    @patch.object(Configuration, "remove_version")
    @patch.object(Configuration, "get_errors")
    @patch.object(Configuration, "gather_local_configurations")
    @patch(f"{command.__name__}.Errors.from_stdin")
    @patch.object(upgrade.GlobalVersionUpdate, "run")
    @patch.object(ErrorSuppressingCommand, "_suppress_errors")
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
        arguments.error_source = ErrorSource.GENERATE
        arguments.upgrade_version = True
        arguments.no_commit = False
        gather.return_value = []
        FixmeAll.from_arguments(arguments, repository).run()
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
        configuration = Configuration(
            Path("/root/local/.pyre_configuration.local"), {"version": 123}
        )
        configuration.get_path()
        ProjectErrorSuppressingCommand(
            command.CommandArguments.from_arguments(arguments),
            repository=repository,
            only_fix_error_code=None,
            upgrade_version=arguments.upgrade_version,
            error_source=arguments.error_source,
        )._suppress_errors_in_project(configuration, Path("/root"))
        run_global_version_update.assert_not_called()
        suppress_errors.assert_called_once_with(pyre_errors)
        submit_changes.assert_called_once_with(
            commit=True, submit=False, title="Update pyre version for local"
        )

        # Test with lint
        submit_changes.reset_mock()
        suppress_errors.reset_mock()
        arguments.error_source = ErrorSource.GENERATE
        arguments.lint = True
        ProjectErrorSuppressingCommand(
            command.CommandArguments.from_arguments(arguments),
            repository=repository,
            only_fix_error_code=None,
            upgrade_version=arguments.upgrade_version,
            error_source=arguments.error_source,
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
        arguments.error_source = ErrorSource.STDIN
        arguments.lint = True
        arguments.upgrade_version = False
        errors_from_stdin.return_value = pyre_errors
        get_errors.return_value = pyre_errors
        ProjectErrorSuppressingCommand(
            command.CommandArguments.from_arguments(arguments),
            repository=repository,
            only_fix_error_code=None,
            upgrade_version=arguments.upgrade_version,
            error_source=arguments.error_source,
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
    @patch.object(Configuration, "gather_local_configurations")
    @patch.object(Configuration, "find_project_configuration", return_value=Path("."))
    @patch.object(Configuration, "write")
    @patch.object(Configuration, "remove_version")
    @patch.object(Configuration, "get_errors")
    @patch.object(upgrade.GlobalVersionUpdate, "run")
    @patch.object(ErrorSuppressingCommand, "_suppress_errors")
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
        arguments.error_source = ErrorSource.GENERATE
        arguments.upgrade_version = True
        arguments.no_commit = False
        gather.return_value = [
            Configuration(Path("local/.pyre_configuration.local"), {"version": 123})
        ]
        get_errors.return_value = []
        FixmeAll.from_arguments(arguments, repository).run()
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
        FixmeAll.from_arguments(arguments, repository).run()
        run_global_version_update.assert_not_called()
        suppress_errors.assert_called_once_with(pyre_errors)
        submit_changes.assert_called_once_with(
            commit=True, submit=False, title="Update pyre version for local"
        )

        # Test configuraton with no version set
        suppress_errors.reset_mock()
        submit_changes.reset_mock()
        gather.return_value = [
            Configuration(Path("local/.pyre_configuration.local"), {})
        ]
        FixmeAll.from_arguments(arguments, repository).run()
        suppress_errors.assert_not_called()
        submit_changes.assert_not_called()

        arguments.upgrade_version = False
        suppress_errors.reset_mock()
        submit_changes.reset_mock()
        FixmeAll.from_arguments(arguments, repository).run()
        suppress_errors.assert_called_once_with(pyre_errors)
        submit_changes.assert_called_once_with(
            commit=True, submit=False, title="Suppress pyre errors for local"
        )

        # Test with given hash
        arguments.upgrade_version = True
        suppress_errors.reset_mock()
        submit_changes.reset_mock()
        gather.return_value = [
            Configuration(Path("local/.pyre_configuration.local"), {"version": 123})
        ]
        arguments.hash = "abc"
        arguments.submit = True
        FixmeAll.from_arguments(arguments, repository).run()
        run_global_version_update.assert_not_called()
        suppress_errors.assert_called_once_with(pyre_errors)
        submit_changes.assert_called_once_with(
            commit=True, submit=True, title="Update pyre version for local"
        )
