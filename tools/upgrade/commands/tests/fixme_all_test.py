# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import subprocess
import unittest
from pathlib import Path
from unittest.mock import call, MagicMock, mock_open, patch

from ... import upgrade
from ...repository import Repository
from .. import command
from ..command import ErrorSource, ErrorSuppressingCommand
from ..fixme_all import Configuration, FixmeAll


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
    @patch.object(ErrorSuppressingCommand, "_apply_suppressions")
    @patch(f"{upgrade.__name__}.Repository.format")
    def test_upgrade_project(
        self,
        repository_format,
        apply_suppressions,
        run_global_version_update,
        errors_from_stdin,
        gather,
        get_errors,
        remove_version,
        configuration_write,
        subprocess,
    ) -> None:
        arguments = MagicMock()
        arguments.lint = False
        arguments.error_source = ErrorSource.GENERATE
        arguments.upgrade_version = True
        arguments.no_commit = False
        gather.return_value = []
        FixmeAll.from_arguments(arguments, repository).run()
        apply_suppressions.assert_not_called()

        pyre_errors = [
            {
                "line": 2,
                "column": 4,
                "path": "local.py",
                "code": 7,
                "name": "Kind",
                "concise_description": "Error",
                "ignore_error": False,
                "external_to_global_root": False,
            }
        ]
        get_errors.return_value = pyre_errors
        configuration = Configuration(
            Path("/root/local/.pyre_configuration.local"), {"version": 123}
        )
        configuration.get_path()
        ErrorSuppressingCommand(
            command.CommandArguments.from_arguments(arguments), repository=repository
        )._get_and_suppress_errors(
            configuration,
            upgrade_version=arguments.upgrade_version,
            error_source=arguments.error_source,
        )
        run_global_version_update.assert_not_called()
        apply_suppressions.assert_called_once_with(pyre_errors)

        # Test with lint
        apply_suppressions.reset_mock()
        arguments.error_source = ErrorSource.GENERATE
        arguments.lint = True
        ErrorSuppressingCommand(
            command.CommandArguments.from_arguments(arguments), repository=repository
        )._get_and_suppress_errors(
            configuration,
            upgrade_version=arguments.upgrade_version,
            error_source=arguments.error_source,
        )
        errors_from_stdin.assert_not_called()
        run_global_version_update.assert_not_called()
        calls = [call(pyre_errors), call(pyre_errors)]
        apply_suppressions.assert_has_calls(calls)

        # Test with from_stdin and lint
        repository_format.return_value = True
        apply_suppressions.reset_mock()
        get_errors.reset_mock()
        arguments.error_source = ErrorSource.STDIN
        arguments.lint = True
        arguments.upgrade_version = False
        errors_from_stdin.return_value = pyre_errors
        get_errors.return_value = pyre_errors
        ErrorSuppressingCommand(
            command.CommandArguments.from_arguments(arguments), repository=repository
        )._get_and_suppress_errors(
            configuration,
            upgrade_version=arguments.upgrade_version,
            error_source=arguments.error_source,
        )
        # Called in the first round to get initial errors
        errors_from_stdin.assert_called()
        # Called in the second round to get new errors after applying lint.
        get_errors.assert_called_once()
        run_global_version_update.assert_not_called()
        calls = [call(pyre_errors), call(pyre_errors)]
        apply_suppressions.assert_has_calls(calls)

    @patch("subprocess.run")
    @patch.object(Configuration, "gather_local_configurations")
    @patch.object(Configuration, "find_project_configuration", return_value=Path("/"))
    @patch.object(Configuration, "write")
    @patch.object(Configuration, "remove_version")
    @patch.object(Configuration, "get_errors")
    @patch.object(upgrade.GlobalVersionUpdate, "run")
    @patch.object(ErrorSuppressingCommand, "_apply_suppressions")
    @patch(f"{upgrade.__name__}.Repository.commit_changes")
    def test_run_fixme_all(
        self,
        commit_changes,
        apply_suppressions,
        run_global_version_update,
        get_errors,
        remove_version,
        configuration_write,
        find_configuration,
        gather,
        subprocess,
    ) -> None:
        arguments = MagicMock()
        arguments.lint = False
        arguments.error_source = ErrorSource.GENERATE
        arguments.upgrade_version = True
        arguments.no_commit = False
        gather.return_value = [
            Configuration(Path("/local/.pyre_configuration.local"), {"version": 123})
        ]
        get_errors.return_value = []
        FixmeAll.from_arguments(arguments, repository).run()
        run_global_version_update.assert_not_called()
        apply_suppressions.assert_not_called()
        commit_changes.assert_called_once_with(
            commit=True, title="Update pyre version for local"
        )

        apply_suppressions.reset_mock()
        commit_changes.reset_mock()
        pyre_errors = [
            {
                "line": 2,
                "column": 4,
                "path": "local.py",
                "code": 7,
                "name": "Kind",
                "concise_description": "Error",
                "ignore_error": False,
                "external_to_global_root": False,
            }
        ]
        get_errors.return_value = pyre_errors
        FixmeAll.from_arguments(arguments, repository).run()
        run_global_version_update.assert_not_called()
        apply_suppressions.assert_called_once_with(pyre_errors)
        commit_changes.assert_called_once_with(
            commit=True, title="Update pyre version for local"
        )

        # Test configuraton with no version set
        apply_suppressions.reset_mock()
        commit_changes.reset_mock()
        gather.return_value = [
            Configuration(Path("/local/.pyre_configuration.local"), {})
        ]
        FixmeAll.from_arguments(arguments, repository).run()
        apply_suppressions.assert_not_called()
        commit_changes.assert_called_once_with(
            commit=True, title="Update pyre version for local"
        )

        arguments.upgrade_version = False
        apply_suppressions.reset_mock()
        commit_changes.reset_mock()
        FixmeAll.from_arguments(arguments, repository).run()
        apply_suppressions.assert_called_once_with(pyre_errors)
        commit_changes.assert_called_once_with(
            commit=True, title="Suppress pyre errors for local"
        )

        # Test with given hash
        arguments.upgrade_version = True
        apply_suppressions.reset_mock()
        commit_changes.reset_mock()
        gather.return_value = [
            Configuration(Path("/local/.pyre_configuration.local"), {"version": 123})
        ]
        arguments.hash = "abc"
        FixmeAll.from_arguments(arguments, repository).run()
        run_global_version_update.assert_not_called()
        apply_suppressions.assert_called_once_with(pyre_errors)
        commit_changes.assert_called_once_with(
            commit=True, title="Update pyre version for local"
        )
