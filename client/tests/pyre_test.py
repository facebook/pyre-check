# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import argparse
import shutil
import unittest
from pathlib import Path
from unittest.mock import MagicMock, call, patch

from .. import commands, configuration, pyre, recently_used_configurations, statistics
from ..commands import ExitCode
from ..exceptions import EnvironmentException
from ..pyre import _set_default_command
from .mocks import mock_configuration, mock_incremental_command


def _get_arguments() -> argparse.Namespace:
    arguments = argparse.Namespace()
    arguments.local_configuration = None
    arguments.version = False
    arguments.debug = False
    arguments.sequential = False
    arguments.strict = False
    arguments.additional_check = []
    arguments.show_error_traces = False
    arguments.output = "text"
    arguments.enable_profiling = False
    arguments.enable_memory_profiling = False
    arguments.noninteractive = True
    arguments.logging_sections = None
    arguments.log_identifier = None
    arguments.logger = None
    arguments.formatter = None
    arguments.targets = ["//foo"]
    arguments.use_buck_builder = False
    arguments.use_buck_source_database = True
    arguments.source_directories = []
    arguments.filter_directory = []
    arguments.buck_mode = None
    arguments.no_saved_state = False
    arguments.search_path = []
    arguments.binary = None
    arguments.buck_builder_binary = None
    arguments.exclude = []
    arguments.typeshed = None
    arguments.save_initial_state_to = None
    arguments.load_initial_state_from = None
    arguments.changed_files_path = None
    arguments.saved_state_project = None
    arguments.dot_pyre_directory = None
    arguments.features = None
    return arguments


class PyreTest(unittest.TestCase):
    @patch.object(commands.Start, "run")
    @patch.object(commands.Persistent, "run_null_server")
    def test_persistent_integration(
        self, run_null_server: MagicMock, run_start: MagicMock
    ) -> None:
        run_start.side_effect = commands.ClientException
        self.assertEqual(pyre.main(["persistent"]), 2)
        run_null_server.assert_not_called()

        run_start.side_effect = EnvironmentException
        self.assertEqual(pyre.main(["persistent"]), 0)
        run_null_server.assert_has_calls([call(timeout=3600 * 12)])

    # pyre-fixme[56]: Argument `shutil` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(shutil, "which", return_value=True)
    def test_set_default_command__watchman_exists(self, which: MagicMock) -> None:
        arguments = argparse.Namespace()
        _set_default_command(arguments)
        self.assertEqual(arguments.command, commands.Incremental.from_arguments)

    # pyre-fixme[56]: Argument `shutil` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(shutil, "which", return_value=False)
    def test_set_default_command__no_watchman(self, which: MagicMock) -> None:
        arguments = argparse.Namespace()
        _set_default_command(arguments)
        self.assertEqual(arguments.command, commands.Check.from_arguments)

    # pyre-fixme[56]: Argument `tools.pyre.client.statistics` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(statistics, "log")
    def test_log_statistics(self, statistics_log: MagicMock) -> None:
        arguments = argparse.Namespace()
        test_configuration = configuration.Configuration(
            project_root="irrelevant", dot_pyre_directory=Path(".pyre"), logger="logger"
        )
        command = mock_incremental_command(test_configuration)
        pyre._log_statistics(command, arguments, 0.0, "foo", "bar", 42)
        statistics_log.assert_called_once()

    # pyre-fixme[56]: Argument `tools.pyre.client.statistics` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(statistics, "log")
    def test_log_statistics__should_rerun(self, statistics_log: MagicMock) -> None:
        arguments = argparse.Namespace()
        test_configuration = configuration.Configuration(
            project_root="irrelevant", dot_pyre_directory=Path(".pyre"), logger="logger"
        )
        command = mock_incremental_command(test_configuration)
        pyre._log_statistics(
            command, arguments, 0.0, "foo", "bar", 42, should_log=False
        )
        statistics_log.assert_not_called()

    @patch.object(
        recently_used_configurations,
        "prompt_user_for_local_root",
        return_value="foo/bar",
    )
    @patch.object(
        recently_used_configurations.Cache,
        "get_all_items",
        return_value=["foo/bar", "baz"],
    )
    # pyre-fixme[56]: Argument `tools.pyre.client.pyre` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(pyre, "run_pyre")
    def test_run_pyre_with_retry(
        self, run_pyre: MagicMock, get_all_items: MagicMock, prompt_user: MagicMock
    ) -> None:
        run_pyre.side_effect = [
            pyre.FailedOutsideLocalConfigurationException(
                exit_code=ExitCode.FAILURE,
                command=mock_incremental_command(mock_configuration()),
                exception_message="something",
            ),
            0,
        ]
        command_line_arguments = _get_arguments()
        actual_exit_code = pyre._run_pyre_with_retry(command_line_arguments)
        self.assertEqual(run_pyre.call_count, 2)
        self.assertEqual(actual_exit_code, ExitCode.SUCCESS)
        self.assertEqual(command_line_arguments.local_configuration, "foo/bar")

    @patch.object(recently_used_configurations.Cache, "get_all_items", return_value=[])
    # pyre-fixme[56]: Argument `tools.pyre.client.pyre` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(pyre, "run_pyre")
    def test_run_pyre_with_retry__no_recent_configurations(
        self, run_pyre: MagicMock, get_all_items: MagicMock
    ) -> None:
        run_pyre.side_effect = pyre.FailedOutsideLocalConfigurationException(
            exit_code=ExitCode.FAILURE,
            command=mock_incremental_command(mock_configuration()),
            exception_message="something",
        )
        command_line_arguments = _get_arguments()
        actual_exit_code = pyre._run_pyre_with_retry(command_line_arguments)
        self.assertEqual(run_pyre.call_count, 1)
        self.assertEqual(actual_exit_code, ExitCode.FAILURE)

    @patch.object(
        recently_used_configurations, "prompt_user_for_local_root", return_value="foo"
    )
    @patch.object(
        recently_used_configurations.Cache, "get_all_items", return_value=["foo"]
    )
    # pyre-fixme[56]: Argument `tools.pyre.client.pyre` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(pyre, "run_pyre")
    def test_run_pyre_with_retry__fail_again(
        self, run_pyre: MagicMock, get_all_items: MagicMock, prompt_user: MagicMock
    ) -> None:
        run_pyre.side_effect = [
            pyre.FailedOutsideLocalConfigurationException(
                exit_code=ExitCode.FAILURE,
                command=mock_incremental_command(mock_configuration()),
                exception_message="something",
            ),
            pyre.FailedOutsideLocalConfigurationException(
                exit_code=ExitCode.FAILURE,
                command=mock_incremental_command(mock_configuration()),
                exception_message="something",
            ),
        ]
        command_line_arguments = _get_arguments()
        actual_exit_code = pyre._run_pyre_with_retry(command_line_arguments)
        self.assertEqual(run_pyre.call_count, 2)
        self.assertEqual(actual_exit_code, commands.ExitCode.FAILURE)

    @patch.object(
        recently_used_configurations, "prompt_user_for_local_root", return_value=None
    )
    @patch.object(
        recently_used_configurations.Cache, "get_all_items", return_value=["foo"]
    )
    # pyre-fixme[56]: Argument `tools.pyre.client.pyre` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(pyre, "run_pyre")
    def test_run_pyre_with_retry__invalid_user_input(
        self, run_pyre: MagicMock, get_all_items: MagicMock, prompt_user: MagicMock
    ) -> None:
        run_pyre.side_effect = pyre.FailedOutsideLocalConfigurationException(
            exit_code=ExitCode.FAILURE,
            command=mock_incremental_command(mock_configuration()),
            exception_message="something",
        )
        command_line_arguments = _get_arguments()
        actual_exit_code = pyre._run_pyre_with_retry(command_line_arguments)
        self.assertEqual(run_pyre.call_count, 1)
        self.assertEqual(actual_exit_code, ExitCode.FAILURE)
