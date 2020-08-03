# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import argparse
import json
import os
import shutil
import unittest
from unittest.mock import MagicMock, call, patch

from .. import (
    analysis_directory,
    buck,
    commands,
    configuration,
    pyre,
    recently_used_configurations,
    statistics,
    terminal,
)
from ..commands import ExitCode
from ..exceptions import EnvironmentException
from ..pyre import _set_default_command
from .mocks import mock_incremental_command


class PyreTest(unittest.TestCase):
    @patch.object(configuration.Configuration, "_validate")
    @patch.object(configuration.Configuration, "disabled", return_value=True)
    def test_disabled(self, disabled, validate) -> None:
        self.assertEqual(pyre.main(["check"]), 0)

    @patch.object(configuration.Configuration, "_read")
    @patch.object(configuration.Configuration, "_validate")
    @patch.object(commands.Persistent, "run_null_server")
    def test_persistent_integration(self, run_null_server, validate, read) -> None:
        validate.side_effect = commands.ClientException
        self.assertEqual(pyre.main(["persistent"]), 2)
        run_null_server.assert_not_called()

        validate.side_effect = EnvironmentException
        self.assertEqual(pyre.main(["persistent"]), 0)
        run_null_server.assert_has_calls([call(timeout=3600 * 12)])

    @patch.object(os, "getenv")
    @patch.object(os, "isatty")
    @patch.object(json, "dump")
    @patch.object(json, "load")
    @patch.object(configuration.Configuration, "_read")
    @patch.object(configuration.Configuration, "_validate")
    # pyre-fixme[56]: Argument `tools.pyre.client.buck` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(buck, "generate_source_directories", return_value=["."])
    def test_buck_build_prompting(
        self,
        generate_source_directories,
        validate,
        read,
        _json_load,
        _json_dump,
        _os_isatty,
        _os_getenv,
    ) -> None:
        mock_success = MagicMock()
        mock_success.exit_code = lambda: 0

        # Pretend that all CI jobs are running attached to a terminal, in a tty.
        _os_isatty.return_value = True
        _os_getenv.return_value = "term"

        with patch.object(
            commands.Check, "run", return_value=mock_success
        ), patch.object(
            analysis_directory.SharedAnalysisDirectory, "cleanup"
        ) as cleanup:
            self.assertEqual(pyre.main(["check"]), 0)
            generate_source_directories.assert_not_called()
            cleanup.assert_has_calls([call()])

            # The generation of source directories is handled within _run, which is
            # mocked here (via the mock of run()), so verify that we don't
            # call generate_source_directories outside of run. Tests in the
            # subcommands verify that prepare() is called, which calls
            # generate_source_directories.
            self.assertEqual(pyre.main(["--target", "//a/b", "check"]), 0)
            generate_source_directories.assert_not_called()

        with patch.object(commands.Incremental, "run", return_value=mock_success):
            # One for shutil.which("watchman"),
            # another for shutil.which(BINARY_NAME).
            with patch.object(shutil, "which", side_effect=[True, True]):
                self.assertEqual(pyre.main([]), 0)
                generate_source_directories.assert_not_called()
        with patch.object(commands.Persistent, "run", return_value=mock_success):
            self.assertEqual(pyre.main(["persistent"]), 0)
            generate_source_directories.assert_not_called()
        with patch.object(commands.Start, "run", return_value=mock_success):
            self.assertEqual(pyre.main(["start"]), 0)
            generate_source_directories.assert_not_called()
        with patch.object(commands.Start, "run", return_value=mock_success):
            self.assertEqual(pyre.main(["--noninteractive", "start"]), 0)
            generate_source_directories.assert_not_called()

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
        command = mock_incremental_command()
        command._configuration.logger = MagicMock()
        pyre._log_statistics(command, arguments, 0.0, "foo", "bar", 42)
        statistics_log.assert_called_once()

    # pyre-fixme[56]: Argument `tools.pyre.client.statistics` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(statistics, "log")
    def test_log_statistics__should_rerun(self, statistics_log: MagicMock) -> None:
        arguments = argparse.Namespace()
        command = mock_incremental_command()
        command._configuration.logger = MagicMock()
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
                command=mock_incremental_command(),
                exception_message="something",
            ),
            0,
        ]
        command_line_arguments = argparse.Namespace()
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
            command=mock_incremental_command(),
            exception_message="something",
        )
        command_line_arguments = argparse.Namespace()
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
                command=mock_incremental_command(),
                exception_message="something",
            ),
            pyre.FailedOutsideLocalConfigurationException(
                exit_code=ExitCode.FAILURE,
                command=mock_incremental_command(),
                exception_message="something",
            ),
        ]
        command_line_arguments = argparse.Namespace()
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
            command=mock_incremental_command(),
            exception_message="something",
        )
        command_line_arguments = argparse.Namespace()
        actual_exit_code = pyre._run_pyre_with_retry(command_line_arguments)
        self.assertEqual(run_pyre.call_count, 1)
        self.assertEqual(actual_exit_code, ExitCode.FAILURE)
