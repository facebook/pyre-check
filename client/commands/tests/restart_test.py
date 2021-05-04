# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from unittest.mock import MagicMock, patch

from ... import (
    commands,
    configuration_monitor,
    project_files_monitor,
    configuration as configuration_module,
)
from ...analysis_directory import AnalysisDirectory
from ...commands import restart
from ..command import IncrementalStyle
from .command_test import mock_arguments, mock_configuration


class RestartTest(unittest.TestCase):
    @patch("{}.ProjectFilesMonitor".format(project_files_monitor.__name__))
    @patch.object(restart, "Start")
    @patch.object(restart, "Stop")
    @patch.object(restart, "Incremental")
    @patch.object(configuration_monitor.ConfigurationMonitor, "daemonize")
    def test_restart(
        self,
        _daemonize,
        commands_Incremental,
        commands_Stop,
        commands_Start,
        _daemonize_project_files_monitor,
    ) -> None:
        state = MagicMock()
        state.running = ["."]
        original_directory = "/original/directory"
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement(".")
        )

        commands_Stop().run().exit_code.return_value = commands.ExitCode.SUCCESS

        commands.Restart(
            arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            terminal=False,
            incremental_style=IncrementalStyle.FINE_GRAINED,
            use_watchman=True,
            store_type_check_resolution=False,
        )._run()
        commands_Stop.assert_called_with(
            arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            from_restart=True,
        )
        commands_Incremental.assert_called_with(
            arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            nonblocking=False,
            incremental_style=IncrementalStyle.FINE_GRAINED,
            no_start_server=False,
            no_watchman=False,
        )
        commands_Start.assert_not_called()

        commands_Stop.reset_mock()
        commands_Incremental.reset_mock()
        commands_Stop().run().exit_code.return_value = commands.ExitCode.FAILURE
        commands.Restart(
            arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            terminal=False,
            incremental_style=IncrementalStyle.FINE_GRAINED,
            use_watchman=True,
            store_type_check_resolution=False,
        )._run()
        commands_Stop.assert_called_with(
            arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
            from_restart=True,
        )
        commands_Incremental.assert_not_called()
        commands_Start.assert_not_called()
