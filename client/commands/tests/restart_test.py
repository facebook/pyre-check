# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import MagicMock, patch

from ... import commands, monitor, project_files_monitor
from ...commands import restart
from .command_test import mock_arguments, mock_configuration


class RestartTest(unittest.TestCase):
    @patch("{}.Monitor".format(project_files_monitor.__name__))
    @patch.object(restart, "Stop")
    @patch.object(monitor.Monitor, "daemonize")
    def test_restart(
        self, _daemonize, commands_Stop, _daemonize_project_files_monitor
    ) -> None:
        state = MagicMock()
        state.running = ["."]

        arguments = mock_arguments()
        arguments.terminal = False

        configuration = mock_configuration()

        analysis_directory = "."

        with patch.object(restart, "Stop") as commands_Stop, patch.object(
            restart, "Start"
        ) as commands_Start, patch.object(
            restart, "Incremental"
        ) as commands_Incremental:
            commands.Restart(arguments, configuration, analysis_directory)._run()
            commands_Stop.assert_called_with(
                arguments, configuration, analysis_directory
            )
            commands_Incremental.assert_called_with(
                arguments, configuration, analysis_directory
            )
            commands_Start.assert_not_called()
