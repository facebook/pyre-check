# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import os
import unittest
from io import StringIO
from unittest.mock import MagicMock, Mock, call, patch

from ... import commands  # noqa
from ...analysis_directory import AnalysisDirectory
from ...commands import command, stop  # noqa
from .command_test import mock_arguments, mock_configuration


def _mark_processes_as_completed(process_id: int, signal: int) -> None:
    if signal == 0:
        raise ProcessLookupError()
    else:
        return


@patch.object(os, "kill", side_effect=_mark_processes_as_completed)
@patch.object(commands.stop.ProjectFilesMonitor, "stop_project_monitor")
@patch.object(
    commands.stop.ProjectFilesMonitor, "pid_path", return_value="file_monitor.pid"
)
@patch.object(commands.stop, "open", side_effect=lambda filename: StringIO("42"))
@patch.object(commands.Kill, "_run")
@patch.object(commands.Command, "_state")
class StopTest(unittest.TestCase):
    def setUp(self) -> None:
        self.original_directory = "/original/directory"
        self.arguments = mock_arguments()
        self.arguments.terminal = False
        self.configuration = mock_configuration()
        self.analysis_directory = AnalysisDirectory(".")

    def test_stop_running_server(
        self,
        commands_Command_state: MagicMock,
        kill_command_run: MagicMock,
        file_open: MagicMock,
        pid_path: MagicMock,
        stop_project_monitor: MagicMock,
        os_kill: MagicMock,
    ) -> None:
        commands_Command_state.return_value = commands.command.State.RUNNING
        with patch.object(commands.Command, "_call_client") as call_client:
            commands.Stop(
                self.arguments,
                self.original_directory,
                self.configuration,
                self.analysis_directory,
            ).run()
            call_client.assert_called_once_with(command=commands.Stop.NAME)
            kill_command_run.assert_not_called()
            os_kill.assert_called_once_with(42, 0)
            stop_project_monitor.assert_called_once()

    def test_stop_dead_server(
        self,
        commands_Command_state: MagicMock,
        kill_command_run: MagicMock,
        file_open: MagicMock,
        pid_path: MagicMock,
        stop_project_monitor: MagicMock,
        os_kill: MagicMock,
    ) -> None:
        commands_Command_state.return_value = commands.command.State.DEAD
        with patch.object(commands.Command, "_call_client") as call_client:
            commands.Stop(
                self.arguments,
                self.original_directory,
                self.configuration,
                self.analysis_directory,
            ).run()
            call_client.assert_not_called()
            kill_command_run.assert_called_once()
            os_kill.assert_not_called()
            stop_project_monitor.assert_called_once()

    def test_stop_running_server__stop_fails(
        self,
        commands_Command_state: MagicMock,
        kill_command_run: MagicMock,
        file_open: MagicMock,
        pid_path: MagicMock,
        stop_project_monitor: MagicMock,
        os_kill: MagicMock,
    ) -> None:
        commands_Command_state.return_value = commands.command.State.RUNNING
        with patch.object(commands.Command, "_call_client") as call_client:

            def fail_on_stop(command, flags=None):  # noqa
                flags = flags or []
                if command == commands.Stop.NAME:
                    raise commands.ClientException
                return Mock()

            call_client.side_effect = fail_on_stop
            commands.Stop(
                self.arguments,
                self.original_directory,
                self.configuration,
                self.analysis_directory,
            ).run()
            call_client.assert_has_calls([call(command=commands.Stop.NAME)])
            kill_command_run.assert_called_once()
            os_kill.assert_not_called()
            stop_project_monitor.assert_called_once()

    def test_stop_ignores_flags(
        self,
        commands_Command_state: MagicMock,
        kill_command_run: MagicMock,
        file_open: MagicMock,
        pid_path: MagicMock,
        stop_project_monitor: MagicMock,
        os_kill: MagicMock,
    ) -> None:
        self.arguments.debug = True
        flags = commands.Stop(
            self.arguments,
            self.original_directory,
            self.configuration,
            self.analysis_directory,
        )._flags()
        self.assertEqual(flags, ["-log-directory", ".pyre"])
