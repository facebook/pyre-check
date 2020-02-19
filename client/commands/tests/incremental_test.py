# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import os
import subprocess
import unittest
from unittest.mock import MagicMock, Mock, call, patch

from ... import commands, json_rpc
from ...analysis_directory import AnalysisDirectory, SharedAnalysisDirectory
from ...commands import command, incremental, stop  # noqa
from ...socket_connection import SocketConnection
from ..command import __name__ as client_name
from .command_test import mock_arguments, mock_configuration


_typeshed_search_path: str = "{}.typeshed_search_path".format(
    commands.incremental.__name__
)


class IncrementalTest(unittest.TestCase):
    @patch.object(
        command,
        "_convert_json_response_to_result",
        return_value=command.Result(output="{}", code=0),
    )
    @patch.object(json_rpc.Request, "write")
    @patch.object(json_rpc, "read_response")
    @patch.object(SocketConnection, "connect")
    @patch.object(SocketConnection, "perform_handshake")
    @patch("{}.find_project_root".format(client_name), return_value=".")
    @patch("{}.find_local_root".format(client_name), return_value=None)
    @patch.object(os.path, "exists", side_effect=lambda path: True)
    @patch(_typeshed_search_path, Mock(return_value=["path3"]))
    @patch.object(incremental, "ProjectFilesMonitor")
    @patch.object(commands.Command, "_state")
    @patch.object(incremental, "Start")
    @patch.object(stop, "Stop")
    def test_incremental(
        self,
        commands_Stop,
        commands_Start,
        commands_Command_state,
        Monitor,
        exists,
        find_local_root,
        find_project_root,
        perform_handshake,
        connect,
        read_response,
        write,
        _convert_to_result,
    ) -> None:
        state = MagicMock()
        state.running = ["running"]
        state.dead = []
        commands_Command_state.return_value = state
        start_exit_code = MagicMock()
        start_exit_code.return_value = commands.ExitCode.SUCCESS
        start_command = commands_Start()
        start_command._configuration = mock_configuration()
        start_command.run().exit_code = start_exit_code
        file_monitor_instance = MagicMock()
        Monitor.return_value = file_monitor_instance
        Monitor.is_alive.return_value = False

        original_directory = "/original/directory"
        arguments = mock_arguments()

        configuration = mock_configuration()
        configuration.version_hash = "hash"
        analysis_directory = AnalysisDirectory(".")

        with patch.object(SocketConnection, "connect") as connect, patch(
            "json.loads", return_value=[]
        ):
            test_command = incremental.Incremental(
                arguments, original_directory, configuration, analysis_directory
            )
            self.assertEqual(
                test_command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )

            test_command.run()
            connect.assert_called_once()
            Monitor.is_alive.assert_called_once_with(configuration)
            Monitor.assert_called_once_with(configuration, ".", analysis_directory)
            file_monitor_instance.daemonize.assert_called_once_with()

        Monitor.reset_mock()
        Monitor.is_alive.return_value = True
        file_monitor_instance.reset_mock()
        with patch.object(SocketConnection, "connect") as connect, patch(
            "json.loads", return_value=[]
        ):
            nonblocking_arguments = mock_arguments()
            nonblocking_arguments.nonblocking = True
            test_command = incremental.Incremental(
                nonblocking_arguments,
                original_directory,
                configuration,
                analysis_directory,
            )
            self.assertEqual(
                test_command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                    "-nonblocking",
                ],
            )

            test_command.run()
            connect.assert_called_once()
            Monitor.is_alive.assert_called_once_with(configuration)
            Monitor.assert_not_called()
            file_monitor_instance.daemonize.assert_not_called()

        commands_Command_state.return_value = commands.command.State.DEAD
        Monitor.reset_mock()
        file_monitor_instance.reset_mock()
        with patch.object(SocketConnection, "connect") as connect, patch(
            "json.loads", return_value=[]
        ):
            test_command = commands.Incremental(
                arguments, original_directory, configuration, analysis_directory
            )
            self.assertEqual(
                test_command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )

            test_command.run()
            commands_Start.assert_called_with(
                arguments, original_directory, configuration, analysis_directory
            )
            connect.assert_called_once()
            Monitor.assert_not_called()
            file_monitor_instance.daemonize.assert_not_called()

        with patch.object(SocketConnection, "connect") as connect, patch(
            "json.loads", return_value=[]
        ), patch.object(SharedAnalysisDirectory, "prepare") as prepare:
            test_command = incremental.Incremental(
                arguments, original_directory, configuration, analysis_directory
            )
            self.assertEqual(
                test_command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )

            test_command.run()
            connect.assert_called_once()
            Monitor.assert_not_called()
            file_monitor_instance.daemonize.assert_not_called()
            # Prepare only gets called when actually starting the server.
            prepare.assert_not_called()

        commands_Command_state.return_value = commands.command.State.DEAD
        arguments = mock_arguments(
            load_initial_state_from="/a/b", changed_files_path="/c/d"
        )
        with patch.object(SocketConnection, "connect") as connect, patch(
            "json.loads", return_value=[]
        ):
            test_command = commands.Incremental(
                arguments, original_directory, configuration, analysis_directory
            )
            self.assertEqual(
                test_command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )

            test_command.run()
            commands_Start.assert_called_with(
                arguments, original_directory, configuration, analysis_directory
            )
            connect.assert_called_once()
            Monitor.assert_not_called()
            file_monitor_instance.daemonize.assert_not_called()

        arguments = mock_arguments()
        original_directory = "/test"  # called from
        find_project_root.return_value = "/"  # project root
        configuration = mock_configuration()
        configuration.version_hash = "hash"
        analysis_directory = AnalysisDirectory(".")

        with patch.object(SocketConnection, "connect") as connect, patch(
            "json.loads",
            return_value=[
                {
                    "line": 4,
                    "column": 11,
                    "path": "test/path.py",
                    "code": -1,
                    "name": "Revealed type",
                    "description": ".Fake error",
                    "inference": {},
                    "define": "c.$toplevel",
                }
            ],
        ):
            test_command = incremental.Incremental(
                arguments, original_directory, configuration, analysis_directory
            )
            self.assertEqual(
                test_command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    "/",
                    "-log-directory",
                    ".pyre",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )

            test_command.run()
            connect.assert_called_once()
            Monitor.assert_not_called()
            file_monitor_instance.daemonize.assert_not_called()
            self.assertEqual(test_command._exit_code, commands.ExitCode.FOUND_ERRORS)

        # If Start returns with an error, fail early
        start_exit_code.return_value = commands.ExitCode.FAILURE
        with patch.object(SocketConnection, "connect") as connect:
            test_command = incremental.Incremental(
                arguments, original_directory, configuration, analysis_directory
            )
            test_command.run()
            connect.assert_not_called()
            self.assertEqual(test_command._exit_code, commands.ExitCode.FAILURE)

    def test_read_stderr(self) -> None:
        with patch("subprocess.Popen") as popen:
            original_directory = "/original/directory"
            arguments = mock_arguments()

            configuration = mock_configuration()
            configuration.version_hash = "hash"
            analysis_directory = AnalysisDirectory("/root")
            test_command = incremental.Incremental(
                arguments, original_directory, configuration, analysis_directory
            )
            stream = MagicMock()
            test_command._read_stderr(stream)
            popen.assert_called_once_with(
                ["tail", "--follow", "--lines=0", ".pyre/server/server.stdout"],
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
            )

    @patch.object(incremental.Incremental, "_send_and_handle_socket_request")
    @patch.object(commands.Command, "_state")
    @patch.object(incremental, "Start")
    def test_run_passes_no_watchman_flag(
        self,
        start_class: MagicMock,
        command_state: MagicMock,
        send_and_handle_socket_request: MagicMock,
    ) -> None:
        incremental_command = incremental.Incremental(
            mock_arguments(no_watchman=True),
            "/original/directory",
            mock_configuration(version_hash="hash"),
            AnalysisDirectory("/root"),
        )
        command_state.return_value = commands.command.State.DEAD
        incremental_command._run()
        self.assertTrue(start_class.call_args[0][0].no_watchman)

    @patch.object(incremental.Incremental, "_send_and_handle_socket_request")
    @patch.object(commands.Command, "_state")
    @patch.object(incremental, "Start")
    def test_run_passes_false_no_watchman_flag(
        self,
        start_class: MagicMock,
        command_state: MagicMock,
        send_and_handle_socket_request: MagicMock,
    ) -> None:
        incremental_command = incremental.Incremental(
            mock_arguments(no_watchman=False),
            "/original/directory",
            mock_configuration(version_hash="hash"),
            AnalysisDirectory("/root"),
        )
        command_state.return_value = commands.command.State.DEAD
        incremental_command._run()
        self.assertFalse(start_class.call_args[0][0].no_watchman)
