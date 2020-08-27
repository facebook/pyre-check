# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import os
import subprocess
import unittest
from pathlib import Path
from unittest.mock import MagicMock, Mock, patch

from ... import commands, json_rpc
from ...analysis_directory import AnalysisDirectory, SharedAnalysisDirectory
from ...commands import command, incremental, stop  # noqa
from ...find_directories import FoundRoot
from ...socket_connection import SocketConnection
from ..command import IncrementalStyle, __name__ as client_name
from .command_test import mock_arguments, mock_configuration


_typeshed_search_path: str = "{}.typeshed_search_path".format(
    commands.incremental.__name__
)


class IncrementalTest(unittest.TestCase):
    @patch.object(subprocess, "Popen")
    @patch.object(
        command,
        "_convert_json_response_to_result",
        return_value=command.Result(output="{}", code=0),
    )
    @patch.object(json_rpc.Request, "write")
    @patch.object(json_rpc, "read_response")
    @patch.object(SocketConnection, "connect")
    @patch.object(SocketConnection, "perform_handshake")
    @patch(
        f"{client_name}.find_global_and_local_root", return_value=FoundRoot(Path("."))
    )
    @patch.object(os.path, "exists", side_effect=lambda path: True)
    @patch(_typeshed_search_path, Mock(return_value=["path3"]))
    @patch.object(incremental.Incremental, "_restart_file_monitor_if_needed")
    @patch.object(commands.Command, "_state")
    @patch.object(incremental, "Start")
    # pyre-fixme[56]: Argument `tools.pyre.client.commands.stop` to decorator
    #  factory `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(stop, "Stop")
    def test_incremental(
        self,
        commands_Stop,
        commands_Start,
        commands_Command_state,
        restart_file_monitor_if_needed,
        exists,
        find_global_and_local_root,
        perform_handshake,
        connect,
        read_response,
        write,
        _convert_to_result,
        popen,
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

        original_directory = "/original/directory"
        arguments = mock_arguments()

        configuration = mock_configuration()
        configuration.version_hash = "hash"
        analysis_directory = AnalysisDirectory(".")

        with patch.object(SocketConnection, "connect") as connect, patch.object(
            json, "loads", return_value=[]
        ):
            test_command = incremental.Incremental(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=analysis_directory,
                nonblocking=False,
                incremental_style=IncrementalStyle.FINE_GRAINED,
                no_start_server=False,
                no_watchman=False,
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
            restart_file_monitor_if_needed.assert_called_once()
            popen.assert_called_once_with(
                ["tail", "--follow", "--lines=0", ".pyre/server/server.stdout"],
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
                universal_newlines=True,
            )

        restart_file_monitor_if_needed.reset_mock()
        with patch.object(SocketConnection, "connect") as connect, patch.object(
            json, "loads", return_value=[]
        ):
            nonblocking_arguments = mock_arguments()
            test_command = incremental.Incremental(
                nonblocking_arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=analysis_directory,
                nonblocking=True,
                incremental_style=IncrementalStyle.FINE_GRAINED,
                no_start_server=False,
                no_watchman=False,
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
            restart_file_monitor_if_needed.assert_called_once()

        restart_file_monitor_if_needed.reset_mock()
        commands_Command_state.return_value = commands.command.State.DEAD
        with patch.object(SocketConnection, "connect") as connect, patch.object(
            json, "loads", return_value=[]
        ):
            test_command = commands.Incremental(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=analysis_directory,
                nonblocking=False,
                incremental_style=IncrementalStyle.FINE_GRAINED,
                no_start_server=False,
                no_watchman=False,
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
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=analysis_directory,
                terminal=False,
                store_type_check_resolution=False,
                use_watchman=True,
                incremental_style=command.IncrementalStyle.FINE_GRAINED,
            )
            connect.assert_called_once()
            restart_file_monitor_if_needed.assert_not_called()

        restart_file_monitor_if_needed.reset_mock()
        with patch.object(SocketConnection, "connect") as connect, patch.object(
            json, "loads", return_value=[]
        ), patch.object(SharedAnalysisDirectory, "prepare") as prepare:
            test_command = incremental.Incremental(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=analysis_directory,
                nonblocking=False,
                incremental_style=IncrementalStyle.FINE_GRAINED,
                no_start_server=False,
                no_watchman=False,
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
            restart_file_monitor_if_needed.assert_not_called()
            # Prepare only gets called when actually starting the server.
            prepare.assert_not_called()

        commands_Command_state.return_value = commands.command.State.DEAD
        arguments = mock_arguments(
            load_initial_state_from="/a/b", changed_files_path="/c/d"
        )
        restart_file_monitor_if_needed.reset_mock()
        with patch.object(SocketConnection, "connect") as connect, patch.object(
            json, "loads", return_value=[]
        ):
            test_command = commands.Incremental(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=analysis_directory,
                nonblocking=False,
                incremental_style=IncrementalStyle.FINE_GRAINED,
                no_start_server=False,
                no_watchman=False,
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
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=analysis_directory,
                terminal=False,
                store_type_check_resolution=False,
                use_watchman=True,
                incremental_style=command.IncrementalStyle.FINE_GRAINED,
            )
            connect.assert_called_once()
            restart_file_monitor_if_needed.assert_not_called()

        arguments = mock_arguments()
        original_directory = "/test"  # called from
        find_global_and_local_root.return_value = FoundRoot(Path("/"))  # project root
        configuration = mock_configuration()
        configuration.version_hash = "hash"
        analysis_directory = AnalysisDirectory(".")

        restart_file_monitor_if_needed.reset_mock()
        with patch.object(SocketConnection, "connect") as connect, patch.object(
            json,
            "loads",
            return_value={
                "errors": [
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
                ]
            },
        ):
            test_command = incremental.Incremental(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=analysis_directory,
                nonblocking=False,
                incremental_style=IncrementalStyle.FINE_GRAINED,
                no_start_server=False,
                no_watchman=False,
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
            restart_file_monitor_if_needed.assert_not_called()
            self.assertEqual(test_command._exit_code, commands.ExitCode.FOUND_ERRORS)

        # If Start returns with an error, fail early
        start_exit_code.return_value = commands.ExitCode.FAILURE
        with patch.object(SocketConnection, "connect") as connect:
            test_command = incremental.Incremental(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=analysis_directory,
                nonblocking=False,
                incremental_style=IncrementalStyle.FINE_GRAINED,
                no_start_server=False,
                no_watchman=False,
            )
            test_command.run()
            connect.assert_not_called()
            self.assertEqual(test_command._exit_code, commands.ExitCode.FAILURE)

    @patch.object(incremental.Incremental, "_send_and_handle_socket_request")
    @patch.object(commands.Command, "_state")
    # pyre-fixme[56]: Argument `tools.pyre.client.commands.incremental` to decorator
    #  factory `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(incremental, "Start")
    def test_run_passes_no_watchman_flag(
        self,
        start_class: MagicMock,
        command_state: MagicMock,
        send_and_handle_socket_request: MagicMock,
    ) -> None:
        incremental_command = incremental.Incremental(
            mock_arguments(),
            "/original/directory",
            configuration=mock_configuration(version_hash="hash"),
            analysis_directory=AnalysisDirectory("/root"),
            nonblocking=False,
            incremental_style=IncrementalStyle.FINE_GRAINED,
            no_start_server=False,
            no_watchman=True,
        )
        command_state.return_value = commands.command.State.DEAD
        incremental_command._run()
        self.assertFalse(start_class.call_args[1]["use_watchman"])

    @patch.object(incremental.Incremental, "_send_and_handle_socket_request")
    @patch.object(commands.Command, "_state")
    # pyre-fixme[56]: Argument `tools.pyre.client.commands.incremental` to decorator
    #  factory `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(incremental, "Start")
    def test_run_passes_false_no_watchman_flag(
        self,
        start_class: MagicMock,
        command_state: MagicMock,
        send_and_handle_socket_request: MagicMock,
    ) -> None:
        incremental_command = incremental.Incremental(
            mock_arguments(),
            "/original/directory",
            configuration=mock_configuration(version_hash="hash"),
            analysis_directory=AnalysisDirectory("/root"),
            nonblocking=False,
            incremental_style=IncrementalStyle.FINE_GRAINED,
            no_start_server=False,
            no_watchman=False,
        )
        command_state.return_value = commands.command.State.DEAD
        incremental_command._run()
        self.assertTrue(start_class.call_args[1]["use_watchman"])

    @patch.object(incremental.ProjectFilesMonitor, "restart_if_dead")
    def test_restart_file_monitor_if_needed_no_watchman(
        self, restart_if_dead: MagicMock
    ) -> None:
        incremental_command = incremental.Incremental(
            mock_arguments(),
            "/original/directory",
            configuration=mock_configuration(version_hash="hash"),
            analysis_directory=AnalysisDirectory("/root"),
            nonblocking=False,
            incremental_style=IncrementalStyle.FINE_GRAINED,
            no_start_server=False,
            no_watchman=True,
        )
        incremental_command._restart_file_monitor_if_needed()
        restart_if_dead.assert_not_called()

    @patch.object(incremental.ProjectFilesMonitor, "restart_if_dead")
    def test_restart_file_monitor_if_needed(self, restart_if_dead: MagicMock) -> None:
        incremental_command = incremental.Incremental(
            mock_arguments(),
            "/original/directory",
            configuration=mock_configuration(version_hash="hash"),
            analysis_directory=AnalysisDirectory("/root"),
            nonblocking=False,
            incremental_style=IncrementalStyle.FINE_GRAINED,
            no_start_server=False,
            no_watchman=False,
        )
        incremental_command._restart_file_monitor_if_needed()
        restart_if_dead.assert_called_once()
