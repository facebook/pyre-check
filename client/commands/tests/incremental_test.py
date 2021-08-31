# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import os
import subprocess
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

from ... import (
    commands,
    find_directories,
    json_rpc,
    configuration as configuration_module,
)
from ...analysis_directory import AnalysisDirectory, SharedAnalysisDirectory
from ...commands import command, incremental, stop  # noqa
from ...socket_connection import SocketConnection
from ..command import IncrementalStyle
from .command_test import mock_arguments, mock_configuration


class IncrementalTest(unittest.TestCase):
    @patch.object(subprocess, "Popen")
    @patch.object(
        command,
        "_convert_json_response_to_result",
        return_value=command.Result(output="{}", code=0),
    )
    @patch.object(json_rpc, "write_lsp_request")
    @patch.object(json_rpc, "read_lsp_response")
    @patch.object(SocketConnection, "connect")
    @patch.object(SocketConnection, "perform_handshake")
    @patch(
        f"{find_directories.__name__}.find_global_and_local_root",
        return_value=find_directories.FoundRoot(Path("/root")),
    )
    @patch.object(os.path, "exists", side_effect=lambda path: True)
    @patch.object(commands.Command, "_state")
    @patch.object(incremental, "Start")
    @patch.object(stop, "Stop")
    def test_incremental(
        self,
        commands_Stop,
        commands_Start,
        commands_Command_state,
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

        configuration = mock_configuration(version_hash="hash")
        analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement(".")
        )

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
                no_watchman=True,
            )
            self.assertEqual(
                test_command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-expected-binary-version",
                    "hash",
                ],
            )

            test_command.run()
            connect.assert_called_once()
            popen.assert_called_once_with(
                ["tail", "-F", "-n", "0", ".pyre/server/server.stdout"],
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
                universal_newlines=True,
            )

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
                no_watchman=True,
            )
            self.assertEqual(
                test_command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-expected-binary-version",
                    "hash",
                    "-nonblocking",
                ],
            )

            test_command.run()
            connect.assert_called_once()

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
                no_watchman=True,
            )
            self.assertEqual(
                test_command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-expected-binary-version",
                    "hash",
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
                use_watchman=False,
                incremental_style=command.IncrementalStyle.FINE_GRAINED,
            )
            connect.assert_called_once()

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
                no_watchman=True,
            )
            self.assertEqual(
                test_command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-expected-binary-version",
                    "hash",
                ],
            )

            test_command.run()
            connect.assert_called_once()
            # Prepare only gets called when actually starting the server.
            prepare.assert_not_called()

        commands_Command_state.return_value = commands.command.State.DEAD
        arguments = mock_arguments(
            load_initial_state_from="/a/b", changed_files_path="/c/d"
        )
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
                no_watchman=True,
            )
            self.assertEqual(
                test_command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-expected-binary-version",
                    "hash",
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
                use_watchman=False,
                incremental_style=command.IncrementalStyle.FINE_GRAINED,
            )
            connect.assert_called_once()

        arguments = mock_arguments()
        original_directory = "/test"  # called from
        find_global_and_local_root.return_value = find_directories.FoundRoot(
            Path("/root")
        )  # project root
        configuration = mock_configuration(version_hash="hash")
        analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement("/root")
        )

        with patch.object(SocketConnection, "connect") as connect, patch.object(
            json,
            "loads",
            return_value={
                "errors": [
                    {
                        "line": 4,
                        "column": 11,
                        "stop_line": 4,
                        "stop_column": 21,
                        "path": "test/path.py",
                        "code": -1,
                        "name": "Revealed type",
                        "description": ".Fake error",
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
                no_watchman=True,
            )
            self.assertEqual(
                test_command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-expected-binary-version",
                    "hash",
                ],
            )

            test_command.run()
            connect.assert_called_once()
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
                no_watchman=True,
            )
            test_command.run()
            connect.assert_not_called()
            self.assertEqual(test_command._exit_code, commands.ExitCode.FAILURE)

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
            mock_arguments(),
            "/original/directory",
            configuration=mock_configuration(version_hash="hash"),
            analysis_directory=AnalysisDirectory(
                configuration_module.SimpleSearchPathElement("/root")
            ),
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
            analysis_directory=AnalysisDirectory(
                configuration_module.SimpleSearchPathElement("/root")
            ),
            nonblocking=False,
            incremental_style=IncrementalStyle.FINE_GRAINED,
            no_start_server=False,
            no_watchman=False,
        )
        command_state.return_value = commands.command.State.DEAD
        incremental_command._run()
        self.assertTrue(start_class.call_args[1]["use_watchman"])
