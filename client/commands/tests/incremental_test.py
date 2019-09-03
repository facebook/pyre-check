# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import subprocess
import unittest
from unittest.mock import MagicMock, Mock, call, patch

from ... import commands
from ...commands import stop  # noqa
from ...commands import incremental
from ...filesystem import AnalysisDirectory, SharedAnalysisDirectory
from .command_test import mock_arguments, mock_configuration


_typeshed_search_path = "{}.typeshed_search_path".format(commands.incremental.__name__)


class IncrementalTest(unittest.TestCase):
    @patch(_typeshed_search_path, Mock(return_value=["path3"]))
    @patch.object(incremental, "Monitor")
    @patch.object(commands.Command, "_state")
    @patch.object(incremental, "Start")
    @patch.object(stop, "Stop")
    def test_incremental(
        self, commands_Stop, commands_Start, commands_Command_state, Monitor
    ) -> None:
        state = MagicMock()
        state.running = ["running"]
        state.dead = []
        commands_Command_state.return_value = state
        start_exit_code = MagicMock()
        start_exit_code.return_value = commands.ExitCode.SUCCESS
        commands_Start().run().exit_code = start_exit_code
        file_monitor_instance = MagicMock()
        Monitor.return_value = file_monitor_instance
        Monitor.is_alive.return_value = False

        arguments = mock_arguments()

        configuration = mock_configuration()
        configuration.version_hash = "hash"
        analysis_directory = AnalysisDirectory(".")

        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            command = incremental.Incremental(
                arguments, configuration, analysis_directory
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser",
                    "-project-root",
                    ".",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )

            command.run()
            call_client.assert_called_once_with(command=commands.Incremental.NAME)
            Monitor.is_alive.assert_called_once_with(".")
            Monitor.assert_called_once_with(
                arguments, configuration, analysis_directory
            )
            file_monitor_instance.daemonize.assert_called_once_with()

        Monitor.reset_mock()
        Monitor.is_alive.return_value = True
        file_monitor_instance.reset_mock()
        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            nonblocking_arguments = mock_arguments()
            nonblocking_arguments.nonblocking = True
            command = incremental.Incremental(
                nonblocking_arguments, configuration, analysis_directory
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser",
                    "-project-root",
                    ".",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                    "-nonblocking",
                ],
            )

            command.run()
            call_client.assert_called_once_with(command=commands.Incremental.NAME)
            Monitor.is_alive.assert_called_once_with(".")
            Monitor.assert_not_called()
            file_monitor_instance.daemonize.assert_not_called()

        Monitor.reset_mock()
        Monitor.is_alive.return_value = True
        file_monitor_instance.reset_mock()
        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            transitive_arguments = mock_arguments()
            transitive_arguments.incremental_style = (
                commands.IncrementalStyle.TRANSITIVE
            )
            command = incremental.Incremental(
                transitive_arguments, configuration, analysis_directory
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser",
                    "-project-root",
                    ".",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                    "-transitive",
                ],
            )

            command.run()
            call_client.assert_called_once_with(command=commands.Incremental.NAME)
            Monitor.is_alive.assert_called_once_with(".")
            Monitor.assert_not_called()
            file_monitor_instance.daemonize.assert_not_called()

        commands_Command_state.return_value = commands.command.State.DEAD
        Monitor.reset_mock()
        file_monitor_instance.reset_mock()
        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            command = commands.Incremental(arguments, configuration, analysis_directory)
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser",
                    "-project-root",
                    ".",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )

            command.run()
            commands_Start.assert_called_with(
                arguments, configuration, analysis_directory
            )
            call_client.assert_has_calls(
                [call(command=commands.Incremental.NAME)], any_order=True
            )
            Monitor.assert_not_called()
            file_monitor_instance.daemonize.assert_not_called()

        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ), patch.object(SharedAnalysisDirectory, "prepare") as prepare:
            command = incremental.Incremental(
                arguments, configuration, analysis_directory
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser",
                    "-project-root",
                    ".",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )

            command.run()
            call_client.assert_called_once_with(command=commands.Incremental.NAME)
            Monitor.assert_not_called()
            file_monitor_instance.daemonize.assert_not_called()
            # Prepare only gets called when actually starting the server.
            prepare.assert_not_called()

        commands_Command_state.return_value = commands.command.State.DEAD
        arguments = mock_arguments(
            load_initial_state_from="/a/b", changed_files_path="/c/d"
        )
        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            command = commands.Incremental(arguments, configuration, analysis_directory)
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser",
                    "-project-root",
                    ".",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )

            command.run()
            commands_Start.assert_called_with(
                arguments, configuration, analysis_directory
            )
            call_client.assert_has_calls(
                [call(command=commands.Incremental.NAME)], any_order=True
            )
            Monitor.assert_not_called()
            file_monitor_instance.daemonize.assert_not_called()

        arguments = mock_arguments()
        arguments.original_directory = "/test"  # called from
        arguments.current_directory = "/"  # project root
        arguments.local_configuration = None

        configuration = mock_configuration()
        configuration.version_hash = "hash"
        analysis_directory = AnalysisDirectory(".")

        with patch.object(commands.Command, "_call_client") as call_client, patch(
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
            command = incremental.Incremental(
                arguments, configuration, analysis_directory
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser",
                    "-project-root",
                    "/",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2,path3",
                ],
            )

            command.run()
            call_client.assert_called_once_with(command=commands.Incremental.NAME)
            Monitor.assert_not_called()
            file_monitor_instance.daemonize.assert_not_called()
            self.assertEqual(command._exit_code, commands.ExitCode.FOUND_ERRORS)

        # If Start returns with an error, fail early
        start_exit_code.return_value = commands.ExitCode.FAILURE
        with patch.object(commands.Command, "_call_client") as call_client:
            command = incremental.Incremental(
                arguments, configuration, analysis_directory
            )
            command.run()
            call_client.assert_not_called()
            self.assertEqual(command._exit_code, commands.ExitCode.FAILURE)

    def test_read_stderr(self) -> None:
        with patch("subprocess.Popen") as popen:
            arguments = mock_arguments()

            configuration = mock_configuration()
            configuration.version_hash = "hash"
            analysis_directory = AnalysisDirectory("/root")
            command = incremental.Incremental(
                arguments, configuration, analysis_directory
            )
            stream = MagicMock()
            command._read_stderr(stream)
            popen.assert_called_once_with(
                ["tail", "--follow", "--lines=0", "/root/.pyre/server/server.stdout"],
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
            )
