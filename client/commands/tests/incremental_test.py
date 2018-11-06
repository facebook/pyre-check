# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import MagicMock, call, patch

from ... import commands
from ...commands import stop  # noqa
from ...commands import incremental
from ...filesystem import AnalysisDirectory, SharedAnalysisDirectory
from .command_test import mock_arguments, mock_configuration


class IncrementalTest(unittest.TestCase):
    @patch.object(commands.Command, "_state")
    @patch.object(incremental, "Start")
    @patch.object(stop, "Stop")
    def test_incremental(
        self, commands_Stop, commands_Start, commands_Command_state
    ) -> None:
        state = MagicMock()
        state.running = ["running"]
        state.dead = []
        commands_Command_state.return_value = state

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
                    "-project-root",
                    ".",
                    "-typeshed",
                    "stub",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2",
                ],
            )

            command.run()
            call_client.assert_called_once_with(command=commands.Incremental.NAME)

        commands_Command_state.return_value = commands.command.State.DEAD
        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            command = commands.Incremental(arguments, configuration, analysis_directory)
            self.assertEqual(
                command._flags(),
                [
                    "-project-root",
                    ".",
                    "-typeshed",
                    "stub",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2",
                ],
            )

            command.run()
            commands_Start.assert_called_with(
                arguments, configuration, analysis_directory
            )
            call_client.assert_has_calls(
                [call(command=commands.Incremental.NAME)], any_order=True
            )

        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ), patch.object(SharedAnalysisDirectory, "prepare") as prepare:
            command = incremental.Incremental(
                arguments, configuration, analysis_directory
            )
            self.assertEqual(
                command._flags(),
                [
                    "-project-root",
                    ".",
                    "-typeshed",
                    "stub",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2",
                ],
            )

            command.run()
            call_client.assert_called_once_with(command=commands.Incremental.NAME)
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
                    "-project-root",
                    ".",
                    "-typeshed",
                    "stub",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2",
                ],
            )

            command.run()
            commands_Start.assert_called_with(
                arguments, configuration, analysis_directory
            )
            call_client.assert_has_calls(
                [call(command=commands.Incremental.NAME)], any_order=True
            )
        arguments = mock_arguments()

        configuration = mock_configuration()
        configuration.version_hash = "hash"
        analysis_directory = AnalysisDirectory(".")

        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads",
            return_value=[
                {
                    "line": 4,
                    "column": 11,
                    "path": "c.py",
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
                    "-project-root",
                    ".",
                    "-typeshed",
                    "stub",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2",
                ],
            )

            command.run()
            call_client.assert_called_once_with(command=commands.Incremental.NAME)
            self.assertEqual(command._exit_code, commands.ExitCode.FOUND_ERRORS)
