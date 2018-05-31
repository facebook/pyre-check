# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import MagicMock, call, patch

from ... import commands
from ...commands import incremental
from .command_test import mock_arguments, mock_configuration


class IncrementalTest(unittest.TestCase):
    @patch.object(commands.Command, "_state")
    @patch.object(incremental, "Start")
    @patch.object(incremental, "Stop")
    def test_incremental(
        self, commands_Stop, commands_Start, commands_Command_state
    ) -> None:
        state = MagicMock()
        state.running = ["running"]
        state.dead = []
        commands_Command_state.return_value = state

        arguments = mock_arguments()

        configuration = mock_configuration()
        configuration.get_search_path.return_value = ["stub", "root"]
        configuration.get_version_hash.return_value = "hash"

        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            incremental.Incremental(
                arguments, configuration, source_directory="."
            ).run()
            call_client.assert_called_once_with(
                command=commands.Incremental.NAME,
                flags=[
                    "-project-root",
                    ".",
                    "-search-path",
                    "stub,root",
                    "-expected-binary-version",
                    "hash",
                ],
            )

        commands_Command_state.return_value = commands.command.State.DEAD
        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            commands.Incremental(arguments, configuration, source_directory=".").run()
            commands_Start.assert_called_with(arguments, configuration, ".")
            call_client.assert_has_calls(
                [
                    call(
                        command=commands.Incremental.NAME,
                        flags=[
                            "-project-root",
                            ".",
                            "-search-path",
                            "stub,root",
                            "-expected-binary-version",
                            "hash",
                        ],
                    )
                ],
                any_order=True,
            )
