# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import Mock, call, patch

from ... import commands  # noqa
from .command_test import mock_arguments, mock_configuration


class StopTest(unittest.TestCase):
    @patch.object(commands.Kill, "_run")
    @patch.object(commands.Command, "_state")
    def test_stop(self, commands_Command_state, kill_run) -> None:
        arguments = mock_arguments()
        arguments.terminal = False

        configuration = mock_configuration()
        configuration.get_typeshed.return_value = "stub"
        configuration.get_search_path.return_value = ["path1", "path2"]

        # Check start without watchman.
        commands_Command_state.return_value = commands.command.State.RUNNING
        with patch.object(commands.Command, "_call_client") as call_client:
            commands.Stop(arguments, configuration, analysis_directory=".").run()
            call_client.assert_called_once_with(command=commands.Stop.NAME)

        commands_Command_state.return_value = commands.command.State.DEAD
        with patch.object(commands.Command, "_call_client") as call_client:
            commands.Stop(arguments, configuration, analysis_directory=".").run()
            call_client.assert_not_called()

        commands_Command_state.return_value = commands.command.State.RUNNING
        with patch.object(commands.Command, "_call_client") as call_client:

            def fail_on_stop(command, flags=None):
                flags = flags or []
                if command == commands.Stop.NAME:
                    raise commands.ClientException
                return Mock()

            call_client.side_effect = fail_on_stop
            commands.Stop(arguments, configuration, analysis_directory=".").run()
            call_client.assert_has_calls([call(command=commands.Stop.NAME)])
            kill_run.assert_has_calls([call()])
