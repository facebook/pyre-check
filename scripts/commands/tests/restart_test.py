# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import (
    MagicMock,
    patch,
)

from ... import commands
from ...commands import restart
from .command_test import (
    mock_arguments,
    mock_configuration,
)


class RestartTest(unittest.TestCase):
    @patch.object(restart, 'Stop')
    @patch.object(restart, 'Start')
    def test_restart(self, commands_Start, commands_Stop) -> None:
        state = MagicMock()
        state.running = ['.']

        arguments = mock_arguments()
        arguments.terminal = False

        configuration = mock_configuration()
        configuration.get_search_path.return_value = ['root']

        source_directory = '.'

        with patch.object(restart, 'Stop') as commands_Stop, \
                patch.object(restart, 'Start') as commands_Start, \
                patch.object(restart, 'Incremental') as commands_Incremental:
            commands.Restart(
                arguments,
                configuration,
                source_directory,
                blocking=False)._run()
            commands_Stop.assert_called_with(
                arguments,
                configuration,
                source_directory)
            commands_Start.assert_called_with(
                arguments,
                configuration,
                source_directory)
            commands_Incremental.assert_not_called()

        with patch.object(restart, 'Stop') as commands_Stop, \
                patch.object(restart, 'Start') as commands_Start, \
                patch.object(restart, 'Incremental') as commands_Incremental:
            commands.Restart(
                arguments,
                configuration,
                source_directory)._run()
            commands_Stop.assert_called_with(
                arguments,
                configuration,
                source_directory)
            commands_Incremental.assert_called_with(
                arguments,
                configuration,
                source_directory)
            commands_Start.assert_not_called()
