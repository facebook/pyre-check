# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import (
    MagicMock,
    patch,
)

from ... import commands  # noqa
from .command_test import (
    mock_arguments,
    mock_configuration,
)


class AnalyzeTest(unittest.TestCase):
    @patch('subprocess.check_output')
    @patch('os.path.realpath')
    def test_analyze(self, realpath, check_output) -> None:
        realpath.side_effect = lambda x: x
        arguments = mock_arguments()
        arguments.number_of_workers = 5

        configuration = mock_configuration()
        configuration.get_search_path.return_value = ['stub', 'root']

        with patch.object(commands.Command, '_call_client') as call_client, \
                patch('json.loads', return_value=[]):
            result = MagicMock()
            result.output = ''
            call_client.return_value = result

            commands.Analyze(
                arguments,
                configuration,
                source_directory='.').run()
            call_client.assert_called_once_with(
                command=commands.Check.NAME,
                flags=[
                    '-project-root',
                    '.',
                    '-workers',
                    '5',
                    '-search-path',
                    'stub,root',
                    '-analyze',
                ])
