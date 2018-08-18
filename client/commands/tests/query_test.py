# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import MagicMock, patch

from ... import commands  # noqa
from .command_test import mock_arguments, mock_configuration


class QueryTest(unittest.TestCase):
    def test_query(self) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()

        with patch.object(commands.Command, "_call_client") as call_client:
            result = MagicMock()
            result.output = "{}"
            call_client.return_value = result

            commands.Query(arguments, configuration, analysis_directory=".").run()
            call_client.assert_called_once_with(command=commands.Query.NAME)
