# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import MagicMock, patch

from ... import commands  # noqa
from .command_test import mock_arguments, mock_configuration


class AnalyzeTest(unittest.TestCase):
    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    def test_analyze(self, directories_to_analyze, realpath, check_output) -> None:
        realpath.side_effect = lambda x: x
        arguments = mock_arguments()

        configuration = mock_configuration()
        configuration.get_typeshed.return_value = "stub"
        configuration.get_search_path.return_value = ["path1", "path2"]
        configuration.number_of_workers = 5

        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            result = MagicMock()
            result.output = ""
            call_client.return_value = result

            command = commands.Analyze(arguments, configuration, analysis_directory=".")
            self.assertEquals(
                command._flags(),
                [
                    "-project-root",
                    ".",
                    "-workers",
                    "5",
                    "-typeshed",
                    "stub",
                    "-search-path",
                    "path1,path2",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Analyze.NAME)
