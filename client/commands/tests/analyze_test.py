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
        arguments.taint_models_path = None

        configuration = mock_configuration()
        configuration.taint_models_path = None

        result = MagicMock()
        result.output = ""

        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            command = commands.Analyze(arguments, configuration, ".")
            self.assertEqual(
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
                    "-dump-call-graph",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Analyze.NAME)

        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            configuration.taint_models_path = "taint_models"
            command = commands.Analyze(arguments, configuration, ".")
            self.assertEqual(
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
                    "-taint-models",
                    "taint_models",
                    "-dump-call-graph",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Analyze.NAME)

        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            configuration.taint_models_path = "taint_models"
            arguments.taint_models_path = "overriding_models"
            command = commands.Analyze(arguments, configuration, ".")
            self.assertEqual(
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
                    "-taint-models",
                    "overriding_models",
                    "-dump-call-graph",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Analyze.NAME)

        arguments = mock_arguments()
        arguments.save_results_to = "/tmp/results.json"
        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            command = commands.Analyze(arguments, configuration, ".")
            self.assertEqual(
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
                    "-taint-models",
                    "taint_models",
                    "-save-results-to",
                    "/tmp/results.json",
                    "-dump-call-graph",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Analyze.NAME)
