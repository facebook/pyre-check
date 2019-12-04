# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from unittest.mock import MagicMock, Mock, patch

from ... import commands  # noqa
from ...analysis_directory import AnalysisDirectory
from ...commands import check
from ..command import __name__ as client_name
from .command_test import mock_arguments, mock_configuration


_typeshed_search_path: str = "{}.typeshed_search_path".format(check.__name__)


class AnalyzeTest(unittest.TestCase):
    @patch("{}.find_project_root".format(client_name), return_value=".")
    @patch("{}.find_local_root".format(client_name), return_value=None)
    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch(_typeshed_search_path, Mock(return_value=["path3"]))
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    def test_analyze(
        self,
        directories_to_analyze,
        realpath,
        check_output,
        find_local_root,
        find_project_root,
    ) -> None:
        realpath.side_effect = lambda x: x
        arguments = mock_arguments()
        arguments.taint_models_path = []

        configuration = mock_configuration()
        configuration.taint_models_path = []

        original_directory = "/original/directory"

        result = MagicMock()
        result.output = ""

        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            command = commands.Analyze(
                arguments, original_directory, configuration, AnalysisDirectory(".")
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-search-path",
                    "path1,path2,path3",
                    "-analysis",
                    "taint",
                    "-dump-call-graph",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Analyze.NAME)

        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            configuration.taint_models_path = ["taint_models"]
            command = commands.Analyze(
                arguments, original_directory, configuration, AnalysisDirectory(".")
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-search-path",
                    "path1,path2,path3",
                    "-analysis",
                    "taint",
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
            configuration.taint_models_path = ["taint_models_1", "taint_models_2"]
            command = commands.Analyze(
                arguments, original_directory, configuration, AnalysisDirectory(".")
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-search-path",
                    "path1,path2,path3",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "taint_models_1",
                    "-taint-models",
                    "taint_models_2",
                    "-dump-call-graph",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Analyze.NAME)

        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            configuration.taint_models_path = {"taint_models"}
            arguments.taint_models_path = {"overriding_models"}
            command = commands.Analyze(
                arguments, original_directory, configuration, AnalysisDirectory(".")
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-search-path",
                    "path1,path2,path3",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "overriding_models",
                    "-dump-call-graph",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Analyze.NAME)

        arguments = mock_arguments(no_verify=True)
        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            configuration.taint_models_path = {"taint_models"}
            arguments.taint_models_path = {"overriding_models"}
            command = commands.Analyze(
                arguments, original_directory, configuration, AnalysisDirectory(".")
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-search-path",
                    "path1,path2,path3",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "overriding_models",
                    "-dump-call-graph",
                    "-no-verify",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Analyze.NAME)

        # Test "." is a valid directory
        arguments = mock_arguments()
        arguments.save_results_to = "."
        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            command = commands.Analyze(
                arguments, original_directory, configuration, AnalysisDirectory(".")
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-search-path",
                    "path1,path2,path3",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "taint_models",
                    "-save-results-to",
                    ".",
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
            command = commands.Analyze(
                arguments, original_directory, configuration, AnalysisDirectory(".")
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-search-path",
                    "path1,path2,path3",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "taint_models",
                    "-save-results-to",
                    "/tmp/results.json",
                    "-dump-call-graph",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Analyze.NAME)

        arguments = mock_arguments()
        arguments.save_results_to = "/tmp/results.json"
        arguments.repository_root = "/home/username/root"
        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            command = commands.Analyze(
                arguments, original_directory, configuration, AnalysisDirectory(".")
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-search-path",
                    "path1,path2,path3",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "taint_models",
                    "-save-results-to",
                    "/tmp/results.json",
                    "-dump-call-graph",
                    "-repository-root",
                    "/home/username/root",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Analyze.NAME)

        arguments = mock_arguments()
        arguments.rule = [5021, 5022]
        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            command = commands.Analyze(
                arguments, original_directory, configuration, AnalysisDirectory(".")
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-search-path",
                    "path1,path2,path3",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "taint_models",
                    "-dump-call-graph",
                    "-rules",
                    "5021,5022",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Analyze.NAME)

        arguments = mock_arguments()
        arguments.analysis = "liveness"
        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            command = commands.Analyze(
                arguments, original_directory, configuration, AnalysisDirectory(".")
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser,-progress",
                    "-project-root",
                    ".",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-search-path",
                    "path1,path2,path3",
                    "-analysis",
                    "liveness",
                    "-taint-models",
                    "taint_models",
                    "-dump-call-graph",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Analyze.NAME)
