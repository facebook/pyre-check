# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from pathlib import Path
from unittest.mock import MagicMock, Mock, patch

from ... import commands, find_directories
from ...analysis_directory import AnalysisDirectory
from ...commands import check
from .command_test import mock_arguments, mock_configuration


_typeshed_search_path: str = "{}.typeshed_search_path".format(check.__name__)


class AnalyzeTest(unittest.TestCase):
    @patch(
        f"{find_directories.__name__}.find_global_and_local_root",
        return_value=find_directories.FoundRoot(Path(".")),
    )
    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch(_typeshed_search_path, Mock(return_value=["path3"]))
    # pyre-fixme[56]: Argument `set()` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    def test_analyze(
        self, directories_to_analyze, realpath, check_output, find_global_and_local_root
    ) -> None:
        realpath.side_effect = lambda x: x
        arguments = mock_arguments()

        configuration = mock_configuration()
        configuration.taint_models_path = []

        original_directory = "/original/directory"

        result = MagicMock()
        result.output = ""

        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            command = commands.Analyze(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory("."),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to=None,
                dump_call_graph=True,
                repository_root=None,
                rules=None,
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-source-path",
                    ".",
                    "-search-path",
                    "path3",
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
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory("."),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to=None,
                dump_call_graph=True,
                repository_root=None,
                rules=None,
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-source-path",
                    ".",
                    "-search-path",
                    "path3",
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
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory("."),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to=None,
                dump_call_graph=True,
                repository_root=None,
                rules=None,
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-source-path",
                    ".",
                    "-search-path",
                    "path3",
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
            command = commands.Analyze(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory("."),
                analysis="taint",
                taint_models_path=["overriding_models"],
                no_verify=False,
                save_results_to=None,
                dump_call_graph=True,
                repository_root=None,
                rules=None,
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-source-path",
                    ".",
                    "-search-path",
                    "path3",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "overriding_models",
                    "-dump-call-graph",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Analyze.NAME)

        arguments = mock_arguments()
        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            configuration.taint_models_path = {"taint_models"}
            command = commands.Analyze(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory("."),
                analysis="taint",
                taint_models_path=["overriding_models"],
                no_verify=True,
                save_results_to=None,
                dump_call_graph=True,
                repository_root=None,
                rules=None,
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-source-path",
                    ".",
                    "-search-path",
                    "path3",
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
        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            command = commands.Analyze(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory("."),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to=".",
                dump_call_graph=True,
                repository_root=None,
                rules=None,
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-source-path",
                    ".",
                    "-search-path",
                    "path3",
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
        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            command = commands.Analyze(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory("."),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to="/tmp/results.json",
                dump_call_graph=True,
                repository_root=None,
                rules=None,
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-source-path",
                    ".",
                    "-search-path",
                    "path3",
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
        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            command = commands.Analyze(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory("."),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to="/tmp/results.json",
                dump_call_graph=True,
                repository_root="/home/username/root",
                rules=None,
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-source-path",
                    ".",
                    "-search-path",
                    "path3",
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

        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            command = commands.Analyze(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory("."),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to=None,
                dump_call_graph=True,
                repository_root=None,
                rules=[5021, 5022],
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-source-path",
                    ".",
                    "-search-path",
                    "path3",
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
        with patch.object(
            commands.Command, "_call_client", return_value=result
        ) as call_client, patch("json.loads", return_value=[]):
            command = commands.Analyze(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory("."),
                analysis="liveness",
                taint_models_path=[],
                no_verify=False,
                save_results_to=None,
                dump_call_graph=True,
                repository_root=None,
                rules=None,
            )
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-log-directory",
                    ".pyre",
                    "-workers",
                    "5",
                    "-source-path",
                    ".",
                    "-search-path",
                    "path3",
                    "-analysis",
                    "liveness",
                    "-taint-models",
                    "taint_models",
                    "-dump-call-graph",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Analyze.NAME)
