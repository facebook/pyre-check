# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

from ... import commands, find_directories, configuration as configuration_module
from ...analysis_directory import AnalysisDirectory
from .command_test import mock_arguments, mock_configuration


class AnalyzeTest(unittest.TestCase):
    @patch(
        f"{find_directories.__name__}.find_global_and_local_root",
        return_value=find_directories.FoundRoot(Path(".")),
    )
    @patch("subprocess.check_output")
    @patch("os.path.realpath")
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
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to=None,
                dump_call_graph="/call-graph",
                repository_root=None,
                rules=None,
                use_cache=False,
                inline_decorators=False,
                maximum_trace_length=None,
                maximum_tito_depth=None,
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
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                    "-analysis",
                    "taint",
                    "-dump-call-graph",
                    "/call-graph",
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
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to=None,
                dump_call_graph=None,
                repository_root=None,
                rules=None,
                use_cache=False,
                inline_decorators=False,
                maximum_trace_length=None,
                maximum_tito_depth=None,
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
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "taint_models",
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
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to=None,
                dump_call_graph=None,
                repository_root=None,
                rules=None,
                use_cache=False,
                inline_decorators=False,
                maximum_trace_length=None,
                maximum_tito_depth=None,
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
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "taint_models_1",
                    "-taint-models",
                    "taint_models_2",
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
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
                analysis="taint",
                taint_models_path=["overriding_models"],
                no_verify=False,
                save_results_to=None,
                dump_call_graph=None,
                repository_root=None,
                rules=None,
                use_cache=False,
                inline_decorators=False,
                maximum_trace_length=None,
                maximum_tito_depth=None,
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
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "overriding_models",
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
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
                analysis="taint",
                taint_models_path=["overriding_models"],
                no_verify=True,
                save_results_to=None,
                dump_call_graph="/call-graph",
                repository_root=None,
                rules=None,
                use_cache=False,
                inline_decorators=False,
                maximum_trace_length=None,
                maximum_tito_depth=None,
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
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "overriding_models",
                    "-dump-call-graph",
                    "/call-graph",
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
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to=".",
                dump_call_graph=None,
                repository_root=None,
                rules=None,
                use_cache=False,
                inline_decorators=False,
                maximum_trace_length=None,
                maximum_tito_depth=None,
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
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "taint_models",
                    "-save-results-to",
                    ".",
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
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to="/tmp/results.json",
                dump_call_graph=None,
                repository_root=None,
                rules=None,
                use_cache=False,
                inline_decorators=False,
                maximum_trace_length=None,
                maximum_tito_depth=None,
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
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "taint_models",
                    "-save-results-to",
                    "/tmp/results.json",
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
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to="/tmp/results.json",
                dump_call_graph="/call-graph",
                repository_root="/home/username/root",
                rules=None,
                use_cache=False,
                inline_decorators=False,
                maximum_trace_length=None,
                maximum_tito_depth=None,
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
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "taint_models",
                    "-save-results-to",
                    "/tmp/results.json",
                    "-dump-call-graph",
                    "/call-graph",
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
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to=None,
                dump_call_graph=None,
                repository_root=None,
                rules=[5021, 5022],
                use_cache=False,
                inline_decorators=False,
                maximum_trace_length=None,
                maximum_tito_depth=None,
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
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "taint_models",
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
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
                analysis="liveness",
                taint_models_path=[],
                no_verify=False,
                save_results_to=None,
                dump_call_graph=None,
                repository_root=None,
                rules=None,
                use_cache=False,
                inline_decorators=False,
                maximum_trace_length=None,
                maximum_tito_depth=None,
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
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                    "-analysis",
                    "liveness",
                    "-taint-models",
                    "taint_models",
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
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to=None,
                dump_call_graph=None,
                repository_root=None,
                rules=None,
                use_cache=True,
                inline_decorators=False,
                maximum_trace_length=None,
                maximum_tito_depth=None,
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
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "taint_models",
                    "-use-cache",
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
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to=None,
                dump_call_graph=None,
                repository_root=None,
                rules=None,
                use_cache=True,
                inline_decorators=True,
                maximum_trace_length=None,
                maximum_tito_depth=None,
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
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "taint_models",
                    "-use-cache",
                    "-inline-decorators",
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
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to=None,
                dump_call_graph=None,
                repository_root=None,
                rules=None,
                use_cache=False,
                inline_decorators=False,
                maximum_trace_length=2,
                maximum_tito_depth=None,
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
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "taint_models",
                    "-maximum-trace-length",
                    "2",
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
                analysis_directory=AnalysisDirectory(
                    configuration_module.SimpleSearchPathElement(".")
                ),
                analysis="taint",
                taint_models_path=[],
                no_verify=False,
                save_results_to=None,
                dump_call_graph=None,
                repository_root=None,
                rules=None,
                use_cache=False,
                inline_decorators=False,
                maximum_trace_length=None,
                maximum_tito_depth=3,
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
                    "-python-major-version",
                    "3",
                    "-python-minor-version",
                    "6",
                    "-python-micro-version",
                    "0",
                    "-shared-memory-heap-size",
                    "1073741824",
                    "-workers",
                    "5",
                    "-analysis",
                    "taint",
                    "-taint-models",
                    "taint_models",
                    "-maximum-tito-depth",
                    "3",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Analyze.NAME)
