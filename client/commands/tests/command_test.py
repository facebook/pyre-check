# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import io
import unittest
from typing import List
from unittest.mock import MagicMock, Mock, mock_open, patch

from ... import EnvironmentException  # noqa
from ... import commands  # noqa
from ...filesystem import AnalysisDirectory


def mock_arguments(
    no_watchman=False,
    terminal=False,
    saved_state_project=None,
    save_initial_state_to=None,
    load_initial_state_from=None,
    changed_files_path=None,
    no_saved_state=False,
    original_directory=None,
    show_parse_errors=False,
    output=None,
    command=None,
    build=False,
    source_directories=None,
    targets=None,
    store_type_check_resolution=False,
) -> MagicMock:
    arguments = MagicMock()
    arguments.build = build
    arguments.command = command
    arguments.current_directory = "."
    arguments.debug = False
    arguments.no_saved_state = no_saved_state
    arguments.additional_check = []
    arguments.filter_directory = ["."]
    arguments.local = False
    arguments.local_configuration = None
    arguments.log_identifier = None
    arguments.logger = None
    arguments.logging_sections = None
    arguments.enable_profiling = None
    arguments.no_watchman = no_watchman
    arguments.original_directory = original_directory or "/original/directory/"
    arguments.output = output
    arguments.saved_state_project = saved_state_project
    arguments.save_initial_state_to = save_initial_state_to
    arguments.load_initial_state_from = load_initial_state_from
    arguments.changed_files_path = changed_files_path
    arguments.save_results_to = None
    arguments.sequential = False
    arguments.show_error_traces = False
    arguments.source_directories = source_directories
    arguments.hide_parse_errors = False
    arguments.strict = False
    arguments.taint_models_path = []
    arguments.targets = targets
    arguments.terminal = terminal
    arguments.verbose = False
    arguments.nonblocking = False
    arguments.transitive = False
    arguments.store_type_check_resolution = store_type_check_resolution
    return arguments


def mock_configuration(version_hash=None, file_hash=None) -> MagicMock:
    configuration = MagicMock()
    configuration.strict = False
    configuration.source_directories = ["."]
    configuration.logger = None
    configuration.number_of_workers = 5
    configuration.search_path = ["path1", "path2"]
    configuration.taint_models_path = []
    configuration.typeshed = "stub"
    configuration.version_hash = version_hash
    configuration.file_hash = file_hash
    configuration.local_configuration_root = None
    return configuration


class CommandTest(unittest.TestCase):
    def test_relative_path(self) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory(".")
        self.assertEqual(
            commands.Command(
                arguments, configuration, analysis_directory
            )._relative_path("/original/directory/path"),
            "path",
        )
        self.assertEqual(
            commands.Command(
                arguments, configuration, analysis_directory
            )._relative_path("/original/directory/"),
            ".",
        )

    @patch("os.kill")
    def test_state(self, os_kill) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()

        with patch("builtins.open", mock_open()) as open:
            open.side_effect = [io.StringIO("1")]
            self.assertEqual(
                commands.Command(
                    arguments, configuration, AnalysisDirectory(".")
                )._state(),
                commands.command.State.RUNNING,
            )

        with patch("builtins.open", mock_open()) as open:
            open.side_effect = [io.StringIO("derp")]
            self.assertEqual(
                commands.Command(
                    arguments, configuration, AnalysisDirectory(".")
                )._state(),
                commands.command.State.DEAD,
            )

    def test_logger(self) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory(".")

        command = commands.Command(arguments, configuration, analysis_directory)
        self.assertEqual(
            command._flags(), ["-logging-sections", "parser", "-project-root", "."]
        )

        configuration.logger = "/foo/bar"
        command = commands.Command(arguments, configuration, analysis_directory)
        self.assertEqual(
            command._flags(),
            [
                "-logging-sections",
                "parser",
                "-project-root",
                ".",
                "-logger",
                "/foo/bar",
            ],
        )

    @patch("os.path.isdir", Mock(return_value=True))
    @patch("os.listdir")
    def test_profiling(self, os_listdir) -> None:
        # Mock typeshed file hierarchy
        def mock_listdir(path: str) -> List[str]:
            if path == "root/stdlib":
                return ["2.7", "2", "2and3", "3.5", "3.6", "3.7", "3"]
            elif path == "root/third_party":
                return ["3", "3.5", "2", "2and3"]
            else:
                raise RuntimeError("Path not expected by mock listdir")

        os_listdir.side_effect = mock_listdir
        self.assertEqual(
            commands.typeshed_search_path("root"),
            [
                "root/stdlib/3.7",
                "root/stdlib/3.6",
                "root/stdlib/3.5",
                "root/stdlib/3",
                "root/stdlib/2and3",
                "root/third_party/3.5",
                "root/third_party/3",
                "root/third_party/2and3",
            ],
        )
