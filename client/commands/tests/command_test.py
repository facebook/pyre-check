# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import io
import unittest
from unittest.mock import MagicMock, mock_open, patch

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
) -> MagicMock:
    arguments = MagicMock()
    arguments.current_directory = "."
    arguments.debug = False
    arguments.run_additional_checks = False
    arguments.filter_directories = ["."]
    arguments.local = False
    arguments.local_configuration = None
    arguments.log_identifier = None
    arguments.logger = None
    arguments.logging_sections = None
    arguments.no_watchman = no_watchman
    arguments.original_directory = "/original/directory/"
    arguments.saved_state_project = saved_state_project
    arguments.save_initial_state_to = save_initial_state_to
    arguments.load_initial_state_from = load_initial_state_from
    arguments.changed_files_path = changed_files_path
    arguments.save_results_to = None
    arguments.sequential = False
    arguments.show_error_traces = False
    arguments.show_parse_errors = False
    arguments.strict = False
    arguments.taint_models_path = None
    arguments.terminal = terminal
    arguments.verbose = False
    return arguments


def mock_configuration(version_hash=None) -> MagicMock:
    configuration = MagicMock()
    configuration.source_directories = ["."]
    configuration.logger = None
    configuration.number_of_workers = 5
    configuration.search_path = ["path1", "path2"]
    configuration.taint_models_path = None
    configuration.typeshed = "stub"
    configuration.version_hash = version_hash
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
        self.assertEqual(command._flags(), ["-project-root", "."])

        configuration.logger = "/foo/bar"
        command = commands.Command(arguments, configuration, analysis_directory)
        self.assertEqual(
            command._flags(), ["-project-root", ".", "-logger", "/foo/bar"]
        )
