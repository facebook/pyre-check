# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import io
import unittest
from typing import List
from unittest.mock import MagicMock, Mock, mock_open, patch

from ... import EnvironmentException  # noqa
from ... import commands  # noqa
from ...analysis_directory import AnalysisDirectory
from ..command import __name__ as client_name


def mock_arguments(  # noqa
    build=False,
    changed_files_path=None,
    command=None,  # noqa
    load_initial_state_from=None,
    no_saved_state=False,
    no_verify=False,
    no_watchman=False,
    output=None,
    save_initial_state_to=None,
    saved_state_project=None,
    source_directories=None,
    store_type_check_resolution=False,
    targets=None,
    terminal=False,
) -> MagicMock:
    arguments = MagicMock()
    arguments.additional_check = []
    arguments.analysis = "taint"
    arguments.build = build
    arguments.changed_files_path = changed_files_path
    arguments.command = command
    arguments.debug = False
    arguments.enable_profiling = False
    arguments.enable_memory_profiling = False
    arguments.filter_directory = ["."]
    arguments.hide_parse_errors = False
    arguments.incremental_style = commands.IncrementalStyle.SHALLOW
    arguments.load_initial_state_from = load_initial_state_from
    arguments.local = False
    arguments.local_configuration = None
    arguments.log_directory = ".pyre"
    arguments.log_identifier = None
    arguments.logger = None
    arguments.logging_sections = None
    arguments.no_saved_state = no_saved_state
    arguments.no_verify = no_verify
    arguments.no_watchman = no_watchman
    arguments.nonblocking = False
    arguments.output = output
    arguments.save_initial_state_to = save_initial_state_to
    arguments.save_results_to = None
    arguments.saved_state_project = saved_state_project
    arguments.sequential = False
    arguments.show_error_traces = False
    arguments.source_directories = source_directories
    arguments.store_type_check_resolution = store_type_check_resolution
    arguments.strict = False
    arguments.taint_models_path = []
    arguments.targets = targets
    arguments.terminal = terminal
    arguments.use_json_sockets = False
    arguments.verbose = False
    arguments.repository_root = None
    arguments.rule = None
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
    configuration.autocomplete = False
    configuration.log_directory = ".pyre"
    configuration._use_json_sockets = False
    configuration.disabled = False
    return configuration


class CommandTest(unittest.TestCase):
    @patch("os.getcwd", return_value="/original/directory")
    @patch("{}.switch_root".format(client_name), return_value=".")
    @patch("{}.find_local_root".format(client_name), return_value=None)
    def test_relative_path(self, find_local_root, switch_root, getcwd) -> None:
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

    @patch("os.getcwd", return_value="/original/directory")
    @patch("{}.switch_root".format(client_name), return_value=".")
    @patch("{}.find_local_root".format(client_name), return_value=None)
    def test_logger(self, find_local_root, switch_root, getcwd) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory(".")

        test_command = commands.Command(arguments, configuration, analysis_directory)
        self.assertEqual(
            test_command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-log-directory",
                ".pyre",
            ],
        )

        configuration.logger = "/foo/bar"
        test_command = commands.Command(arguments, configuration, analysis_directory)
        self.assertEqual(
            test_command._flags(),
            [
                "-logging-sections",
                "parser,-progress",
                "-project-root",
                ".",
                "-logger",
                "/foo/bar",
                "-log-directory",
                ".pyre",
            ],
        )

    @patch("os.path.isdir", Mock(return_value=True))
    @patch("os.listdir")
    def test_grofiling(self, os_listdir) -> None:
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
