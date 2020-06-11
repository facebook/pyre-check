# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import io
import unittest
from pathlib import Path
from typing import List, Optional
from unittest.mock import MagicMock, Mock, mock_open, patch

from ... import commands
from ...analysis_directory import AnalysisDirectory
from ...tests.mocks import mock_arguments, mock_configuration
from ..command import __name__ as client_name


class CommandTest(unittest.TestCase):
    @patch("{}.find_project_root".format(client_name), return_value=".")
    @patch("{}.find_local_root".format(client_name), return_value=None)
    def test_relative_path(self, find_local_root, find_project_root) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory(".")
        original_directory = "/original/directory"
        self.assertEqual(
            commands.Command(
                arguments, original_directory, configuration, analysis_directory
            )._relative_path("/original/directory/path"),
            "path",
        )
        self.assertEqual(
            commands.Command(
                arguments, original_directory, configuration, analysis_directory
            )._relative_path("/original/directory/"),
            ".",
        )

    @patch("os.kill")
    def test_state(self, os_kill) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        original_directory = "/original/directory"

        with patch("builtins.open", mock_open()) as open:
            open.side_effect = [io.StringIO("1")]
            self.assertEqual(
                commands.Command(
                    arguments, original_directory, configuration, AnalysisDirectory(".")
                )._state(),
                commands.command.State.RUNNING,
            )

        with patch("builtins.open", mock_open()) as open:
            open.side_effect = [io.StringIO("derp")]
            self.assertEqual(
                commands.Command(
                    arguments, original_directory, configuration, AnalysisDirectory(".")
                )._state(),
                commands.command.State.DEAD,
            )

    @patch("{}.find_project_root".format(client_name), return_value=".")
    @patch("{}.find_local_root".format(client_name), return_value=None)
    def test_logger(self, find_local_root, find_project_root) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory(".")
        original_directory = "/original/directory"

        test_command = commands.Command(
            arguments, original_directory, configuration, analysis_directory
        )
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
        test_command = commands.Command(
            arguments, original_directory, configuration, analysis_directory
        )
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
        with patch.object(
            commands.Command, "generate_configuration", return_value=configuration
        ):
            test_command = commands.Command(
                arguments,
                original_directory=original_directory,
                configuration=None,
                analysis_directory=analysis_directory,
            )
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
