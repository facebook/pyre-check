# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

from ... import commands, configuration as configuration_module, find_directories
from ...analysis_directory import AnalysisDirectory
from ...process import Process
from ...tests.mocks import mock_arguments, mock_configuration


class CommandTest(unittest.TestCase):
    @patch(
        f"{find_directories.__name__}.find_global_and_local_root",
        return_value=find_directories.FoundRoot(Path(".")),
    )
    def test_relative_path(self, find_global_and_local_root) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement(".")
        )
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

    @patch.object(Process, "is_alive", return_value=True)
    def test_state__alive(self, is_alive: MagicMock) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement(".")
        )
        original_directory = "/original/directory"
        self.assertEqual(
            commands.Command(
                arguments, original_directory, configuration, analysis_directory
            )._state(),
            commands.command.State.RUNNING,
        )

    @patch.object(Process, "is_alive", return_value=False)
    def test_state__dead(self, is_alive: MagicMock) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement(".")
        )
        original_directory = "/original/directory"
        self.assertEqual(
            commands.Command(
                arguments, original_directory, configuration, analysis_directory
            )._state(),
            commands.command.State.DEAD,
        )

    @patch(
        f"{find_directories.__name__}.find_global_and_local_root",
        return_value=find_directories.FoundRoot(Path(".")),
    )
    def test_logger(self, find_global_and_local_root) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement(".")
        )
        original_directory = "/original/directory"

        test_command = commands.Command(
            arguments, original_directory, configuration, analysis_directory
        )
        self.assertEqual(
            test_command._flags(),
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
                "-progress",
                "-project-root",
                "/root",
                "-logger",
                "/foo/bar",
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
            ],
        )
        with patch.object(
            configuration_module, "create_configuration", return_value=configuration
        ):
            test_command = commands.Command(
                arguments,
                original_directory=original_directory,
                configuration=configuration,
                analysis_directory=analysis_directory,
            )
            self.assertEqual(
                test_command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    "/root",
                    "-logger",
                    "/foo/bar",
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
                ],
            )
