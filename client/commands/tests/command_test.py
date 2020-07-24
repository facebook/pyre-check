# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import argparse
import unittest
from typing import List
from unittest.mock import MagicMock, Mock, patch

from ... import commands
from ...analysis_directory import AnalysisDirectory
from ...process import Process
from ...tests.mocks import mock_arguments, mock_configuration
from ..command import __name__ as client_name


class CommandTest(unittest.TestCase):
    @patch("{}.find_project_root".format(client_name), return_value=".")
    # pyre-fixme[56]: Argument
    #  `"{}.find_local_root".format(tools.pyre.client.commands.command.__name__)` to
    #  decorator factory `unittest.mock.patch` could not be resolved in a global scope.
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

    @patch.object(Process, "is_alive", return_value=True)
    def test_state__alive(self, is_alive: MagicMock) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        analysis_directory = AnalysisDirectory(".")
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
        analysis_directory = AnalysisDirectory(".")
        original_directory = "/original/directory"
        self.assertEqual(
            commands.Command(
                arguments, original_directory, configuration, analysis_directory
            )._state(),
            commands.command.State.DEAD,
        )

    @patch("{}.find_project_root".format(client_name), return_value=".")
    # pyre-fixme[56]: Argument
    #  `"{}.find_local_root".format(tools.pyre.client.commands.command.__name__)` to
    #  decorator factory `unittest.mock.patch` could not be resolved in a global scope.
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

    def test_argument_parsing(self) -> None:
        parser = argparse.ArgumentParser()
        commands.Command.add_arguments(parser)
        self.assertEqual(
            parser.parse_args(["--use-buck-builder"]).use_buck_builder, True
        )
        self.assertEqual(
            parser.parse_args(["--use-legacy-buck-builder"]).use_buck_builder, False
        )
        self.assertEqual(parser.parse_args([]).use_buck_builder, None)
