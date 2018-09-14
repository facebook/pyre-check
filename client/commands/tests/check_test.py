# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import MagicMock, patch

from ... import commands  # noqa
from ...filesystem import AnalysisDirectory
from .command_test import mock_arguments, mock_configuration


class CheckTest(unittest.TestCase):
    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    def test_check(self, get_directories_to_analyze, realpath, check_output) -> None:
        realpath.side_effect = lambda x: x

        arguments = mock_arguments()
        configuration = mock_configuration()

        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            command = commands.Check(arguments, configuration, AnalysisDirectory("."))
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
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Check.NAME)

        shared_analysis_directory = MagicMock()
        shared_analysis_directory.get_root = lambda: "."
        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ), patch.object(shared_analysis_directory, "prepare") as prepare:
            command = commands.Check(
                arguments, configuration, shared_analysis_directory
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Check.NAME)
            prepare.assert_called_once_with()

    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    def test_sequential_check(
        self, directories_to_analyze, realpath, check_output
    ) -> None:
        realpath.side_effect = lambda x: x

        arguments = mock_arguments()
        arguments.sequential = True
        configuration = mock_configuration()

        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            command = commands.Check(arguments, configuration, AnalysisDirectory("."))
            self.assertEqual(
                command._flags(),
                [
                    "-sequential",
                    "-project-root",
                    ".",
                    "-workers",
                    "5",
                    "-typeshed",
                    "stub",
                    "-search-path",
                    "path1,path2",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Check.NAME)

    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch.object(
        commands.Reporting, "_get_directories_to_analyze", return_value=set(["a", "b"])
    )
    def test_filter_directories(
        self, directories_to_analyze, realpath, check_output
    ) -> None:
        realpath.side_effect = lambda x: x

        arguments = mock_arguments()
        arguments.sequential = True
        configuration = mock_configuration()

        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            command = commands.Check(arguments, configuration, AnalysisDirectory("."))
            self.assertEqual(
                command._flags(),
                [
                    "-sequential",
                    "-project-root",
                    ".",
                    "-filter-directories",
                    "a;b",
                    "-workers",
                    "5",
                    "-typeshed",
                    "stub",
                    "-search-path",
                    "path1,path2",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Check.NAME)

    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    def test_check_dumb_terminal(
        self, directories_to_analyze, realpath, check_output
    ) -> None:
        realpath.side_effect = lambda x: x

        arguments = mock_arguments()
        arguments.capable_terminal = False
        configuration = mock_configuration()

        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            command = commands.Check(arguments, configuration, AnalysisDirectory("."))
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "-progress",
                    "-project-root",
                    ".",
                    "-workers",
                    "5",
                    "-typeshed",
                    "stub",
                    "-search-path",
                    "path1,path2",
                ],
            )
            exit_code = command.run().exit_code()
            self.assertEqual(exit_code, 0)
            call_client.assert_called_once_with(command=commands.Check.NAME)

    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    def test_check_show_parse_errors(
        self, directories_to_analyze, realpath, check_output
    ) -> None:
        realpath.side_effect = lambda x: x

        arguments = mock_arguments()
        arguments.show_parse_errors = True
        configuration = mock_configuration()

        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            command = commands.Check(arguments, configuration, AnalysisDirectory("."))
            self.assertEqual(
                command._flags(),
                [
                    "-logging-sections",
                    "parser",
                    "-project-root",
                    ".",
                    "-workers",
                    "5",
                    "-typeshed",
                    "stub",
                    "-search-path",
                    "path1,path2",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Check.NAME)
