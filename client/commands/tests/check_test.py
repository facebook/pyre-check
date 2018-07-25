# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import patch

from ... import commands  # noqa
from .command_test import mock_arguments, mock_configuration


class CheckTest(unittest.TestCase):
    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch.object(
        commands.ErrorHandling, "_get_directories_to_analyze", return_value=set()
    )
    def test_check(self, get_directories_to_analyze, realpath, check_output) -> None:
        realpath.side_effect = lambda x: x
        arguments = mock_arguments()

        configuration = mock_configuration()
        configuration.get_typeshed.return_value = "stub"
        configuration.get_search_path.return_value = ["path1", "path2"]
        configuration.number_of_workers = 5

        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            commands.Check(arguments, configuration, source_directory=".").run()
            call_client.assert_called_once_with(
                command=commands.Check.NAME,
                flags=[
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

    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch.object(
        commands.ErrorHandling, "_get_directories_to_analyze", return_value=set()
    )
    def test_sequential_check(
        self, directories_to_analyze, realpath, check_output
    ) -> None:
        realpath.side_effect = lambda x: x
        arguments = mock_arguments()
        arguments.sequential = True

        configuration = mock_configuration()
        configuration.get_typeshed.return_value = "stub"
        configuration.get_search_path.return_value = ["path1", "path2"]
        configuration.number_of_workers = 5

        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            commands.Check(arguments, configuration, source_directory=".").run()
            call_client.assert_called_once_with(
                command=commands.Check.NAME,
                flags=[
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

    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch.object(
        commands.ErrorHandling, "_get_directories_to_analyze", return_value=set()
    )
    def test_check_dumb_terminal(
        self, directories_to_analyze, realpath, check_output
    ) -> None:
        realpath.side_effect = lambda x: x
        arguments = mock_arguments()
        arguments.capable_terminal = False

        configuration = mock_configuration()
        configuration.get_typeshed.return_value = "stub"
        configuration.get_search_path.return_value = ["path1", "path2"]
        configuration.number_of_workers = 5

        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            exit_code = commands.Check(
                arguments, configuration, source_directory="."
            ).run()
            self.assertEqual(exit_code, 0)
            call_client.assert_called_once_with(
                command=commands.Check.NAME,
                flags=[
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

    @patch("subprocess.check_output")
    @patch("os.path.realpath")
    @patch.object(
        commands.ErrorHandling, "_get_directories_to_analyze", return_value=set()
    )
    def test_check_show_parse_errors(
        self, directories_to_analyze, realpath, check_output
    ) -> None:
        realpath.side_effect = lambda x: x
        arguments = mock_arguments()
        arguments.show_parse_errors = True

        configuration = mock_configuration()
        configuration.get_typeshed.return_value = "stub"
        configuration.get_search_path.return_value = ["path1", "path2"]
        configuration.number_of_workers = 5

        with patch.object(commands.Command, "_call_client") as call_client, patch(
            "json.loads", return_value=[]
        ):
            commands.Check(arguments, configuration, source_directory=".").run()
            call_client.assert_called_once_with(
                command=commands.Check.NAME,
                flags=[
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
