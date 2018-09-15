# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import MagicMock, call, patch

from .. import monitor  # noqa
from ... import commands  # noqa
from ...filesystem import AnalysisDirectory  # noqa
from .command_test import mock_arguments, mock_configuration


class StartTest(unittest.TestCase):
    @patch("fcntl.lockf")
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    @patch.object(monitor.Monitor, "daemonize")
    def test_start(self, _daemonize, get_directories_to_analyze, lock_file) -> None:
        arguments = mock_arguments()
        arguments.terminal = False

        configuration = mock_configuration()
        configuration.version_hash = "hash"
        configuration.number_of_workers = 5

        analysis_directory = AnalysisDirectory(".")
        # Check start without watchman.
        with patch.object(commands.Command, "_call_client") as call_client:
            arguments.no_watchman = True
            command = commands.Start(arguments, configuration, analysis_directory)
            self.assertEqual(
                command._flags(),
                [
                    "-project-root",
                    ".",
                    "-workers",
                    "5",
                    "-typeshed",
                    "stub",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Start.NAME)

        # Check start with watchman.
        with patch.object(commands.Command, "_call_client") as call_client:
            arguments.no_watchman = False
            command = commands.Start(arguments, configuration, analysis_directory)
            self.assertEqual(
                command._flags(),
                [
                    "-project-root",
                    ".",
                    "-use-watchman",
                    "-workers",
                    "5",
                    "-typeshed",
                    "stub",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2",
                ],
            )
            command.run()
            call_client.assert_has_calls([call(command=commands.Start.NAME)])

        # Check start with terminal.
        with patch.object(commands.Command, "_call_client") as call_client:
            arguments.no_watchman = True
            arguments.terminal = True

            command = commands.Start(arguments, configuration, analysis_directory)
            self.assertEqual(
                command._flags(),
                [
                    "-project-root",
                    ".",
                    "-terminal",
                    "-workers",
                    "5",
                    "-typeshed",
                    "stub",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Start.NAME)

        # Shared analysis directories are prepared when starting.
        shared_analysis_directory = MagicMock()
        shared_analysis_directory.get_root = lambda: "."
        with patch.object(
            commands.Command, "_call_client"
        ) as call_client, patch.object(shared_analysis_directory, "prepare") as prepare:
            arguments.no_watchman = True
            arguments.terminal = False
            command = commands.Start(
                arguments, configuration, shared_analysis_directory
            )
            self.assertEqual(
                command._flags(),
                [
                    "-project-root",
                    ".",
                    "-workers",
                    "5",
                    "-typeshed",
                    "stub",
                    "-expected-binary-version",
                    "hash",
                    "-search-path",
                    "path1,path2",
                ],
            )
            command.run()
            call_client.assert_called_once_with(command=commands.Start.NAME)
            prepare.assert_called_once_with()

        analysis_directory = AnalysisDirectory(".")
        # Check filter directories.
        with patch.object(commands.Command, "_call_client") as call_client:
            arguments.no_watchman = True
            command = commands.Start(arguments, configuration, analysis_directory)
            with patch.object(
                command, "_get_directories_to_analyze"
            ) as get_directories:
                get_directories.return_value = {"a", "b"}
                self.assertEqual(
                    command._flags(),
                    [
                        "-project-root",
                        ".",
                        "-filter-directories",
                        "a;b",
                        "-workers",
                        "5",
                        "-typeshed",
                        "stub",
                        "-expected-binary-version",
                        "hash",
                        "-search-path",
                        "path1,path2",
                    ],
                )

        # Check save-initial-state-to.
        arguments.no_watchman = False
        arguments.save_initial_state_to = "/tmp"
        command = commands.Start(arguments, configuration, analysis_directory)
        self.assertEqual(
            command._flags(),
            [
                "-project-root",
                ".",
                "-use-watchman",
                "-save-initial-state-to",
                "/tmp",
                "-workers",
                "5",
                "-typeshed",
                "stub",
                "-expected-binary-version",
                "hash",
                "-search-path",
                "path1,path2",
            ],
        )
