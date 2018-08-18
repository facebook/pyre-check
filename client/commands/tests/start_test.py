# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import call, patch

from .. import monitor  # noqa
from ... import commands  # noqa
from .command_test import mock_arguments, mock_configuration


class StartTest(unittest.TestCase):
    @patch("fcntl.lockf")
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    @patch.object(monitor.Monitor, "daemonize")
    def test_start(self, _daemonize, get_directories_to_analyze, lock_file) -> None:
        arguments = mock_arguments()
        arguments.terminal = False

        configuration = mock_configuration()
        configuration.get_typeshed.return_value = "stub"
        configuration.get_search_path.return_value = ["path1", "path2"]
        configuration.get_version_hash.return_value = "hash"
        configuration.number_of_workers = 5

        # Check start without watchman.
        with patch.object(commands.Command, "_call_client") as call_client:
            arguments.no_watchman = True
            command = commands.Start(arguments, configuration, analysis_directory=".")
            self.assertEquals(
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
            command = commands.Start(arguments, configuration, analysis_directory=".")
            self.assertEquals(
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

            command = commands.Start(arguments, configuration, analysis_directory=".")
            self.assertEquals(
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
