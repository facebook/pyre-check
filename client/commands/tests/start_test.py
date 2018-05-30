# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import call, patch

from ... import commands
from .command_test import mock_arguments, mock_configuration


class StartTest(unittest.TestCase):
    @patch("fcntl.lockf")
    def test_start(self, lock_file) -> None:
        arguments = mock_arguments()
        arguments.terminal = False

        configuration = mock_configuration()
        configuration.get_search_path.return_value = ["root"]
        configuration.get_version_hash.return_value = "hash"
        configuration.number_of_workers = 5

        # Check start without watchman.
        with patch.object(commands.Command, "_call_client") as call_client:
            arguments.no_watchman = True
            commands.Start(arguments, configuration, source_directory=".").run()
            call_client.assert_called_once_with(
                command=commands.Start.NAME,
                flags=[
                    "-project-root",
                    ".",
                    "-workers",
                    "5",
                    "-search-path",
                    "root",
                    "-version",
                    "hash",
                ],
            )

        # Check start with watchman.
        with patch.object(commands.Command, "_call_client") as call_client:
            arguments.no_watchman = False
            commands.Start(arguments, configuration, source_directory=".").run()
            call_client.assert_has_calls(
                [
                    call(
                        command=commands.Start.NAME,
                        flags=[
                            "-project-root",
                            ".",
                            "-use-watchman",
                            "-workers",
                            "5",
                            "-search-path",
                            "root",
                            "-version",
                            "hash",
                        ],
                    )
                ]
            )

        # Check start with terminal.
        with patch.object(commands.Command, "_call_client") as call_client:
            arguments.no_watchman = True
            arguments.terminal = True
            commands.Start(arguments, configuration, source_directory=".").run()
            call_client.assert_called_once_with(
                command=commands.Start.NAME,
                flags=[
                    "-project-root",
                    ".",
                    "-terminal",
                    "-workers",
                    "5",
                    "-search-path",
                    "root",
                    "-version",
                    "hash",
                ],
            )
