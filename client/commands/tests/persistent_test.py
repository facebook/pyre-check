# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import io
import unittest
from unittest.mock import call, patch

from ... import EnvironmentException, commands, monitor, project_files_monitor  # noqa
from ...filesystem import AnalysisDirectory
from .command_test import mock_arguments, mock_configuration


class PersistentTest(unittest.TestCase):
    @patch.object(project_files_monitor, "Monitor")
    @patch.object(commands.Persistent, "run_null_server", return_value=None)
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    @patch.object(monitor.Monitor, "daemonize")
    def test_persistent(
        self, _daemonize, directories_to_analyze, run_null_server, Monitor
    ) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        configuration.version_hash = "hash"
        configuration.number_of_workers = 42

        # Check start without watchman.
        with patch.object(commands.Command, "_call_client") as call_client:
            arguments.no_watchman = True
            command = commands.Persistent(
                arguments, configuration, AnalysisDirectory(".")
            )
            self.assertEqual(
                command._flags(),
                ["-log-identifier", '"."', "-expected-binary-version", "hash"],
            )
            self.assertEqual(arguments.store_type_check_resolution, False)
            command.run()
            call_client.assert_has_calls(
                [
                    call(command=commands.Persistent.NAME, capture_output=False),
                    call().check(),
                ]
            )

        # Check null server initialize output
        command = commands.Persistent(arguments, configuration, AnalysisDirectory("."))
        self.assertEqual(
            command._initialize_response(5),
            "Content-Length: 59\r\n\r\n"
            '{"id": 5, "jsonrpc": "2.0", "result": {"capabilities": {}}}\r\n',
        )

    @patch("select.select")
    @patch("sys.stdout", new_callable=io.StringIO)
    @patch.object(commands.Command, "_call_client")
    def test_null_server(self, call_client, stdout, select) -> None:
        input = """
        {
            "jsonrpc": "2.0",
            "id": 0,
            "method": "initialize",
            "params": {
              "processId": 2588352,
              "rootPath": "/data/users/a/instagram/instagram-server",
              "rootUri": "file:///data/users/a/instagram/instagram-server",
              "capabilities": {
                "workspace": {
                  "applyEdit": true
                }
              }
            }
        }
        """
        stdin = io.StringIO(
            "Content-Length: {}\r\n\r\n{}\r\n".format(len(input), input)
        )

        select.return_value = ([stdin], [], [])
        # Check null server output when a valid input is given.
        commands.Persistent.run_null_server(timeout=0)
        json = '{"id": 0, "jsonrpc": "2.0", "result": {"capabilities": {}}}'
        self.assertEqual(
            stdout.getvalue(), "Content-Length: 59\r\n\r\n{}\r\n".format(json)
        )
