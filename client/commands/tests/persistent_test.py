# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import io
import json
import unittest
from unittest.mock import MagicMock, call, patch

from ... import commands, configuration_monitor, project_files_monitor
from ...analysis_directory import AnalysisDirectory
from .command_test import mock_arguments, mock_configuration


class PersistentTest(unittest.TestCase):
    @patch.object(project_files_monitor, "ProjectFilesMonitor")
    @patch.object(commands.Persistent, "run_null_server", return_value=None)
    # pyre-fixme[56]: Argument `set()` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(commands.Reporting, "_get_directories_to_analyze", return_value=set())
    @patch.object(configuration_monitor.ConfigurationMonitor, "daemonize")
    def test_persistent(
        self,
        _daemonize: MagicMock,
        directories_to_analyze: MagicMock,
        run_null_server: MagicMock,
        Monitor: MagicMock,
    ) -> None:
        original_directory = "/original/directory"
        configuration = mock_configuration()
        configuration.version_hash = "hash"
        configuration.number_of_workers = 42

        # Check start without watchman.
        with patch.object(commands.Command, "_call_client") as call_client:
            arguments = mock_arguments(
                features=json.dumps(
                    {"click_to_fix": True, "go_to_definition": True, "hover": True}
                )
            )
            command = commands.Persistent(
                arguments,
                original_directory,
                configuration=configuration,
                analysis_directory=AnalysisDirectory("."),
                no_watchman=True,
            )
            self.assertEqual(
                command._flags(),
                [
                    "-log-identifier",
                    '"."',
                    "-expected-binary-version",
                    "hash",
                    "-log-directory",
                    ".pyre",
                    "-features",
                    '{"click_to_fix": true, "go_to_definition": true, "hover": true}',
                ],
            )
            command.run()
            call_client.assert_has_calls(
                [
                    call(command=commands.Persistent.NAME, capture_output=False),
                    call().check(),
                ]
            )

        # Check null server initialize output
        command = commands.Persistent(
            mock_arguments(),
            original_directory,
            configuration=configuration,
            analysis_directory=AnalysisDirectory("."),
            no_watchman=True,
        )
        self.assertEqual(
            command._initialize_response(5),
            "Content-Length: 59\r\n\r\n"
            '{"id": 5, "jsonrpc": "2.0", "result": {"capabilities": {}}}\r\n',
        )

    @patch("select.select")
    @patch("sys.stdout", new_callable=io.StringIO)
    @patch.object(commands.Command, "_call_client")
    def test_null_server(
        self, call_client: MagicMock, stdout: MagicMock, select: MagicMock
    ) -> None:
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
