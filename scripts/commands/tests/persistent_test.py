# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import io
import unittest
from unittest.mock import call, patch

from ... import EnvironmentException  # noqa
from ... import commands  # noqa

from .command_test import (
    mock_arguments,
    mock_configuration,
)


class PersistentTest(unittest.TestCase):
    @patch.object(commands.Persistent, '_run_null_server', return_value=None)
    def test_persistent(self, run_null_server) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()
        configuration.get_version_hash.return_value = 'hash'
        configuration.number_of_workers = 42

        # Check start without watchman.
        with patch.object(commands.Command, '_call_client') as call_client:
            arguments.no_watchman = True
            commands.Persistent(
                arguments,
                configuration,
                source_directory='.').run()
            call_client.assert_called_once_with(
                command=commands.Persistent.NAME,
                flags=["-log-identifier", '"."', '-version', 'hash'],
                capture_output=False)

        # Check start of null server.
        with patch.object(commands.Command, '_call_client') as call_client:
            call_client.side_effect = EnvironmentException('derp')
            arguments.no_watchman = True
            try:
                commands.Persistent(
                    arguments,
                    configuration,
                    source_directory='.').run()
            except (commands.ClientException, EnvironmentException):
                commands.Persistent(
                    arguments,
                    configuration,
                    source_directory='.').on_client_exception()
            call_client.assert_has_calls(
                [
                    call(command=commands.Persistent.NAME,
                         flags=["-log-identifier", '"."', '-version', 'hash'],
                         capture_output=False),
                ])
            run_null_server.assert_called_once()

        # Check null server initialize output
        command = commands.Persistent(
            arguments,
            configuration,
            source_directory='.')
        self.assertEqual(
            command._initialize_response(5),
            "Content-Length: 59\r\n\r\n"
            '{"jsonrpc": "2.0", "id": 5, "result": {"capabilities": {}}}\r\n'
        )

    @patch('select.select')
    @patch('sys.stdout', new_callable=io.StringIO)
    @patch.object(commands.Command, '_call_client')
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
            "Content-Length: {}\r\n\r\n{}\r\n".format(len(input), input))

        select.return_value = ([stdin], [], [])
        # Check null server output when a valid input is given.
        commands.Persistent(
            mock_arguments(),
            mock_configuration(),
            source_directory='.',
        )._run_null_server(timeout=0)
        json = '{"jsonrpc": "2.0", "id": 0, "result": {"capabilities": {}}}'
        self.assertEqual(
            stdout.getvalue(),
            "Content-Length: 59\r\n\r\n{}\r\n".format(json))
