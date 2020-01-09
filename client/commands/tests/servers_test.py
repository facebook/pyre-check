# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import subprocess
import unittest
from pathlib import Path
from unittest.mock import MagicMock, Mock, call, patch

from ... import commands
from .. import servers
from ..servers import ServerDetails, Servers


class ServersCommandTest(unittest.TestCase):
    @patch.object(commands.servers, "find_project_root", return_value="/root")
    def test_dot_pyre_root(self, find_project_root: MagicMock) -> None:
        self.assertEqual(Servers._dot_pyre_root(), Path("/root/.pyre"))

    @patch("builtins.print")
    def test_print_server_details(self, print_function: MagicMock) -> None:
        commands.Servers._print_server_details(
            [
                ServerDetails(
                    pid=789,
                    local_root=".",
                    server_pid_path=Path("/root/.pyre/server/server.pid"),
                ),
                ServerDetails(
                    pid=456,
                    local_root="bar/baz",
                    server_pid_path=Path("/root/.pyre/bar/baz/server/server.pid"),
                ),
                ServerDetails(
                    pid=123,
                    local_root="foo",
                    server_pid_path=Path("/root/.pyre/foo/server/server.pid"),
                ),
            ]
        )
        print_function.assert_has_calls(
            [
                call("Pyre servers: <pid> <server-root>"),
                call("789        <root>"),
                call("456        bar/baz"),
                call("123        foo"),
            ]
        )

    @patch.object(Servers, "__init__", return_value=None)
    @patch.object(Servers, "_dot_pyre_root", return_value=Path("/root/.pyre"))
    @patch.object(servers, "Stop")
    def test_stop_servers(
        self, stop_class: MagicMock, dot_pyre_root: MagicMock, constructor: MagicMock
    ) -> None:
        servers = Servers(arguments=MagicMock(), original_directory=MagicMock())
        servers._arguments = Mock()
        servers._stop_servers(
            [
                ServerDetails(
                    pid=789,
                    local_root=".",
                    server_pid_path=Path("/root/.pyre/server/server.pid"),
                ),
                ServerDetails(
                    pid=456,
                    local_root="bar/baz",
                    server_pid_path=Path("/root/.pyre/bar/baz/server/server.pid"),
                ),
                ServerDetails(
                    pid=123,
                    local_root="foo",
                    server_pid_path=Path("/root/.pyre/foo/server/server.pid"),
                ),
            ]
        )
        stop_class.assert_has_calls(
            [
                call(arguments=servers._arguments, original_directory="/root"),
                call(arguments=servers._arguments, original_directory="/root/bar/baz"),
                call(arguments=servers._arguments, original_directory="/root/foo"),
            ],
            any_order=True,
        )

    @patch.object(Servers, "__init__", return_value=None)
    @patch.object(Servers, "_find_servers")
    @patch.object(ServerDetails, "_from_server_path")
    @patch.object(Servers, "_print_server_details")
    @patch.object(Servers, "_stop_servers")
    def test_run(
        self,
        stop_servers: MagicMock,
        print_server_details: MagicMock,
        find_servers: MagicMock,
        fetch_server_details: MagicMock,
        constructor: MagicMock,
    ) -> None:
        servers = Servers(arguments=MagicMock(), original_directory=MagicMock())
        servers._arguments = Mock(servers_subcommand=None)
        servers._run()
        print_server_details.assert_called_once()
        print_server_details.reset_mock()

        servers._arguments = Mock(servers_subcommand="list")
        servers._run()
        print_server_details.assert_called_once()
        print_server_details.reset_mock()

        servers._arguments = Mock(servers_subcommand="stop")
        servers._run()
        stop_servers.assert_called_once()
        print_server_details.assert_not_called()


class ServerDetailsTest(unittest.TestCase):
    @patch.object(Path, "read_text")
    def test_from_server_path(self, read_text: MagicMock) -> None:
        read_text.side_effect = ["123", "789"]
        self.assertEqual(
            ServerDetails._from_server_path(
                Path("/root/.pyre/bar/baz/server/server.pid"), Path("/root/.pyre")
            ),
            ServerDetails(
                pid=123,
                local_root="bar/baz",
                server_pid_path=Path("/root/.pyre/bar/baz/server/server.pid"),
            ),
        )
        self.assertEqual(
            ServerDetails._from_server_path(
                Path("/root/.pyre/server/server.pid"), Path("/root/.pyre")
            ),
            ServerDetails(
                pid=789,
                local_root=".",
                server_pid_path=Path("/root/.pyre/server/server.pid"),
            ),
        )
