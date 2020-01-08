# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import subprocess
import unittest
from pathlib import Path
from unittest.mock import MagicMock, call, patch

from ... import commands
from ..servers import Servers


class ServersCommandTest(unittest.TestCase):
    @patch.object(commands.servers, "find_project_root", return_value="/root")
    def test_dot_pyre_root(self, find_project_root: MagicMock) -> None:
        self.assertEqual(Servers._dot_pyre_root(), Path("/root/.pyre"))

    @patch.object(Servers, "_dot_pyre_root", return_value=Path("/root/.pyre"))
    @patch.object(subprocess, "check_output")
    def test_find_servers(
        self, check_output: MagicMock, dot_pyre_root: MagicMock
    ) -> None:
        check_output.return_value = (
            b"/root/.pyre/foo/server/server.pid\n"
            b"/root/.pyre/bar/baz/server/server.pid\n"
        )
        self.assertEqual(
            commands.Servers._find_servers(),
            [
                Path("/root/.pyre/foo/server/server.pid"),
                Path("/root/.pyre/bar/baz/server/server.pid"),
            ],
        )
        check_output.assert_called_once_with(
            ["find", "/root/.pyre", "-type", "f", "-name", "server.pid"]
        )

    @patch.object(Path, "read_text")
    def test_fetch_server_details(self, read_text: MagicMock) -> None:
        read_text.side_effect = ["123", "789"]
        self.assertEqual(
            commands.Servers._fetch_server_details(
                Path("/root/.pyre/bar/baz/server/server.pid"), Path("/root/.pyre")
            ),
            {
                "pid": 123,
                "local_root": "bar/baz",
                "path": Path("/root/.pyre/bar/baz/server/server.pid"),
            },
        )
        self.assertEqual(
            commands.Servers._fetch_server_details(
                Path("/root/.pyre/server/server.pid"), Path("/root/.pyre")
            ),
            {
                "pid": 789,
                "local_root": "<root>",
                "path": Path("/root/.pyre/server/server.pid"),
            },
        )

    @patch("builtins.print")
    def test_print_server_details(self, print_function: MagicMock) -> None:
        commands.Servers._print_server_details(
            [
                {
                    "pid": 789,
                    "local_root": "<root>",
                    "path": Path("/root/.pyre/server/server.pid"),
                },
                {
                    "pid": 456,
                    "local_root": "bar/baz",
                    "path": Path("/root/.pyre/bar/baz/server/server.pid"),
                },
                {
                    "pid": 123,
                    "local_root": "foo",
                    "path": Path("/root/.pyre/foo/server/server.pid"),
                },
            ]
        )
        print_function.assert_has_calls(
            [
                call("Pyre servers: <pid> <server-path>"),
                call("789        <root>"),
                call("456        bar/baz"),
                call("123        foo"),
            ]
        )
