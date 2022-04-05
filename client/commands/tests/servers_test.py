# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import socketserver
import tempfile
from pathlib import Path

import testslide

from ...tests import setup
from ..servers import (
    AllServerStatus,
    DefunctServerStatus,
    find_all_servers,
    InvalidServerResponse,
    RunningServerStatus,
)


class MockServerRequestHandler(socketserver.StreamRequestHandler):
    def handle(self) -> None:
        try:
            while True:
                _ = self.rfile.readline()
                self.wfile.write(
                    json.dumps(
                        [
                            "Info",
                            {
                                "pid": 42,
                                "version": "abc",
                                "global_root": "/global",
                                "extra": 0,
                            },
                        ]
                    ).encode("utf-8")
                )
                self.wfile.write(b"\n")
                self.wfile.flush()
        except BrokenPipeError:
            pass


class ServersTest(testslide.TestCase):
    def test_parse_running_server_status(self) -> None:
        def assert_parsed(input: str, expected: RunningServerStatus) -> None:
            self.assertEqual(RunningServerStatus.from_server_response(input), expected)

        def assert_raises(input: str) -> None:
            with self.assertRaises(InvalidServerResponse):
                RunningServerStatus.from_server_response(input)

        assert_raises("42")
        assert_raises("[]")
        assert_raises("{}")
        assert_raises('["Info"]')
        assert_raises('["Info", 42]')
        assert_raises('["Derp", "Derp"]')
        assert_raises('["Info", {"pid": 42}]')
        assert_raises('["Info", {"pid": 42, "version": "derp"}]')
        assert_raises(
            json.dumps(
                [
                    "Info",
                    {
                        "pid": "42",
                        "version": "derp",
                        "global_root": "/global",
                        "log_path": "/log",
                    },
                ]
            )
        )
        assert_raises(
            json.dumps(
                [
                    "Info",
                    {
                        "pid": 42,
                        "version": "derp",
                        "global_root": "/global",
                        "relative_local_root": 0,
                    },
                ]
            )
        )

        assert_parsed(
            json.dumps(
                [
                    "Info",
                    {
                        "pid": 42,
                        "version": "abc",
                        "global_root": "/global",
                    },
                ]
            ),
            expected=RunningServerStatus(pid=42, version="abc", global_root="/global"),
        )
        assert_parsed(
            json.dumps(
                [
                    "Info",
                    {
                        "pid": 42,
                        "version": "abc",
                        "global_root": "/global",
                        "extra_field": 0,
                    },
                ]
            ),
            expected=RunningServerStatus(
                pid=42,
                version="abc",
                global_root="/global",
            ),
        )
        assert_parsed(
            json.dumps(
                [
                    "Info",
                    {
                        "pid": 42,
                        "version": "abc",
                        "global_root": "/global",
                        "extra_field": 0,
                    },
                ]
            ),
            expected=RunningServerStatus(
                pid=42,
                version="abc",
                global_root="/global",
            ),
        )
        assert_parsed(
            json.dumps(
                [
                    "Info",
                    {
                        "pid": 42,
                        "version": "abc",
                        "global_root": "/global",
                        "relative_local_root": "local",
                    },
                ]
            ),
            expected=RunningServerStatus(
                pid=42,
                version="abc",
                global_root="/global",
                relative_local_root="local",
            ),
        )

    def test_find_all_servers(self) -> None:
        with tempfile.TemporaryDirectory(dir="/tmp") as socket_root:
            socket_root_path = Path(socket_root)
            good_socket = socket_root_path / "good.sock"
            with setup.spawn_unix_stream_server_with_socket(
                MockServerRequestHandler, socket_path=good_socket
            ):
                bad_socket = socket_root_path / "bad.sock"
                bad_socket.touch()
                servers = find_all_servers([good_socket, bad_socket])
                self.assertListEqual(
                    servers.running,
                    [
                        RunningServerStatus(
                            pid=42,
                            version="abc",
                            global_root="/global",
                        )
                    ],
                )
                self.assertCountEqual(
                    servers.defunct,
                    [
                        DefunctServerStatus(str(bad_socket)),
                    ],
                )

    def test_to_json(self) -> None:
        self.assertCountEqual(
            AllServerStatus(
                running=[
                    RunningServerStatus(pid=123, version="abc", global_root="/g0"),
                    RunningServerStatus(
                        pid=456,
                        version="xyz",
                        global_root="/g1",
                        relative_local_root="local",
                    ),
                ],
                defunct=[
                    DefunctServerStatus(socket_path="/p0.sock"),
                    DefunctServerStatus("/p1.sock"),
                ],
            ).to_json(),
            [
                {
                    "status": "running",
                    "pid": 123,
                    "version": "abc",
                    "global_root": "/g0",
                    "relative_local_root": None,
                },
                {
                    "status": "running",
                    "pid": 456,
                    "version": "xyz",
                    "global_root": "/g1",
                    "relative_local_root": "local",
                },
                {"status": "defunct", "socket": "/p0.sock"},
                {"status": "defunct", "socket": "/p1.sock"},
            ],
        )
