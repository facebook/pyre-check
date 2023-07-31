# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import socketserver
import tempfile
from pathlib import Path

import testslide

from ... import identifiers

from ...tests import setup

from .. import servers


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
        def assert_parsed(
            input: str,
            expected: servers.RunningServerStatus,
            flavor: identifiers.PyreFlavor = identifiers.PyreFlavor.CLASSIC,
        ) -> None:
            self.assertEqual(
                servers.RunningServerStatus.from_server_response(input, flavor),
                expected,
            )

        def assert_raises(input: str) -> None:
            with self.assertRaises(servers.InvalidServerResponse):
                servers.RunningServerStatus.from_server_response(
                    input, identifiers.PyreFlavor.CLASSIC
                )

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
            expected=servers.RunningServerStatus(
                pid=42,
                version="abc",
                global_root="/global",
                flavor=identifiers.PyreFlavor.CLASSIC.value,
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
            expected=servers.RunningServerStatus(
                pid=42,
                version="abc",
                global_root="/global",
                flavor="classic",
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
            expected=servers.RunningServerStatus(
                pid=42,
                version="abc",
                global_root="/global",
                flavor="classic",
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
            expected=servers.RunningServerStatus(
                pid=42,
                version="abc",
                global_root="/global",
                relative_local_root="local",
                flavor="classic",
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
            flavor=identifiers.PyreFlavor.CODE_NAVIGATION,
            expected=servers.RunningServerStatus(
                pid=42,
                version="abc",
                global_root="/global",
                relative_local_root="local",
                flavor="code_navigation",
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
                all_server_status = servers.find_all_servers([good_socket, bad_socket])
                self.assertListEqual(
                    all_server_status.running,
                    [
                        servers.RunningServerStatus(
                            pid=42,
                            version="abc",
                            global_root="/global",
                            flavor=identifiers.PyreFlavor.CLASSIC.value,
                        )
                    ],
                )
                self.assertCountEqual(
                    all_server_status.defunct,
                    [
                        servers.DefunctServerStatus(str(bad_socket)),
                    ],
                )

    def test_to_json(self) -> None:
        self.assertCountEqual(
            servers.AllServerStatus(
                running=[
                    servers.RunningServerStatus(
                        pid=123,
                        version="abc",
                        global_root="/g0",
                        flavor=identifiers.PyreFlavor.CLASSIC.value,
                    ),
                    servers.RunningServerStatus(
                        pid=456,
                        version="xyz",
                        global_root="/g1",
                        relative_local_root="local",
                        flavor=identifiers.PyreFlavor.CODE_NAVIGATION.value,
                    ),
                ],
                defunct=[
                    servers.DefunctServerStatus(socket_path="/p0.sock"),
                    servers.DefunctServerStatus("/p1.sock"),
                ],
            ).to_json(),
            [
                {
                    "status": "running",
                    "pid": 123,
                    "version": "abc",
                    "global_root": "/g0",
                    "relative_local_root": None,
                    "flavor": "classic",
                },
                {
                    "status": "running",
                    "pid": 456,
                    "version": "xyz",
                    "global_root": "/g1",
                    "relative_local_root": "local",
                    "flavor": "code_navigation",
                },
                {"status": "defunct", "socket": "/p0.sock"},
                {"status": "defunct", "socket": "/p1.sock"},
            ],
        )
