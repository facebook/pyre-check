# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import socketserver
from pathlib import Path

import testslide

from ...tests import setup
from ..server_connection import connect


class EchoServerRequestHandler(socketserver.StreamRequestHandler):
    def handle(self) -> None:
        try:
            while True:
                data = self.rfile.readline()
                self.wfile.write(data)
                self.wfile.flush()
        except BrokenPipeError:
            pass


class ConnectionTest(testslide.TestCase):
    def _test_connect(self, socket_path: Path, message: str) -> None:
        with connect(socket_path) as (input_channel, output_channel):
            output_channel.write(f"{message}\n")
            result = input_channel.readline().strip()
            self.assertEqual(message, result)

    def test_connect(self) -> None:
        with setup.spawn_unix_stream_server(EchoServerRequestHandler) as socket_path:
            # Connect to test server from the main thread
            message = (
                "My word! You've correctly identified the most recognizable man"
                "in the colony. Remarkable."
            )
            self._test_connect(socket_path, message)
