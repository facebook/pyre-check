# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import hashlib
import socketserver
import tempfile
from pathlib import Path

import testslide

from ....tests import setup
from ..server_connection import connect, connect_in_text_mode, get_socket_path


class SocketTest(testslide.TestCase):
    def test_get_socket_path(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            setup.ensure_directories_exists(root_path, ["log"])

            md5_hash = hashlib.md5(str(root_path / "log").encode("utf-8")).hexdigest()
            self.assertEqual(
                get_socket_path(root=root_path, log_directory=root_path / "log"),
                root_path / f"pyre_server_{md5_hash}.sock",
            )


class EchoServerRequestHandler(socketserver.StreamRequestHandler):
    def handle(self) -> None:
        data = self.rfile.readline()
        self.wfile.write(data)
        self.wfile.flush()


class ConnectionTest(testslide.TestCase):
    def _test_binary_connect(self, socket_path: Path, message: str) -> None:
        with connect(socket_path) as (input_channel, output_channel):
            output_channel.write(f"{message}\n".encode("utf-8"))
            # Binary mode does not do auto-flush on newline.
            output_channel.flush()
            result = input_channel.readline().decode().strip()
            self.assertEqual(message, result)

    def _test_text_connect(self, socket_path: Path, message: str) -> None:
        with connect_in_text_mode(socket_path) as (input_channel, output_channel):
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
            self._test_binary_connect(socket_path, message)
            self._test_text_connect(socket_path, message)
