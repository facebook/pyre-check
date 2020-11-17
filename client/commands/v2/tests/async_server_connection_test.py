# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path

import testslide

from ....tests import setup
from ..async_server_connection import connect, connect_in_text_mode
from .server_connection_test import EchoServerRequestHandler


class AsyncConnectionTest(testslide.TestCase):
    async def _test_binary_connect(self, socket_path: Path) -> None:
        async with connect(socket_path) as (input_channel, output_channel):
            await output_channel.write("abc\n".encode("utf-8"))
            result = (await input_channel.readline()).decode("utf-8").strip()
            self.assertEqual("abc", result)

            await output_channel.write("xyzuvw\n".encode("utf-8"))
            result = (await input_channel.read_until(b"z")).decode("utf-8")
            self.assertEqual("xyz", result)

            result = (await input_channel.read_exactly(2)).decode("utf-8")
            self.assertEqual("uv", result)

    async def _test_text_connect(self, socket_path: Path) -> None:
        async with connect_in_text_mode(socket_path) as (input_channel, output_channel):
            await output_channel.write("abc\n")
            result = (await input_channel.readline()).strip()
            self.assertEqual("abc", result)

            await output_channel.write("xyzuvw\n")
            result = await input_channel.read_until("z")
            self.assertEqual("xyz", result)

            result = await input_channel.read_exactly(2)
            self.assertEqual("uv", result)

    @setup.async_test
    async def test_connect(self) -> None:
        with setup.spawn_unix_stream_server(EchoServerRequestHandler) as socket_path:
            # Connect to test server from the main thread
            await self._test_binary_connect(socket_path)
            await self._test_text_connect(socket_path)
