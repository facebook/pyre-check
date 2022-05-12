# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio
from pathlib import Path

import testslide

from ...tests import setup
from ..async_server_connection import (
    BackgroundTask,
    BackgroundTaskManager,
    connect,
    connect_in_text_mode,
    MemoryBytesReader,
    MemoryBytesWriter,
)
from .server_connection_test import EchoServerRequestHandler


class MemoryIOTest(testslide.TestCase):
    @setup.async_test
    async def test_memory_read(self) -> None:
        reader = MemoryBytesReader(b"abcdefghijk")
        result = await reader.read_until(b"de")
        self.assertEqual(result, b"abcde")
        result = await reader.read_exactly(4)
        self.assertEqual(result, b"fghi")
        result = await reader.readline()
        self.assertEqual(result, b"jk")

        reader.reset()
        result = await reader.read_exactly(0)
        self.assertEqual(result, b"")
        result = await reader.read_exactly(11)
        self.assertEqual(result, b"abcdefghijk")
        result = await reader.readline()
        self.assertEqual(result, b"")

        try:
            await MemoryBytesReader(b"abc").read_until(b"d")
        except asyncio.IncompleteReadError as error:
            self.assertEqual(error.partial, b"abc")

        try:
            await MemoryBytesReader(b"abc").read_exactly(4)
        except asyncio.IncompleteReadError as error:
            self.assertEqual(error.partial, b"abc")

    @setup.async_test
    async def test_memory_write(self) -> None:
        writer = MemoryBytesWriter()
        await writer.write(b"foo")
        await writer.write(b"bar")
        await writer.write(b"baz")
        self.assertListEqual(writer.items(), [b"foo", b"bar", b"baz"])


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

    @setup.async_test
    async def test_read_until(self) -> None:
        with setup.spawn_unix_stream_server(EchoServerRequestHandler) as socket_path:
            # Intentionally use a small buffer size, to test over-sized reads
            async with connect(socket_path, buffer_size=64) as (
                input_channel,
                output_channel,
            ):
                # Try reading ~4MB of data from the input channel, and verify that
                # `read_until` do not choke.
                message = b"a" * (2**14) + b"\n"
                await output_channel.write(message)
                result = await input_channel.read_until(b"\n")
                self.assertEqual(message, result)


class WaitForeverTask(BackgroundTask):
    async def run(self) -> None:
        await asyncio.Event().wait()


class WaitForEventTask(BackgroundTask):
    event: asyncio.Event

    def __init__(self) -> None:
        self.event = asyncio.Event()

    async def run(self) -> None:
        # Wait until the event is set. Then quit.
        await self.event.wait()


class BackgroundTaskTest(testslide.TestCase):
    @setup.async_test
    async def test_background_task_manager(self) -> None:
        task = WaitForEventTask()
        manager = BackgroundTaskManager(task)
        self.assertFalse(manager.is_task_running())

        await manager.ensure_task_running()
        # Yield control to the event loop, allowing the background task to start
        await asyncio.sleep(0)
        self.assertTrue(manager.is_task_running())

        # Terminate the server handler by setting the event
        task.event.set()
        # Yield control to the event loop, allowing the server handler to stop
        await asyncio.sleep(0)
        self.assertFalse(manager.is_task_running())

        # Restart the background task
        task.event.clear()
        await manager.ensure_task_running()
        # Yield control to the event loop, allowing the background task to start
        await asyncio.sleep(0)
        self.assertTrue(manager.is_task_running())

        await manager.ensure_task_stop()
        self.assertFalse(manager.is_task_running())

    @setup.async_test
    async def test_background_task_manager_shutdown_before_start(self) -> None:
        manager = BackgroundTaskManager(WaitForeverTask())
        self.assertFalse(manager.is_task_running())

        await manager.ensure_task_running()
        await manager.ensure_task_stop()
        self.assertFalse(manager.is_task_running())
