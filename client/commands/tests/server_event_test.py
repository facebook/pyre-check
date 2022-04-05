# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import io
from pathlib import Path
from typing import Type

import testslide

from ...tests import setup
from ..async_server_connection import create_memory_text_reader
from ..server_event import (
    create_from_string,
    ErrorKind,
    EventParsingException,
    ServerException,
    ServerInitialized,
    ServerStartException,
    SocketCreated,
    Waiter,
)


class ServerEventTest(testslide.TestCase):
    def test_error_kind(self) -> None:
        self.assertEqual(ErrorKind.from_string("Watchman"), ErrorKind.WATCHMAN)
        self.assertEqual(ErrorKind.from_string("BuckInternal"), ErrorKind.BUCK_INTERNAL)
        self.assertEqual(ErrorKind.from_string("BuckUser"), ErrorKind.BUCK_USER)
        self.assertEqual(ErrorKind.from_string("Pyre"), ErrorKind.PYRE)
        self.assertEqual(ErrorKind.from_string("Unknown"), ErrorKind.UNKNOWN)
        self.assertEqual(ErrorKind.from_string("derp"), ErrorKind.UNKNOWN)

    def test_create(self) -> None:
        self.assertIsNone(create_from_string("derp"))
        self.assertIsNone(create_from_string("[]"))
        self.assertEqual(
            create_from_string('["SocketCreated", "/foo/bar"]'),
            SocketCreated(Path("/foo/bar")),
        )
        self.assertIsNone(create_from_string('["SocketCreated"]'))
        self.assertEqual(
            create_from_string('["ServerInitialized"]'), ServerInitialized()
        )
        self.assertEqual(
            create_from_string('["Exception", "Burn baby burn!"]'),
            ServerException("Burn baby burn!"),
        )
        self.assertEqual(
            create_from_string('["Exception", "Burn baby burn!", ["BuckUser"]]'),
            ServerException(message="Burn baby burn!", kind=ErrorKind.BUCK_USER),
        )
        self.assertEqual(
            create_from_string('["Exception", "Burn baby burn!", "derp"]'),
            ServerException(message="Burn baby burn!", kind=ErrorKind.UNKNOWN),
        )
        self.assertIsNone(create_from_string('["Exception"]'))
        self.assertIsNone(create_from_string('["Exception", 42]'))
        self.assertIsNone(create_from_string('["UNRECOGNIZABLE", "message"]'))

    def test_waiter(self) -> None:
        def assert_ok(event_output: str, wait_on_initialization: bool) -> None:
            Waiter(wait_on_initialization=wait_on_initialization).wait_on(
                io.StringIO(event_output)
            )

        def assert_raises(
            event_output: str,
            wait_on_initialization: bool,
            exception: Type[Exception] = EventParsingException,
        ) -> None:
            with self.assertRaises(exception):
                Waiter(wait_on_initialization=wait_on_initialization).wait_on(
                    io.StringIO(event_output)
                )

        assert_raises("garbage", wait_on_initialization=False)
        assert_raises("[]", wait_on_initialization=False)
        assert_ok('["SocketCreated", "/path/to/socket"]', wait_on_initialization=False)
        assert_raises('["ServerInitialized"]', wait_on_initialization=False)
        assert_raises(
            '["Exception", "message"]',
            wait_on_initialization=False,
            exception=ServerStartException,
        )

        assert_raises("garbage", wait_on_initialization=True)
        assert_raises("[]", wait_on_initialization=True)
        assert_raises(
            '["SocketCreated", "/path/to/socket"]', wait_on_initialization=True
        )
        assert_raises(
            '["Exception", "message"]',
            wait_on_initialization=True,
            exception=ServerStartException,
        )
        assert_raises(
            '["SocketCreated", "/path/to/socket"]\n' + '["ServerException", "message"]',
            wait_on_initialization=True,
        )
        assert_raises(
            '["SocketCreated", "/path/to/socket"]\n'
            + '["SocketCreated", "/path/to/socket"]',
            wait_on_initialization=True,
        )
        assert_ok(
            '["SocketCreated", "/path/to/socket"]\n' + '["ServerInitialized"]',
            wait_on_initialization=True,
        )

    @setup.async_test
    async def test_async_waiter(self) -> None:
        async def assert_ok(event_output: str, wait_on_initialization: bool) -> None:
            await Waiter(wait_on_initialization=wait_on_initialization).async_wait_on(
                create_memory_text_reader(event_output)
            )

        async def assert_raises(
            event_output: str,
            wait_on_initialization: bool,
            exception: Type[Exception] = EventParsingException,
        ) -> None:
            with self.assertRaises(exception):
                await Waiter(
                    wait_on_initialization=wait_on_initialization
                ).async_wait_on(create_memory_text_reader(event_output))

        await assert_raises("garbage", wait_on_initialization=False)
        await assert_raises("[]", wait_on_initialization=False)
        await assert_ok(
            '["SocketCreated", "/path/to/socket"]', wait_on_initialization=False
        )
        await assert_raises('["ServerInitialized"]', wait_on_initialization=False)
        await assert_raises(
            '["Exception", "message"]',
            wait_on_initialization=False,
            exception=ServerStartException,
        )

        await assert_raises("garbage", wait_on_initialization=True)
        await assert_raises("[]", wait_on_initialization=True)
        await assert_raises(
            '["SocketCreated", "/path/to/socket"]', wait_on_initialization=True
        )
        await assert_raises(
            '["Exception", "message"]',
            wait_on_initialization=True,
            exception=ServerStartException,
        )
        await assert_raises(
            '["SocketCreated", "/path/to/socket"]\n' + '["ServerException", "message"]',
            wait_on_initialization=True,
        )
        await assert_raises(
            '["SocketCreated", "/path/to/socket"]\n'
            + '["SocketCreated", "/path/to/socket"]',
            wait_on_initialization=True,
        )
        await assert_ok(
            '["SocketCreated", "/path/to/socket"]\n' + '["ServerInitialized"]',
            wait_on_initialization=True,
        )
