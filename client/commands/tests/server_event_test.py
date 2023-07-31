# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import io
from pathlib import Path
from typing import Type

import testslide

from ...language_server import connections
from ...tests import setup

from .. import server_event


class ServerEventTest(testslide.TestCase):
    def test_error_kind(self) -> None:
        self.assertEqual(
            server_event.ErrorKind.from_string("Watchman"),
            server_event.ErrorKind.WATCHMAN,
        )
        self.assertEqual(
            server_event.ErrorKind.from_string("BuckInternal"),
            server_event.ErrorKind.BUCK_INTERNAL,
        )
        self.assertEqual(
            server_event.ErrorKind.from_string("BuckUser"),
            server_event.ErrorKind.BUCK_USER,
        )
        self.assertEqual(
            server_event.ErrorKind.from_string("Pyre"), server_event.ErrorKind.PYRE
        )
        self.assertEqual(
            server_event.ErrorKind.from_string("Unknown"),
            server_event.ErrorKind.UNKNOWN,
        )
        self.assertEqual(
            server_event.ErrorKind.from_string("derp"), server_event.ErrorKind.UNKNOWN
        )

    def test_create(self) -> None:
        self.assertIsNone(server_event.create_from_string("derp"))
        self.assertIsNone(server_event.create_from_string("[]"))
        self.assertEqual(
            server_event.create_from_string('["SocketCreated", "/foo/bar"]'),
            server_event.SocketCreated(Path("/foo/bar")),
        )
        self.assertIsNone(server_event.create_from_string('["SocketCreated"]'))
        self.assertEqual(
            server_event.create_from_string('["ServerInitialized"]'),
            server_event.ServerInitialized(),
        )
        self.assertEqual(
            server_event.create_from_string('["Exception", "Burn baby burn!"]'),
            server_event.ServerException("Burn baby burn!"),
        )
        self.assertEqual(
            server_event.create_from_string(
                '["Exception", "Burn baby burn!", ["BuckUser"]]'
            ),
            server_event.ServerException(
                message="Burn baby burn!", kind=server_event.ErrorKind.BUCK_USER
            ),
        )
        self.assertEqual(
            server_event.create_from_string('["Exception", "Burn baby burn!", "derp"]'),
            server_event.ServerException(
                message="Burn baby burn!", kind=server_event.ErrorKind.UNKNOWN
            ),
        )
        self.assertIsNone(server_event.create_from_string('["Exception"]'))
        self.assertIsNone(server_event.create_from_string('["Exception", 42]'))
        self.assertIsNone(
            server_event.create_from_string('["UNRECOGNIZABLE", "message"]')
        )

    def test_waiter(self) -> None:
        def assert_ok(event_output: str, wait_on_initialization: bool) -> None:
            server_event.Waiter(wait_on_initialization=wait_on_initialization).wait_on(
                io.StringIO(event_output)
            )

        def assert_raises(
            event_output: str,
            wait_on_initialization: bool,
            exception: Type[Exception] = server_event.EventParsingException,
        ) -> None:
            with self.assertRaises(exception):
                server_event.Waiter(
                    wait_on_initialization=wait_on_initialization
                ).wait_on(io.StringIO(event_output))

        assert_raises("garbage", wait_on_initialization=False)
        assert_raises("[]", wait_on_initialization=False)
        assert_ok(
            '["SocketCreated", "/path/to/socket"]',
            wait_on_initialization=False,
        )
        assert_raises('["ServerInitialized"]', wait_on_initialization=False)
        assert_raises(
            '["Exception", "message"]',
            wait_on_initialization=False,
            exception=server_event.ServerStartException,
        )

        assert_raises("garbage", wait_on_initialization=True)
        assert_raises("[]", wait_on_initialization=True)
        assert_raises(
            '["SocketCreated", "/path/to/socket"]',
            wait_on_initialization=True,
        )
        assert_raises(
            '["Exception", "message"]',
            wait_on_initialization=True,
            exception=server_event.ServerStartException,
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
            await server_event.Waiter(
                wait_on_initialization=wait_on_initialization
            ).async_wait_on(connections.create_memory_text_reader(event_output))

        async def assert_raises(
            event_output: str,
            wait_on_initialization: bool,
            exception: Type[Exception] = server_event.EventParsingException,
        ) -> None:
            with self.assertRaises(exception):
                await server_event.Waiter(
                    wait_on_initialization=wait_on_initialization
                ).async_wait_on(connections.create_memory_text_reader(event_output))

        await assert_raises("garbage", wait_on_initialization=False)
        await assert_raises("[]", wait_on_initialization=False)
        await assert_ok(
            '["SocketCreated", "/path/to/socket"]',
            wait_on_initialization=False,
        )
        await assert_raises('["ServerInitialized"]', wait_on_initialization=False)
        await assert_raises(
            '["Exception", "message"]',
            wait_on_initialization=False,
            exception=server_event.ServerStartException,
        )

        await assert_raises("garbage", wait_on_initialization=True)
        await assert_raises("[]", wait_on_initialization=True)
        await assert_raises(
            '["SocketCreated", "/path/to/socket"]',
            wait_on_initialization=True,
        )
        await assert_raises(
            '["Exception", "message"]',
            wait_on_initialization=True,
            exception=server_event.ServerStartException,
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
