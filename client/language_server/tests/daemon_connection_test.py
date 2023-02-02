# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio
from pathlib import Path
from types import TracebackType
from typing import AsyncContextManager, Optional, Type, TypeVar

import testslide

from ...tests import setup

from .. import connections
from ..daemon_connection import attempt_send_async_raw_request, DaemonConnectionFailure

T = TypeVar("T")


class RaisingBytesReader(connections.AsyncBytesReader):
    def __init__(self, exception: Exception) -> None:
        self.exception = exception

    async def read_until(self, separator: bytes = b"\n") -> bytes:
        raise self.exception

    async def read_exactly(self, count: int) -> bytes:
        raise self.exception


class MockAsyncContextManager(AsyncContextManager[T]):
    def __init__(self, value: T) -> None:
        self.value: T = value

    async def __aenter__(self) -> T:
        return self.value

    # pyre-fixme[14]: `__aexit__` overrides method defined in `AsyncContextManager`
    #  inconsistently.
    async def __aexit__(
        self,
        typ: Optional[Type[BaseException]],
        value: Optional[BaseException],
        traceback: Optional[TracebackType],
    ) -> Optional[bool]:
        return False


class SendTest(testslide.TestCase):
    @setup.async_test
    async def test_attempt_send_async_raw_request_ok(self) -> None:
        self.mock_callable(
            connections,
            "connect_async",
            # Strict mocking will fail due to a bug in asynccontextmanager
            # handling. See https://github.com/facebook/TestSlide/issues/193
            type_validation=False,
        ).to_return_value(
            MockAsyncContextManager(
                (
                    connections.AsyncTextReader(
                        connections.MemoryBytesReader(b"derp\n")
                    ),
                    connections.AsyncTextWriter(connections.MemoryBytesWriter()),
                )
            )
        )
        self.assertEqual(
            await attempt_send_async_raw_request(Path("dummy"), "dummy_request"),
            "derp\n",
        )

    @setup.async_test
    async def test_attempt_send_async_raw_request_failure0(self) -> None:
        self.mock_callable(
            connections,
            "connect_async",
            type_validation=False,
        ).to_return_value(
            MockAsyncContextManager(
                (
                    connections.AsyncTextReader(
                        RaisingBytesReader(connections.ConnectionFailure())
                    ),
                    connections.AsyncTextWriter(connections.MemoryBytesWriter()),
                )
            )
        )
        result = await attempt_send_async_raw_request(Path("dummy"), "dummy_request")
        self.assertTrue(isinstance(result, DaemonConnectionFailure))

    @setup.async_test
    async def test_attempt_send_async_raw_request_failure1(self) -> None:
        self.mock_callable(
            connections,
            "connect_async",
            type_validation=False,
        ).to_return_value(
            MockAsyncContextManager(
                (
                    connections.AsyncTextReader(
                        RaisingBytesReader(ConnectionResetError())
                    ),
                    connections.AsyncTextWriter(connections.MemoryBytesWriter()),
                )
            )
        )
        result = await attempt_send_async_raw_request(Path("dummy"), "dummy_request")
        self.assertTrue(isinstance(result, DaemonConnectionFailure))

    @setup.async_test
    async def test_attempt_send_async_raw_request_failure2(self) -> None:
        self.mock_callable(
            connections,
            "connect_async",
            type_validation=False,
        ).to_return_value(
            MockAsyncContextManager(
                (
                    connections.AsyncTextReader(
                        RaisingBytesReader(asyncio.IncompleteReadError(b"", None))
                    ),
                    connections.AsyncTextWriter(connections.MemoryBytesWriter()),
                )
            )
        )
        result = await attempt_send_async_raw_request(Path("dummy"), "dummy_request")
        self.assertTrue(isinstance(result, DaemonConnectionFailure))
