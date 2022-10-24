# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


from __future__ import annotations

import asyncio
import contextlib
import dataclasses
import logging
from pathlib import Path
from typing import AsyncIterator, Union

from .. import dataclasses_json_extensions as json_mixins, log
from ..language_server import connections


LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class DaemonConnectionFailure(json_mixins.CamlCaseAndExcludeJsonMixin):
    error_message: str


def send_raw_request(socket_path: Path, raw_request: str) -> str:
    with connections.connect(socket_path) as (
        input_channel,
        output_channel,
    ):
        LOG.debug(f"Sending `{log.truncate(raw_request, 400)}`")
        output_channel.write(f"{raw_request}\n")
        raw_response = input_channel.readline().strip()
        LOG.debug(f"Received `{log.truncate(raw_response, 400)}`")
        return raw_response


@dataclasses.dataclass(frozen=True)
class AsyncConnection:
    reader: connections.AsyncTextReader
    writer: connections.AsyncTextWriter

    async def _send_request_no_logging(
        self,
        request: str,
    ) -> str:
        await self.writer.write(f"{request}\n")
        return await self.reader.read_until(separator="\n")

    async def send_request(
        self,
        request: str,
    ) -> str:
        LOG.debug(f"Sending `{log.truncate(request, 400)}`")
        response = await self._send_request_no_logging(request)
        LOG.info(f"Received: `{log.truncate(response, 400)}`")
        return response

    @staticmethod
    @contextlib.asynccontextmanager
    async def create(
        socket_path: Path,
    ) -> AsyncIterator[AsyncConnection]:
        async with connections.connect_async(socket_path) as (reader, writer):
            yield AsyncConnection(reader, writer)


async def send_async_raw_request(
    socket_path: Path,
    request: str,
) -> str:
    async with AsyncConnection.create(socket_path) as connection:
        return await connection.send_request(request)


async def attempt_send_async_raw_request(
    socket_path: Path,
    request: str,
) -> Union[DaemonConnectionFailure, str]:
    try:
        return await send_async_raw_request(
            socket_path,
            request,
        )
    except (
        connections.ConnectionFailure,
        asyncio.IncompleteReadError,
        ConnectionError,
    ) as error:
        return DaemonConnectionFailure(
            "Could not establish connection with an existing Pyre server "
            f"at {socket_path}: {error}"
        )
