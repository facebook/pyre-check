# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import contextlib
import dataclasses
import hashlib

import json
import logging
import tempfile
from pathlib import Path
from typing import AsyncIterator, Optional, TextIO, Type, TypeVar

import dataclasses_json

from .. import log
from . import connections


LOG: logging.Logger = logging.getLogger(__name__)

QueryResponseType = TypeVar("QueryResponseType")


# Socket path logic ---


def get_socket_path(
    root: Path, global_root: Path, relative_local_root: Optional[str]
) -> Path:
    """
    Determine where the server socket file is located. We can't directly use
    `log_directory` because of the ~100 character length limit on Unix socket
    file paths.

    Implementation needs to be kept in sync with the `user_independent_socket_path_of`
    function in `pyre/new_server/start.ml`.
    """
    project_identifier = str(global_root)
    if relative_local_root is not None:
        project_identifier = project_identifier + "//" + relative_local_root
    project_identifier = project_identifier.encode("utf-8")

    project_hash = hashlib.md5(project_identifier).hexdigest()
    return root / f"pyre_server_{project_hash}.sock"


def get_default_socket_root() -> Path:
    # TODO(T77556312): It might be cleaner to turn the root dir into a
    # configuration option instead.
    return Path(tempfile.gettempdir())


def get_default_socket_path(
    project_root: Path, relative_local_root: Optional[str]
) -> Path:
    return get_socket_path(get_default_socket_root(), project_root, relative_local_root)


# Query serialization logic ---


class InvalidQueryResponse(Exception):
    pass


@dataclasses.dataclass(frozen=True)
class Response:
    payload: object

    @staticmethod
    def from_json(
        response_json: object,
    ) -> Response:
        if (
            isinstance(response_json, list)
            and len(response_json) > 1
            and response_json[0] == "Query"
        ):
            return Response(response_json[1])
        else:
            raise InvalidQueryResponse(
                f"Unexpected JSON response from server: {response_json}"
            )

    @staticmethod
    def parse(
        response_text: str,
    ) -> Response:
        try:
            response_json = json.loads(response_text)
            return Response.from_json(response_json)
        except json.JSONDecodeError as decode_error:
            message = f"Cannot parse response as JSON: {decode_error}"
            raise InvalidQueryResponse(message) from decode_error


# Synchronous query API


def _send_query_request(output_channel: TextIO, query_text: str) -> None:
    query_message = json.dumps(["Query", query_text])
    LOG.debug(f"Sending `{log.truncate(query_message, 400)}`")
    output_channel.write(f"{query_message}\n")


def _receive_query_response(input_channel: TextIO) -> Response:
    query_message = input_channel.readline().strip()
    LOG.debug(f"Received `{log.truncate(query_message, 400)}`")
    return Response.parse(query_message)


def execute_query(socket_path: Path, query_text: str) -> Response:
    with connections.connect(socket_path) as (
        input_channel,
        output_channel,
    ):
        _send_query_request(output_channel, query_text)
        return _receive_query_response(input_channel)


# Async connection logic --


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

    @staticmethod
    async def connect_and_send_request(
        socket_path: Path,
        request: str,
    ) -> str:
        async with AsyncConnection.create(socket_path) as connection:
            return await connection.send_request(request)

    @staticmethod
    async def attempt_connect_and_send_request(
        socket_path: Path,
        request: str,
    ) -> Optional[str]:
        try:
            return await AsyncConnection.connect_and_send_request(
                socket_path,
                request,
            )
        except connections.ConnectionFailure:
            LOG.error(
                "Could not establish connection with an existing Pyre server "
                f"at {socket_path}."
            )
            return None

    @staticmethod
    async def attempt_query(
        socket_path: Path,
        query_text: str,
        overlay_id: Optional[str] = None,
    ) -> Optional[Response]:
        response_text = await AsyncConnection.attempt_connect_and_send_request(
            socket_path=socket_path,
            request=json.dumps(
                # TODO:T126924773 send a regular Query when overlay id is None
                [
                    "QueryWithOverlay",
                    {"query_text": query_text, "overlay_id": overlay_id},
                ]
            ),
        )
        try:
            return Response.parse(response_text) if response_text else None
        except InvalidQueryResponse as exception:
            LOG.info(
                f"Failed to parse json {response_text} due to exception: {exception}"
            )
            return None

    @staticmethod
    async def attempt_typed_query(
        response_type: Type[QueryResponseType],
        socket_path: Path,
        query_text: str,
        overlay_id: Optional[str] = None,
    ) -> Optional[QueryResponseType]:
        try:
            response = await AsyncConnection.attempt_query(
                socket_path,
                query_text,
                overlay_id,
            )
            if response is None:
                return None
            else:
                # pyre-ignore[16]: Pyre doesn't understand dataclasses_json
                return response_type.from_dict(response.payload)
        except (
            KeyError,
            ValueError,
            dataclasses_json.mm.ValidationError,
        ) as exception:
            LOG.info(
                f"When interpretting {response.payload} as {response_type.__name__} "
                f"got: {type(exception).__name__}({exception})"
            )
            return None
