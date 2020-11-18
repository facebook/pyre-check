# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio
from typing import List, Iterable

from ... import json_rpc
from . import async_server_connection


async def _read_headers(input_channel: async_server_connection.TextReader) -> List[str]:
    headers = []
    header = await input_channel.read_until("\r\n")
    while header != "\r\n":
        headers.append(header)
        header = await input_channel.read_until("\r\n")
    return headers


def _get_content_length(headers: Iterable[str]) -> int:
    try:
        for header in headers:
            parts = [part.strip().lower() for part in header.split(":", maxsplit=1)]
            if len(parts) <= 1:
                continue

            if parts[0] == "content-length":
                return int(parts[1])

        raise json_rpc.ParseError(f"Failed to find content length header from {parts}")
    except ValueError as error:
        raise json_rpc.ParseError(f"Cannot parse content length into integer: {error}")


async def read_json_rpc(
    input_channel: async_server_connection.TextReader,
) -> json_rpc.Request:
    """
    Asynchronously read a JSON-RPC request from the given input channel.
    May raise `json_rpc.ParseError`, `json_rpc.InvalidRequestError` and
    `json_prc.InvalidParameterError`.
    """
    try:
        headers = await _read_headers(input_channel)
        content_length = _get_content_length(headers)

        payload = await input_channel.read_exactly(content_length)
        return json_rpc.Request.from_string(payload)
    except asyncio.IncompleteReadError as error:
        raise json_rpc.ParseError(str(error)) from error


async def write_json_rpc(
    output_channel: async_server_connection.TextWriter, response: json_rpc.Response
) -> None:
    payload = response.serialize()
    await output_channel.write(f"Content-Length: {len(payload)}\r\n\r\n{payload}")
