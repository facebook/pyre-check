# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json

import testslide

from .... import json_rpc
from ....tests import setup
from ..async_server_connection import (
    TextReader,
    TextWriter,
    MemoryBytesReader,
    MemoryBytesWriter,
)
from ..language_server_protocol import read_json_rpc, write_json_rpc


class LSPInputOutputTest(testslide.TestCase):
    @setup.async_test
    async def test_read_lsp(self) -> None:
        async def assert_parses(input: str, expected: json_rpc.Request) -> None:
            actual = await read_json_rpc(
                TextReader(MemoryBytesReader(input.encode("utf-8")))
            )
            self.assertEqual(actual, expected)

        async def assert_not_parsed(input: str) -> None:
            with self.assertRaises(json_rpc.ParseError):
                await read_json_rpc(
                    TextReader(MemoryBytesReader(input.encode("utf-8")))
                )

        await assert_not_parsed("")
        await assert_not_parsed("derp")
        await assert_not_parsed("Invalid-Header: \r\n\r\n{}")
        await assert_not_parsed("Not-Content-Length: 42\r\n\r\n{}")
        await assert_not_parsed("Content-Length: derp\r\n\r\n{}")
        await assert_not_parsed(
            'Content-Length: 4\r\n\r\n{"jsonrpc": "2.0", "id": 0, "method": "foo"}'
        )
        await assert_parses(
            'Content-Length: 44\r\n\r\n{"jsonrpc": "2.0", "id": 0, "method": "foo"}',
            expected=json_rpc.Request(id=0, method="foo"),
        )
        await assert_parses(
            'CONTENT-LENGTH: 44\r\n\r\n{"jsonrpc": "2.0", "id": 0, "method": "foo"}',
            expected=json_rpc.Request(id=0, method="foo"),
        )
        await assert_parses(
            (
                "Content-Length: 44\r\n"
                "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n"
                '\r\n{"jsonrpc": "2.0", "id": 0, "method": "foo"}'
            ),
            expected=json_rpc.Request(id=0, method="foo"),
        )
        await assert_parses(
            (
                "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n"
                "Content-Length: 44\r\n"
                '\r\n{"jsonrpc": "2.0", "id": 0, "method": "foo"}'
            ),
            expected=json_rpc.Request(id=0, method="foo"),
        )

    @setup.async_test
    async def test_write_lsp(self) -> None:
        async def assert_write(response: json_rpc.Response, expected: str) -> None:
            bytes_writer = MemoryBytesWriter()
            await write_json_rpc(TextWriter(bytes_writer), response)
            actual = bytes_writer.items()[0].decode("utf-8")
            self.assertEqual(actual, expected)

        await assert_write(
            json_rpc.SuccessResponse(id=0, result=42),
            expected=(
                "Content-Length: 41\r\n\r\n"
                + json.dumps({"jsonrpc": "2.0", "id": 0, "result": 42})
            ),
        )
        await assert_write(
            json_rpc.ErrorResponse(id=0, code=42, message="derp"),
            expected=(
                "Content-Length: 69\r\n\r\n"
                + json.dumps(
                    {
                        "jsonrpc": "2.0",
                        "id": 0,
                        "error": {"code": 42, "message": "derp"},
                    }
                )
            ),
        )
