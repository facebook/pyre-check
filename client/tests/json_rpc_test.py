# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import unittest
from io import BytesIO

from ..json_rpc import Request, read_request, write_lsp_request


class JsonRPCTest(unittest.TestCase):
    def test_json(self) -> None:
        self.assertDictEqual(
            Request(method="textDocument/publishDiagnostics").json(),
            {"jsonrpc": "2.0", "method": "textDocument/publishDiagnostics"},
        )

        self.assertDictEqual(
            Request(method="textDocument/publishDiagnostics", id="123abc").json(),
            {
                "jsonrpc": "2.0",
                "id": "123abc",
                "method": "textDocument/publishDiagnostics",
            },
        )

        self.assertDictEqual(
            Request(
                method="textDocument/publishDiagnostics", parameters={"a": "b"}
            ).json(),
            {
                "jsonrpc": "2.0",
                "method": "textDocument/publishDiagnostics",
                "params": {"a": "b"},
            },
        )

        self.assertDictEqual(
            Request(
                method="textDocument/publishDiagnostics",
                id="123abc",
                parameters={"a": "b"},
            ).json(),
            {
                "jsonrpc": "2.0",
                "id": "123abc",
                "method": "textDocument/publishDiagnostics",
                "params": {"a": "b"},
            },
        )

    def test_read_message(self) -> None:
        # well-formed message
        file = BytesIO()
        file.write(
            b"Content-Length: 104\r\n"
            b"Content-Type: application/vscode-jsonrpc; charset=utf8\r\n"
            b"\r\n"
            b'{"jsonrpc":"2.0", "id": "123abc", "method":"textDocument/didSave",'
            b'"params": {"a": 123, "b": ["c", "d"]}}'
        )
        file.seek(0)

        result = read_request(file)

        self.assertNotEqual(result, None)
        # pyre-fixme[16]: Optional type has no attribute `id`.
        self.assertEqual(result.id, "123abc")
        # pyre-fixme[16]: Optional type has no attribute `method`.
        self.assertEqual(result.method, "textDocument/didSave")
        # pyre-fixme[16]: Optional type has no attribute `parameters`.
        self.assertDictEqual(result.parameters, {"a": 123, "b": ["c", "d"]})

        # end of file
        file = BytesIO()
        file.close()

        result = read_request(file)

        self.assertEqual(result, None)

        # broken header
        file = BytesIO()
        file.write(b"Content-Length: 123abc\r\n\r\n{}")
        file.seek(0)

        result = read_request(file)

        self.assertEqual(result, None)

        # missing json-rpc fields
        file = BytesIO()
        file.write(
            b"Content-Length: 87\r\n"
            b"\r\n"
            b'{"id": "123abc", "method":"textDocument/didSave",'
            b'"params": {"a": 123, "b": ["c", "d"]}}'
        )
        file.seek(0)

        result = read_request(file)

        self.assertEqual(result, None)

    def test_write_message(self) -> None:
        file = BytesIO()
        message = Request(
            method="textDocument/hover", id="123abc", parameters={"a": "b"}
        )
        self.assertTrue(write_lsp_request(file, message))
        file.seek(0)
        self.assertEqual(file.readline(), b"Content-Length: 88\r\n")
        self.assertEqual(file.readline(), b"\r\n")
        self.assertDictEqual(
            json.loads(file.readline().decode("utf-8")),
            {
                "jsonrpc": "2.0",
                "method": "textDocument/hover",
                "id": "123abc",
                "params": {"a": "b"},
            },
        )

        file.close()
        self.assertFalse(write_lsp_request(file, message))

    def test_read_write(self) -> None:
        file = BytesIO()
        message = Request(
            method="textDocument/definition",
            id="123abc",
            parameters={"a": "b", "c": "d"},
        )

        write_lsp_request(file, message)
        file.seek(0)
        parsed_message = read_request(file)

        self.assertNotEqual(parsed_message, None)
        # pyre-fixme[16]: Optional type has no attribute `id`.
        self.assertEqual(message.id, parsed_message.id)
        # pyre-fixme[16]: Optional type has no attribute `method`.
        self.assertEqual(message.method, parsed_message.method)
        # pyre-fixme[6]: Expected `Dict[Any, Any]` for 1st param but got
        #  `Optional[Dict[str, Any]]`.
        # pyre-fixme[16]: Optional type has no attribute `parameters`.
        self.assertDictEqual(message.parameters, parsed_message.parameters)
