# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import unittest
from io import BytesIO

from ..language_server_protocol import (
    LanguageServerProtocolMessage,
    read_message,
    write_message,
)


class LanguageServerProtocolTest(unittest.TestCase):
    def test_json(self):
        self.assertDictEqual(
            json.loads(
                LanguageServerProtocolMessage(
                    method="textDocument/publishDiagnostics"
                ).json()
            ),
            {"jsonrpc": "2.0", "method": "textDocument/publishDiagnostics"},
        )

        self.assertDictEqual(
            json.loads(
                LanguageServerProtocolMessage(
                    method="textDocument/publishDiagnostics", id="123abc"
                ).json()
            ),
            {
                "jsonrpc": "2.0",
                "id": "123abc",
                "method": "textDocument/publishDiagnostics",
            },
        )

        self.assertDictEqual(
            json.loads(
                LanguageServerProtocolMessage(
                    method="textDocument/publishDiagnostics", parameters={"a": "b"}
                ).json()
            ),
            {
                "jsonrpc": "2.0",
                "method": "textDocument/publishDiagnostics",
                "params": {"a": "b"},
            },
        )

        self.assertDictEqual(
            json.loads(
                LanguageServerProtocolMessage(
                    method="textDocument/publishDiagnostics",
                    id="123abc",
                    parameters={"a": "b"},
                ).json()
            ),
            {
                "jsonrpc": "2.0",
                "id": "123abc",
                "method": "textDocument/publishDiagnostics",
                "params": {"a": "b"},
            },
        )

    def test_read_message(self):
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

        result = read_message(file)

        self.assertNotEqual(result, None)
        self.assertEqual(result.id, "123abc")
        self.assertEqual(result.method, "textDocument/didSave")
        self.assertDictEqual(result.parameters, {"a": 123, "b": ["c", "d"]})

        # end of file
        file = BytesIO()
        file.close()

        result = read_message(file)

        self.assertEqual(result, None)

        # broken header
        file = BytesIO()
        file.write(b"Content-Length: 123abc\r\n\r\n{}")
        file.seek(0)

        result = read_message(file)

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

        result = read_message(file)

        self.assertEqual(result, None)

    def test_write_message(self):
        file = BytesIO()
        message = LanguageServerProtocolMessage(
            method="textDocument/hover", id="123abc", parameters={"a": "b"}
        )
        self.assertTrue(write_message(file, message))
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
        self.assertFalse(write_message(file, message))

    def test_read_write(self):
        file = BytesIO()
        message = LanguageServerProtocolMessage(
            method="textDocument/definition",
            id="123abc",
            parameters={"a": "b", "c": "d"},
        )

        write_message(file, message)
        file.seek(0)
        parsed_message = read_message(file)

        self.assertNotEqual(parsed_message, None)
        self.assertEqual(message.id, parsed_message.id)
        self.assertEqual(message.method, parsed_message.method)
        self.assertDictEqual(message.parameters, parsed_message.parameters)
