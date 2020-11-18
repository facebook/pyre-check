# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import unittest
from io import BytesIO
from typing import Type

from ..json_rpc import (
    Request,
    Response,
    SuccessResponse,
    ErrorResponse,
    read_lsp_request,
    write_lsp_request,
    ByNameParameters,
    ByPositionParameters,
    JSONRPCException,
    JSON,
    ParseError,
    InvalidRequestError,
    InvalidParameterError,
)


class JsonRPCTest(unittest.TestCase):
    def test_request_serialize(self) -> None:
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
                method="textDocument/publishDiagnostics",
                parameters=ByNameParameters({"a": "b"}),
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
                parameters=ByNameParameters({"a": "b"}),
            ).json(),
            {
                "jsonrpc": "2.0",
                "id": "123abc",
                "method": "textDocument/publishDiagnostics",
                "params": {"a": "b"},
            },
        )

    def test_request_parsing(self) -> None:
        def assert_not_parsed(
            input: str, exception: Type[JSONRPCException] = JSONRPCException
        ) -> None:
            with self.assertRaises(exception):
                Request.from_string(input)

        def assert_parsed(input: JSON, expected: Request) -> None:
            self.assertEqual(Request.from_json(input), expected)

        assert_not_parsed("", ParseError)
        assert_not_parsed("derp", ParseError)
        assert_not_parsed(json.dumps({"no_version": 42}), InvalidRequestError)
        assert_not_parsed(json.dumps({"jsonrpc": "2.0"}), InvalidRequestError)
        assert_not_parsed(
            json.dumps({"jsonrpc": "2.0", "method": 42}), InvalidRequestError
        )
        assert_not_parsed(
            json.dumps({"jsonrpc": "2.0", "method": "foo", "id": []}),
            InvalidRequestError,
        )
        assert_not_parsed(
            json.dumps({"jsonrpc": "2.0", "method": "foo", "params": 42}),
            InvalidParameterError,
        )
        assert_parsed({"jsonrpc": "2.0", "method": "foo"}, Request(method="foo"))
        assert_parsed(
            {"jsonrpc": "2.0", "method": "foo", "id": None}, Request(method="foo")
        )
        assert_parsed(
            {"jsonrpc": "2.0", "method": "foo", "id": 42}, Request(method="foo", id=42)
        )
        assert_parsed(
            {"jsonrpc": "2.0", "method": "foo", "id": "derp"},
            Request(method="foo", id="derp"),
        )
        assert_parsed(
            {"jsonrpc": "2.0", "method": "foo", "id": "derp", "params": None},
            Request(method="foo", id="derp"),
        )
        assert_parsed(
            {"jsonrpc": "2.0", "method": "foo", "id": 42, "params": []},
            Request(method="foo", id=42, parameters=ByPositionParameters()),
        )
        assert_parsed(
            {"jsonrpc": "2.0", "method": "foo", "id": 42, "params": [1, "bar"]},
            Request(method="foo", id=42, parameters=ByPositionParameters([1, "bar"])),
        )
        assert_parsed(
            {"jsonrpc": "2.0", "method": "foo", "id": 42, "params": {}},
            Request(method="foo", id=42, parameters=ByNameParameters()),
        )
        assert_parsed(
            {
                "jsonrpc": "2.0",
                "method": "foo",
                "id": 42,
                "params": {"bar": 42, "baz": False},
            },
            Request(
                method="foo",
                id=42,
                parameters=ByNameParameters({"bar": 42, "baz": False}),
            ),
        )

    def test_response_parsing(self) -> None:
        def assert_not_parsed(
            input: str, exception: Type[JSONRPCException] = JSONRPCException
        ) -> None:
            with self.assertRaises(exception):
                Response.from_string(input)

        def assert_parsed(input: JSON, expected: Response) -> None:
            self.assertEqual(Response.from_json(input), expected)

        assert_not_parsed("", ParseError)
        assert_not_parsed("derp", ParseError)
        assert_not_parsed(json.dumps({"no_version": 42}), InvalidRequestError)
        assert_not_parsed(json.dumps({"jsonrpc": "2.0"}), InvalidRequestError)
        assert_not_parsed(
            json.dumps({"jsonrpc": "2.0", "id": False}), InvalidRequestError
        )
        assert_not_parsed(
            json.dumps({"jsonrpc": "2.0", "id": "foo"}), InvalidRequestError
        )
        assert_not_parsed(
            json.dumps({"jsonrpc": "2.0", "id": None, "error": 42}), InvalidRequestError
        )
        assert_not_parsed(
            json.dumps({"jsonrpc": "2.0", "id": None, "error": {"no_code": 42}}),
            InvalidRequestError,
        )
        assert_not_parsed(
            json.dumps({"jsonrpc": "2.0", "id": None, "error": {"code": "derp"}}),
            InvalidRequestError,
        )
        assert_not_parsed(
            json.dumps(
                {"jsonrpc": "2.0", "id": None, "error": {"code": 42, "message": []}}
            ),
            InvalidRequestError,
        )
        assert_parsed(
            {"jsonrpc": "2.0", "id": None, "result": 42},
            SuccessResponse(id=None, result=42),
        )
        assert_parsed(
            {"jsonrpc": "2.0", "id": 0, "result": 42},
            SuccessResponse(id=0, result=42),
        )
        assert_parsed(
            {"jsonrpc": "2.0", "id": "derp", "result": 42},
            SuccessResponse(id="derp", result=42),
        )
        assert_parsed(
            {"jsonrpc": "2.0", "id": 0, "error": {"code": 42}},
            ErrorResponse(id=0, code=42),
        )
        assert_parsed(
            {"jsonrpc": "2.0", "id": 0, "error": {"code": 42, "message": "foo"}},
            ErrorResponse(id=0, code=42, message="foo"),
        )
        assert_parsed(
            {
                "jsonrpc": "2.0",
                "id": 0,
                "error": {"code": 42, "message": "foo", "data": [1, True]},
            },
            ErrorResponse(id=0, code=42, message="foo", data=[1, True]),
        )

    def test_read_message(self) -> None:
        # well-formed message
        result = read_lsp_request(
            BytesIO(
                b"Content-Length: 104\r\n"
                b"Content-Type: application/vscode-jsonrpc; charset=utf8\r\n"
                b"\r\n"
                b'{"jsonrpc":"2.0", "id": "123abc", "method":"textDocument/didSave",'
                b'"params": {"a": 123, "b": ["c", "d"]}}'
            )
        )

        self.assertEqual(result.id, "123abc")
        self.assertEqual(result.method, "textDocument/didSave")

        parameters = result.parameters
        self.assertIsNotNone(parameters)
        self.assertEqual(parameters.values, {"a": 123, "b": ["c", "d"]})

        # end of file
        with self.assertRaises(JSONRPCException):
            result = read_lsp_request(BytesIO())

        # broken header
        with self.assertRaises(JSONRPCException):
            result = read_lsp_request(BytesIO(b"Content-Length: 123abc\r\n\r\n{}"))

        # missing json-rpc fields
        with self.assertRaises(JSONRPCException):
            result = read_lsp_request(
                BytesIO(
                    b"Content-Length: 87\r\n"
                    b"\r\n"
                    b'{"id": "123abc", "method":"textDocument/didSave",'
                    b'"params": {"a": 123, "b": ["c", "d"]}}'
                )
            )

    def test_write_message(self) -> None:
        file = BytesIO()
        message = Request(
            method="textDocument/hover",
            id="123abc",
            parameters=ByNameParameters({"a": "b"}),
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
            parameters=ByNameParameters({"a": "b", "c": "d"}),
        )

        write_lsp_request(file, message)
        file.seek(0)
        parsed_message = read_lsp_request(file)

        self.assertEquals(message, parsed_message)
