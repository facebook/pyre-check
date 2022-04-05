# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import unittest
from typing import Type

from ..json_rpc import (
    ByNameParameters,
    ByPositionParameters,
    ErrorResponse,
    InvalidParameterError,
    InvalidRequestError,
    JSON,
    JSONRPCException,
    ParseError,
    Request,
    Response,
    SuccessResponse,
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
