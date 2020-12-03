# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
from typing import Mapping

import testslide

from .... import json_rpc
from ....tests import setup
from ..async_server_connection import (
    TextWriter,
    MemoryBytesWriter,
    create_memory_text_reader,
)
from ..language_server_protocol import (
    read_json_rpc,
    write_json_rpc,
    InitializeParameters,
    Info,
    ClientCapabilities,
    TextDocumentClientCapabilities,
    TextDocumentSyncClientCapabilities,
    PublishDiagnosticsClientCapabilities,
    PublishDiagnosticsClientTagSupport,
    DiagnosticTag,
)


class LSPInputOutputTest(testslide.TestCase):
    @setup.async_test
    async def test_read_lsp(self) -> None:
        async def assert_parses(input: str, expected: json_rpc.Request) -> None:
            actual = await read_json_rpc(create_memory_text_reader(input))
            self.assertEqual(actual, expected)

        async def assert_not_parsed(input: str) -> None:
            with self.assertRaises(json_rpc.ParseError):
                await read_json_rpc(create_memory_text_reader(input))

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


class LSPParsingTest(testslide.TestCase):
    def test_parse_initialize(self) -> None:
        def assert_parsed(
            parameters: Mapping[str, object], expected: InitializeParameters
        ) -> None:
            self.assertEqual(
                InitializeParameters.from_json_rpc_parameters(
                    json_rpc.ByNameParameters(parameters)
                ),
                expected,
            )

        def assert_not_parsed(parameters: Mapping[str, object]) -> None:
            with self.assertRaises(json_rpc.InvalidRequestError):
                InitializeParameters.from_json_rpc_parameters(
                    json_rpc.ByNameParameters(parameters)
                )

        assert_not_parsed({})
        assert_not_parsed({"no_capabilities": 42})
        assert_not_parsed({"capabilities": True})
        assert_not_parsed({"processId": "derp"})
        assert_not_parsed({"capabilities": {"textDocument": "foo"}})
        assert_not_parsed(
            {
                "capabilities": {
                    "textDocument": {
                        "publishDiagnostics": {
                            "tagSupport": {"valueSet": [42]},
                        }
                    }
                }
            }
        )

        assert_parsed(
            {
                "processId": 42,
                "clientInfo": {"name": "foo", "version": "v0"},
                "rootUri": "file:///not_relevant",
                "initializationOptions": "not_relevant",
                "capabilities": {
                    "workspace": {},
                    "textDocument": {
                        "publishDiagnostics": {
                            "relatedInformation": True,
                            "versionSupport": False,
                            "tagSupport": {"valueSet": [1, 2]},
                        },
                        "synchronization": {
                            "dynamicRegistration": True,
                            "willSave": True,
                            "willSaveWaitUntil": True,
                            "didSave": True,
                        },
                        "completion": {},
                        "hover": {},
                    },
                },
                "trace": "off",
                "workspaceFolders": [
                    {"uri": "file:///workspalce_folder", "name": "foo"}
                ],
            },
            InitializeParameters(
                process_id=42,
                client_info=Info(name="foo", version="v0"),
                capabilities=ClientCapabilities(
                    text_document=TextDocumentClientCapabilities(
                        synchronization=TextDocumentSyncClientCapabilities(
                            did_save=True
                        ),
                        publish_diagnostics=PublishDiagnosticsClientCapabilities(
                            related_information=True,
                            tag_support=PublishDiagnosticsClientTagSupport(
                                value_set=[
                                    DiagnosticTag.UNNECESSARY,
                                    DiagnosticTag.DEPRECATED,
                                ]
                            ),
                            version_support=False,
                        ),
                    )
                ),
            ),
        )
