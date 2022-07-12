# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import functools
import json
from pathlib import Path
from typing import Callable, Mapping, Optional, TypeVar

import testslide

from ... import json_rpc
from ...tests import setup
from ..async_server_connection import (
    create_memory_text_reader,
    MemoryBytesWriter,
    TextWriter,
)
from ..language_server_protocol import (
    ClientCapabilities,
    DiagnosticTag,
    DidChangeTextDocumentParameters,
    DidCloseTextDocumentParameters,
    DidOpenTextDocumentParameters,
    DidSaveTextDocumentParameters,
    DocumentUri,
    Info,
    InitializationOptions,
    InitializeParameters,
    PublishDiagnosticsClientCapabilities,
    PublishDiagnosticsClientTagSupport,
    read_json_rpc,
    ShowStatusRequestClientCapabilities,
    TextDocumentClientCapabilities,
    TextDocumentIdentifier,
    TextDocumentItem,
    TextDocumentSyncClientCapabilities,
    WindowClientCapabilities,
    write_json_rpc,
)

T = TypeVar("T")


class DocumentUriTest(testslide.TestCase):
    def test_to_file_path(self) -> None:
        def assert_file_path(uri: str, expected: Optional[Path]) -> None:
            self.assertEqual(DocumentUri.parse(uri).to_file_path(), expected)

        assert_file_path("file:///foo/bar", expected=Path("/foo/bar"))
        assert_file_path("file:///foo/space%20%3Fbar", expected=Path("/foo/space ?bar"))
        assert_file_path("file:///foo/bar#frag", expected=Path("/foo/bar"))
        assert_file_path("file://localhost/etc/fstab", expected=Path("/etc/fstab"))
        assert_file_path(
            "file://hostname/path/to/the%20file.txt",
            expected=Path("/path/to/the file.txt"),
        )
        assert_file_path("https://pyre-check.org/", expected=None)
        assert_file_path("nfs://server//a/b/c", expected=None)
        assert_file_path("ssh://foo@192.168.1.1", expected=None)

    def test_from_file_path(self) -> None:
        def assert_file_path(path: Path, expected: str) -> None:
            self.assertEqual(DocumentUri.from_file_path(path).unparse(), expected)

        assert_file_path(Path("/foo/bar"), expected="file:///foo/bar")
        assert_file_path(Path("/foo/space ?bar"), expected="file:///foo/space%20%3Fbar")


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
    def assert_parsed(
        self,
        parser: Callable[[json_rpc.Parameters], T],
        parameters: Mapping[str, object],
        expected: T,
    ) -> None:
        self.assertEqual(
            parser(json_rpc.ByNameParameters(parameters)),
            expected,
        )

    def assert_not_parsed(
        self,
        parser: Callable[[json_rpc.Parameters], T],
        parameters: Mapping[str, object],
    ) -> None:
        with self.assertRaises(json_rpc.InvalidRequestError):
            parser(json_rpc.ByNameParameters(parameters))

    def test_parse_initialize(self) -> None:
        assert_parsed = functools.partial(
            self.assert_parsed, InitializeParameters.from_json_rpc_parameters
        )
        assert_not_parsed = functools.partial(
            self.assert_not_parsed, InitializeParameters.from_json_rpc_parameters
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
                "initializationOptions": {"notebookNumber": 12345},
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
                    "window": {
                        "workDoneProgress": True,
                        "status": {"dynamicRegistration": False},
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
                    ),
                    window=WindowClientCapabilities(
                        work_done_progress=True,
                        status=ShowStatusRequestClientCapabilities(),
                    ),
                ),
                initialization_options=InitializationOptions(notebook_number=12345),
            ),
        )

    def test_parse_did_open(self) -> None:
        assert_parsed = functools.partial(
            self.assert_parsed, DidOpenTextDocumentParameters.from_json_rpc_parameters
        )
        assert_not_parsed = functools.partial(
            self.assert_not_parsed,
            DidOpenTextDocumentParameters.from_json_rpc_parameters,
        )

        assert_not_parsed({})
        assert_not_parsed({"no_text_document": 42})
        assert_not_parsed({"textDocument": "derp"})
        assert_not_parsed(
            {"textDocument": {"missing_uri": "foo", "version": 0, "text": ""}}
        )
        assert_parsed(
            {
                "textDocument": {
                    "languageId": "python",
                    "text": "foo",
                    "uri": "file:///home/user/test.py",
                    "version": 0,
                }
            },
            expected=DidOpenTextDocumentParameters(
                text_document=TextDocumentItem(
                    language_id="python",
                    text="foo",
                    uri="file:///home/user/test.py",
                    version=0,
                )
            ),
        )

    def test_parse_did_close(self) -> None:
        assert_parsed = functools.partial(
            self.assert_parsed, DidCloseTextDocumentParameters.from_json_rpc_parameters
        )
        assert_not_parsed = functools.partial(
            self.assert_not_parsed,
            DidCloseTextDocumentParameters.from_json_rpc_parameters,
        )

        assert_not_parsed({})
        assert_not_parsed({"no_text_document": 42})
        assert_not_parsed({"textDocument": "derp"})
        assert_not_parsed({"textDocument": {"missing_uri": "foo"}})
        assert_parsed(
            {
                "textDocument": {
                    "uri": "file:///home/user/test.py",
                }
            },
            expected=DidCloseTextDocumentParameters(
                text_document=TextDocumentIdentifier(
                    uri="file:///home/user/test.py",
                )
            ),
        )

    def test_parse_did_save(self) -> None:
        assert_parsed = functools.partial(
            self.assert_parsed, DidSaveTextDocumentParameters.from_json_rpc_parameters
        )
        assert_not_parsed = functools.partial(
            self.assert_not_parsed,
            DidSaveTextDocumentParameters.from_json_rpc_parameters,
        )

        assert_not_parsed({})
        assert_not_parsed({"no_text_document": 42})

        assert_parsed(
            {
                "textDocument": {
                    "uri": "file:///home/user/test.py",
                }
            },
            expected=DidSaveTextDocumentParameters(
                text_document=TextDocumentIdentifier(uri="file:///home/user/test.py")
            ),
        )
        assert_parsed(
            {
                "textDocument": {
                    "uri": "file:///home/user/test.py",
                },
                "text": "foo",
            },
            expected=DidSaveTextDocumentParameters(
                text_document=TextDocumentIdentifier(uri="file:///home/user/test.py"),
                text="foo",
            ),
        )

    def test_parse_did_change(self) -> None:
        assert_parsed = functools.partial(
            self.assert_parsed, DidChangeTextDocumentParameters.from_json_rpc_parameters
        )
        assert_not_parsed = functools.partial(
            self.assert_not_parsed,
            DidChangeTextDocumentParameters.from_json_rpc_parameters,
        )

        assert_not_parsed({})
        assert_not_parsed({"no_text_document": 42})
        assert_not_parsed({"textDocument": "derp"})

        assert_parsed(
            {
                "textDocument": {
                    "uri": "file:///home/user/test.py",
                }
            },
            expected=DidChangeTextDocumentParameters(
                text_document=TextDocumentIdentifier(uri="file:///home/user/test.py")
            ),
        )
        assert_parsed(
            {
                "textDocument": {
                    "uri": "file:///home/user/test.py",
                },
                "text": "foo",
            },
            expected=DidChangeTextDocumentParameters(
                text_document=TextDocumentIdentifier(uri="file:///home/user/test.py"),
            ),
        )
