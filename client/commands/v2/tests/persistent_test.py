# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
from pathlib import Path

import testslide

from .... import json_rpc, error
from ....tests import setup
from .. import language_server_protocol as lsp
from ..async_server_connection import (
    TextReader,
    TextWriter,
    MemoryBytesReader,
    MemoryBytesWriter,
    create_memory_text_writer,
    create_memory_text_reader,
    BackgroundTask,
    BackgroundTaskManager,
)
from ..incremental import InvalidServerResponse
from ..persistent import (
    try_initialize,
    InitializationSuccess,
    InitializationFailure,
    InitializationExit,
    Server,
    ServerState,
    parse_subscription_response,
    SubscriptionResponse,
)


async def _create_input_channel_with_request(request: json_rpc.Request) -> TextReader:
    bytes_writer = MemoryBytesWriter()
    await lsp.write_json_rpc(TextWriter(bytes_writer), request)
    return TextReader(MemoryBytesReader(bytes_writer.items()[0]))


class NoOpBackgroundTask(BackgroundTask):
    async def run(self) -> None:
        pass


class PersistentTest(testslide.TestCase):
    @setup.async_test
    async def test_try_initialize_success(self) -> None:
        input_channel = await _create_input_channel_with_request(
            json_rpc.Request(
                id=0,
                method="initialize",
                parameters=json_rpc.ByNameParameters(
                    {
                        "processId": 42,
                        "rootUri": None,
                        "capabilities": {
                            "textDocument": {
                                "publishDiagnostics": {},
                                "synchronization": {
                                    "didSave": True,
                                },
                            },
                        },
                    }
                ),
            )
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(input_channel, output_channel)
        self.assertIsInstance(result, InitializationSuccess)

    @setup.async_test
    async def test_try_initialize_failure__not_a_request(self) -> None:
        input_channel = await _create_input_channel_with_request(
            json_rpc.Request(method="derp", parameters=None)
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(input_channel, output_channel)
        self.assertIsInstance(result, InitializationFailure)

    @setup.async_test
    async def test_try_initialize_failure__invalid_parameters(self) -> None:
        input_channel = await _create_input_channel_with_request(
            json_rpc.Request(id=0, method="initialize", parameters=None)
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(input_channel, output_channel)
        self.assertIsInstance(result, InitializationFailure)

    @setup.async_test
    async def test_try_initialize_exit(self) -> None:
        input_channel = await _create_input_channel_with_request(
            json_rpc.Request(method="exit", parameters=None)
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(input_channel, output_channel)
        self.assertIsInstance(result, InitializationExit)

    def test_parse_subscription(self) -> None:
        def assert_parsed(response: str, expected: SubscriptionResponse) -> None:
            self.assertEqual(
                parse_subscription_response(response),
                expected,
            )

        def assert_not_parsed(response: str) -> None:
            with self.assertRaises(InvalidServerResponse):
                parse_subscription_response(response)

        assert_not_parsed("derp")
        assert_not_parsed("{}")
        assert_not_parsed("[]")
        assert_not_parsed('["Error"]')
        assert_not_parsed('{"name": "foo", "no_body": []}')
        assert_not_parsed('{"body": [], "no_name": "foo"}')

        assert_parsed(
            json.dumps({"name": "foo", "body": ["TypeErrors", []]}),
            expected=SubscriptionResponse(name="foo"),
        )
        assert_parsed(
            json.dumps(
                {
                    "name": "foo",
                    "body": [
                        "TypeErrors",
                        [
                            {
                                "line": 1,
                                "column": 1,
                                "stop_line": 2,
                                "stop_column": 2,
                                "path": "test.py",
                                "code": 42,
                                "name": "Fake name",
                                "description": "Fake description",
                            },
                        ],
                    ],
                }
            ),
            expected=SubscriptionResponse(
                name="foo",
                body=[
                    error.Error(
                        line=1,
                        column=1,
                        stop_line=2,
                        stop_column=2,
                        path=Path("test.py"),
                        code=42,
                        name="Fake name",
                        description="Fake description",
                    ),
                ],
            ),
        )

    def test_open_close(self) -> None:
        server_state = ServerState()
        server = Server(
            input_channel=create_memory_text_reader(""),
            output_channel=create_memory_text_writer(),
            client_capabilities=lsp.ClientCapabilities(),
            state=server_state,
            pyre_manager=BackgroundTaskManager(NoOpBackgroundTask()),
        )
        test_path0 = Path("/foo/bar")
        test_path1 = Path("/foo/baz")

        server.process_open_request(
            lsp.DidOpenTextDocumentParameters(
                text_document=lsp.TextDocumentItem(
                    language_id="python",
                    text="",
                    uri=lsp.DocumentUri.from_file_path(test_path0).unparse(),
                    version=0,
                )
            )
        )
        self.assertIn(test_path0, server_state.opened_documents)

        server.process_open_request(
            lsp.DidOpenTextDocumentParameters(
                text_document=lsp.TextDocumentItem(
                    language_id="python",
                    text="",
                    uri=lsp.DocumentUri.from_file_path(test_path1).unparse(),
                    version=0,
                )
            )
        )
        self.assertIn(test_path1, server_state.opened_documents)

        server.process_close_request(
            lsp.DidCloseTextDocumentParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(test_path0).unparse()
                )
            )
        )
        self.assertNotIn(test_path0, server_state.opened_documents)
