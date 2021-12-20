# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio
import json
import tempfile
from pathlib import Path
from typing import Sequence, Iterable, Optional

import testslide
from libcst.metadata import CodeRange, CodePosition

from ... import json_rpc, error, configuration as configuration_module
from ...coverage_collector import CoveredAndUncoveredLines
from ...tests import setup
from .. import language_server_protocol as lsp, start, backend_arguments
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
    LocationTypeLookup,
    PyreServer,
    PyreServerStartOptions,
    PyreServerStartOptionsReader,
    PyreQueryHandler,
    PyreQueryState,
    ServerState,
    parse_subscription_response,
    TypeErrorSubscription,
    StatusUpdateSubscription,
    SubscriptionResponse,
    type_error_to_diagnostic,
    type_errors_to_diagnostics,
    uncovered_range_to_diagnostic,
    to_coverage_result,
    PyreServerHandler,
    CONSECUTIVE_START_ATTEMPT_THRESHOLD,
)


def _create_server_start_options_reader(
    binary: str,
    server_identifier: str,
    start_arguments: start.Arguments,
    ide_features: Optional[configuration_module.IdeFeatures] = None,
    strict_defualt: bool = False,
    excludes: Optional[Sequence[str]] = None,
) -> PyreServerStartOptionsReader:
    def reader() -> PyreServerStartOptions:
        return PyreServerStartOptions(
            binary,
            server_identifier,
            start_arguments,
            ide_features,
            strict_defualt,
            excludes if excludes else [],
        )

    return reader


async def _create_input_channel_with_requests(
    requests: Iterable[json_rpc.Request],
) -> TextReader:
    bytes_writer = MemoryBytesWriter()
    for request in requests:
        await lsp.write_json_rpc(TextWriter(bytes_writer), request)
    return TextReader(MemoryBytesReader(b"\n".join(bytes_writer.items())))


class NoOpBackgroundTask(BackgroundTask):
    async def run(self) -> None:
        pass


class WaitForeverBackgroundTask(BackgroundTask):
    async def run(self) -> None:
        await asyncio.Event().wait()


class PersistentTest(testslide.TestCase):
    def fake_option_reader(self) -> PyreServerStartOptionsReader:
        return _create_server_start_options_reader(
            binary="/not/relevant",
            server_identifier="not_relevant",
            start_arguments=start.Arguments(
                base_arguments=backend_arguments.BaseArguments(
                    source_paths=backend_arguments.SimpleSourcePath(),
                    log_path="/log/path",
                    global_root="/global/root",
                )
            ),
        )

    @setup.async_test
    async def test_try_initialize_success(self) -> None:
        input_channel = await _create_input_channel_with_requests(
            [
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
                ),
                json_rpc.Request(
                    method="initialized", parameters=json_rpc.ByNameParameters({})
                ),
            ]
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(
            input_channel, output_channel, self.fake_option_reader()
        )
        self.assertIsInstance(result, InitializationSuccess)

    @setup.async_test
    async def test_try_initialize_failure__not_a_request(self) -> None:
        input_channel = await _create_input_channel_with_requests(
            [json_rpc.Request(method="derp", parameters=None)]
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(
            input_channel, output_channel, self.fake_option_reader()
        )
        self.assertIsInstance(result, InitializationFailure)

    @setup.async_test
    async def test_try_initialize_failure__invalid_parameters(self) -> None:
        input_channel = await _create_input_channel_with_requests(
            [json_rpc.Request(id=0, method="initialize", parameters=None)]
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(
            input_channel, output_channel, self.fake_option_reader()
        )
        self.assertIsInstance(result, InitializationFailure)

    @setup.async_test
    async def test_try_initialize_failure__no_initialized(self) -> None:
        input_channel = await _create_input_channel_with_requests(
            [
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
                ),
                json_rpc.Request(
                    method="derp", parameters=json_rpc.ByNameParameters({})
                ),
            ]
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(
            input_channel, output_channel, self.fake_option_reader()
        )
        self.assertIsInstance(result, InitializationFailure)

    @setup.async_test
    async def test_try_initialize_exit(self) -> None:
        input_channel = await _create_input_channel_with_requests(
            [json_rpc.Request(method="exit", parameters=None)]
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(
            input_channel, output_channel, self.fake_option_reader()
        )
        self.assertIsInstance(result, InitializationExit)

    @setup.async_test
    async def test_try_initialize_exit__shutdown_after_initialize(self) -> None:
        input_channel = await _create_input_channel_with_requests(
            [
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
                ),
                json_rpc.Request(method="shutdown", parameters=None),
                json_rpc.Request(method="exit", parameters=None),
            ]
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(
            input_channel, output_channel, self.fake_option_reader()
        )
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
        assert_not_parsed('{"name": "foo", "body": ["Malformed"]}')
        assert_not_parsed('{"name": "foo", "body": ["TypeErrors", {}]}')
        assert_not_parsed('{"name": "foo", "body": ["StatusUpdate", 42]}')
        assert_not_parsed('{"name": "foo", "body": ["StatusUpdate", []]}')

        assert_parsed(
            json.dumps({"name": "foo", "body": ["TypeErrors", []]}),
            expected=SubscriptionResponse(name="foo", body=TypeErrorSubscription()),
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
                body=TypeErrorSubscription(
                    [
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
                    ]
                ),
            ),
        )
        assert_parsed(
            json.dumps(
                {
                    "name": "foo",
                    "body": ["StatusUpdate", ["derp"]],
                }
            ),
            expected=SubscriptionResponse(
                name="foo",
                body=StatusUpdateSubscription(kind="derp"),
            ),
        )

    @setup.async_test
    async def test_open_close(self) -> None:
        server_state = ServerState()
        fake_task_manager = BackgroundTaskManager(WaitForeverBackgroundTask())
        server = PyreServer(
            input_channel=create_memory_text_reader(""),
            output_channel=create_memory_text_writer(),
            state=server_state,
            pyre_manager=BackgroundTaskManager(NoOpBackgroundTask()),
            pyre_query_manager=fake_task_manager,
        )
        test_path0 = Path("/foo/bar")
        test_path1 = Path("/foo/baz")

        await server.process_open_request(
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
        self.assertEqual(
            server.state.query_state.paths_to_be_queried.qsize(),
            1,
        )
        self.assertEqual(
            server.state.query_state.paths_to_be_queried.get_nowait(),
            test_path0,
        )

        await server.process_open_request(
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
        self.assertEqual(
            server.state.query_state.paths_to_be_queried.qsize(),
            1,
        )
        self.assertEqual(
            server.state.query_state.paths_to_be_queried.get_nowait(),
            test_path1,
        )

        await server.process_close_request(
            lsp.DidCloseTextDocumentParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(test_path0).unparse()
                )
            )
        )
        self.assertNotIn(test_path0, server_state.opened_documents)
        self.assertEqual(
            server.state.query_state.paths_to_be_queried.qsize(),
            0,
        )

    @setup.async_test
    async def test_subscription_protocol(self) -> None:
        server_state = ServerState(
            client_capabilities=lsp.ClientCapabilities(
                window=lsp.WindowClientCapabilities(
                    status=lsp.ShowStatusRequestClientCapabilities(),
                ),
            )
        )
        bytes_writer = MemoryBytesWriter()

        def fake_server_start_options_reader() -> PyreServerStartOptions:
            # Server start option is not relevant to this test
            raise NotImplementedError

        server_handler = PyreServerHandler(
            server_start_options_reader=fake_server_start_options_reader,
            client_output_channel=TextWriter(bytes_writer),
            server_state=server_state,
        )
        await server_handler.handle_status_update_subscription(
            StatusUpdateSubscription(kind="Rebuilding")
        )
        await server_handler.handle_status_update_subscription(
            StatusUpdateSubscription(kind="Rechecking")
        )
        await server_handler.handle_type_error_subscription(
            TypeErrorSubscription(
                errors=[
                    error.Error(
                        line=1,
                        column=1,
                        stop_line=2,
                        stop_column=2,
                        path=Path("derp.py"),
                        code=42,
                        name="name",
                        description="description",
                    )
                ]
            )
        )

        client_messages = [x.decode("utf-8") for x in bytes_writer.items()]
        self.assertTrue(len(client_messages) >= 4)
        # Forward the rebuild status message
        self.assertIn("window/showStatus", client_messages[0])
        # Forward the recheck status message
        self.assertIn("window/showStatus", client_messages[1])
        # Clear out diagnostics for subsequent type errors
        self.assertIn("textDocument/publishDiagnostics", client_messages[2])
        # Notify the user that incremental check has finished
        self.assertIn("window/showStatus", client_messages[3])

    @setup.async_test
    async def test_busy_status_clear_diagnostics(self) -> None:
        path = Path("foo.py")
        server_state = ServerState(diagnostics={path: []})
        bytes_writer = MemoryBytesWriter()

        def fake_server_start_options_reader() -> PyreServerStartOptions:
            # Server start option is not relevant to this test
            raise NotImplementedError

        server_handler = PyreServerHandler(
            server_start_options_reader=fake_server_start_options_reader,
            client_output_channel=TextWriter(bytes_writer),
            server_state=server_state,
        )
        await server_handler.handle_status_update_subscription(
            StatusUpdateSubscription(kind="Rebuilding")
        )
        await server_handler.handle_status_update_subscription(
            StatusUpdateSubscription(kind="Rechecking")
        )

        client_messages = [x.decode("utf-8") for x in bytes_writer.items()]
        self.assertTrue(len(client_messages) >= 4)
        # Clear out diagnostics for rebuilding status
        self.assertIn('"diagnostics": []', client_messages[0])
        self.assertIn('"diagnostics": []', client_messages[2])

    @setup.async_test
    async def test_open_triggers_pyre_restart(self) -> None:
        fake_task_manager = BackgroundTaskManager(WaitForeverBackgroundTask())
        fake_task_manager2 = BackgroundTaskManager(WaitForeverBackgroundTask())
        server = PyreServer(
            input_channel=create_memory_text_reader(""),
            output_channel=create_memory_text_writer(),
            state=ServerState(),
            pyre_manager=fake_task_manager,
            pyre_query_manager=fake_task_manager2,
        )
        self.assertFalse(fake_task_manager.is_task_running())

        test_path = Path("/foo.py")
        await server.process_open_request(
            lsp.DidOpenTextDocumentParameters(
                text_document=lsp.TextDocumentItem(
                    language_id="python",
                    text="",
                    uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                    version=0,
                )
            )
        )
        await asyncio.sleep(0)
        self.assertTrue(fake_task_manager.is_task_running())

    @setup.async_test
    async def test_open_triggers_pyre_restart__limit_reached(self) -> None:
        fake_task_manager = BackgroundTaskManager(WaitForeverBackgroundTask())
        fake_task_manager2 = BackgroundTaskManager(WaitForeverBackgroundTask())
        server = PyreServer(
            input_channel=create_memory_text_reader(""),
            output_channel=create_memory_text_writer(),
            state=ServerState(
                consecutive_start_failure=CONSECUTIVE_START_ATTEMPT_THRESHOLD
            ),
            pyre_manager=fake_task_manager,
            pyre_query_manager=fake_task_manager2,
        )
        self.assertFalse(fake_task_manager.is_task_running())

        test_path = Path("/foo.py")
        await server.process_open_request(
            lsp.DidOpenTextDocumentParameters(
                text_document=lsp.TextDocumentItem(
                    language_id="python",
                    text="",
                    uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                    version=0,
                )
            )
        )
        await asyncio.sleep(0)
        self.assertFalse(fake_task_manager.is_task_running())

    @setup.async_test
    async def test_save_triggers_pyre_restart(self) -> None:
        test_path = Path("/foo.py")
        fake_task_manager = BackgroundTaskManager(WaitForeverBackgroundTask())
        fake_task_manager2 = BackgroundTaskManager(WaitForeverBackgroundTask())
        server = PyreServer(
            input_channel=create_memory_text_reader(""),
            output_channel=create_memory_text_writer(),
            state=ServerState(opened_documents={test_path}),
            pyre_manager=fake_task_manager,
            pyre_query_manager=fake_task_manager2,
        )
        self.assertFalse(fake_task_manager.is_task_running())

        await server.process_did_save_request(
            lsp.DidSaveTextDocumentParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                )
            )
        )
        await asyncio.sleep(0)
        self.assertTrue(fake_task_manager.is_task_running())

    @setup.async_test
    async def test_save_triggers_pyre_restart__limit_reached(self) -> None:
        test_path = Path("/foo.py")
        fake_task_manager = BackgroundTaskManager(WaitForeverBackgroundTask())
        fake_task_manager2 = BackgroundTaskManager(WaitForeverBackgroundTask())
        server = PyreServer(
            input_channel=create_memory_text_reader(""),
            output_channel=create_memory_text_writer(),
            state=ServerState(
                opened_documents={test_path},
                consecutive_start_failure=CONSECUTIVE_START_ATTEMPT_THRESHOLD,
            ),
            pyre_manager=fake_task_manager,
            pyre_query_manager=fake_task_manager2,
        )
        self.assertFalse(fake_task_manager.is_task_running())

        await server.process_did_save_request(
            lsp.DidSaveTextDocumentParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                )
            )
        )
        await asyncio.sleep(0)
        self.assertFalse(fake_task_manager.is_task_running())

    @setup.async_test
    async def test_save_adds_path_to_queue(self) -> None:
        test_path = Path("/root/test.py")
        fake_task_manager = BackgroundTaskManager(WaitForeverBackgroundTask())
        fake_task_manager2 = BackgroundTaskManager(WaitForeverBackgroundTask())
        server = PyreServer(
            input_channel=create_memory_text_reader(""),
            output_channel=create_memory_text_writer(),
            state=ServerState(opened_documents={test_path}),
            pyre_manager=fake_task_manager,
            pyre_query_manager=fake_task_manager2,
        )

        # Save should add path to query even if the server is already running.
        await fake_task_manager.ensure_task_running()

        await server.process_did_save_request(
            lsp.DidSaveTextDocumentParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                )
            )
        )
        await asyncio.sleep(0)
        self.assertEqual(server.state.query_state.paths_to_be_queried.qsize(), 1)
        self.assertEqual(
            server.state.query_state.paths_to_be_queried.get_nowait(), test_path
        )

    @setup.async_test
    async def test_close_clears_hover_data(self) -> None:
        test_path = Path("/root/test.py")
        fake_task_manager = BackgroundTaskManager(WaitForeverBackgroundTask())
        fake_task_manager2 = BackgroundTaskManager(WaitForeverBackgroundTask())
        server = PyreServer(
            input_channel=create_memory_text_reader(""),
            output_channel=create_memory_text_writer(),
            state=ServerState(
                opened_documents={test_path},
                query_state=PyreQueryState(
                    path_to_location_type_lookup={
                        test_path: LocationTypeLookup(
                            [
                                (lsp.Position(4, 4), lsp.Position(4, 15), "str"),
                            ]
                        ),
                    }
                ),
            ),
            pyre_manager=fake_task_manager,
            pyre_query_manager=fake_task_manager2,
        )

        await server.process_close_request(
            lsp.DidCloseTextDocumentParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(test_path).unparse()
                )
            )
        )
        await asyncio.sleep(0)
        self.assertEqual(server.state.query_state.path_to_location_type_lookup, {})

    @setup.async_test
    async def test_hover_always_responds(self) -> None:
        def assert_hover_response(expected_hover_contents: str) -> None:
            expected_response = json_rpc.SuccessResponse(
                id=42,
                # pyre-ignore[16]: Pyre does not understand
                # `dataclasses_json`.
                result=lsp.HoverResponse(contents=expected_hover_contents).to_dict(),
            )
            response_string = json.dumps(expected_response.json())
            client_messages = memory_bytes_writer.items()
            self.assertEqual(
                client_messages[-1].decode(),
                f"Content-Length: {len(response_string)}\r\n\r\n" + response_string,
            )

        test_path = Path("/foo.py")
        not_tracked_path = Path("/not_tracked.py")
        fake_task_manager = BackgroundTaskManager(WaitForeverBackgroundTask())
        fake_task_manager2 = BackgroundTaskManager(WaitForeverBackgroundTask())
        memory_bytes_writer: MemoryBytesWriter = MemoryBytesWriter()
        server = PyreServer(
            input_channel=create_memory_text_reader(""),
            output_channel=TextWriter(memory_bytes_writer),
            state=ServerState(
                opened_documents={test_path},
                query_state=PyreQueryState(
                    path_to_location_type_lookup={
                        test_path: LocationTypeLookup(
                            [
                                (lsp.Position(4, 4), lsp.Position(4, 15), "str"),
                            ]
                        ),
                    }
                ),
            ),
            pyre_manager=fake_task_manager,
            pyre_query_manager=fake_task_manager2,
        )

        await fake_task_manager.ensure_task_running()

        await server.process_hover_request(
            lsp.HoverTextDocumentParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                ),
                position=lsp.LspPosition(line=3, character=4),
            ),
            request_id=42,
        )
        await asyncio.sleep(0)

        assert_hover_response("```str```")
        self.assertTrue(fake_task_manager.is_task_running())
        self.assertEqual(server.state.query_state.paths_to_be_queried.qsize(), 1)
        self.assertEqual(
            server.state.query_state.paths_to_be_queried.get_nowait(), test_path
        )

        await server.process_hover_request(
            lsp.HoverTextDocumentParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(not_tracked_path).unparse(),
                ),
                position=lsp.LspPosition(line=3, character=4),
            ),
            request_id=42,
        )
        await asyncio.sleep(0)

        self.assertTrue(fake_task_manager.is_task_running())
        assert_hover_response("")
        self.assertEqual(server.state.query_state.paths_to_be_queried.qsize(), 0)

    def test_type_diagnostics(self) -> None:
        self.assertEqual(
            type_error_to_diagnostic(
                error.Error(
                    line=1,
                    column=1,
                    stop_line=2,
                    stop_column=2,
                    path=Path("/derp.py"),
                    code=42,
                    name="name",
                    description="description",
                )
            ),
            lsp.Diagnostic(
                range=lsp.Range(
                    start=lsp.Position(line=0, character=1),
                    end=lsp.Position(line=1, character=2),
                ),
                message="description",
                severity=lsp.DiagnosticSeverity.ERROR,
                code=None,
                source="Pyre",
            ),
        )
        self.assertDictEqual(
            type_errors_to_diagnostics(
                [
                    error.Error(
                        line=1,
                        column=1,
                        stop_line=2,
                        stop_column=2,
                        path=Path("/foo.py"),
                        code=42,
                        name="foo_name",
                        description="foo_description",
                    ),
                    error.Error(
                        line=2,
                        column=2,
                        stop_line=3,
                        stop_column=3,
                        path=Path("/foo.py"),
                        code=43,
                        name="foo_name2",
                        description="foo_description2",
                    ),
                    error.Error(
                        line=4,
                        column=4,
                        stop_line=5,
                        stop_column=5,
                        path=Path("/bar.py"),
                        code=44,
                        name="bar_name",
                        description="bar_description",
                    ),
                ]
            ),
            {
                Path("/foo.py"): [
                    lsp.Diagnostic(
                        range=lsp.Range(
                            start=lsp.Position(line=0, character=1),
                            end=lsp.Position(line=1, character=2),
                        ),
                        message="foo_description",
                        severity=lsp.DiagnosticSeverity.ERROR,
                        code=None,
                        source="Pyre",
                    ),
                    lsp.Diagnostic(
                        range=lsp.Range(
                            start=lsp.Position(line=1, character=2),
                            end=lsp.Position(line=2, character=3),
                        ),
                        message="foo_description2",
                        severity=lsp.DiagnosticSeverity.ERROR,
                        code=None,
                        source="Pyre",
                    ),
                ],
                Path("/bar.py"): [
                    lsp.Diagnostic(
                        range=lsp.Range(
                            start=lsp.Position(line=3, character=4),
                            end=lsp.Position(line=4, character=5),
                        ),
                        message="bar_description",
                        severity=lsp.DiagnosticSeverity.ERROR,
                        code=None,
                        source="Pyre",
                    )
                ],
            },
        )

    def test_coverage_diagnostics(self) -> None:
        self.assertEqual(
            uncovered_range_to_diagnostic(
                CodeRange(
                    CodePosition(line=1, column=1), CodePosition(line=2, column=2)
                )
            ),
            lsp.Diagnostic(
                range=lsp.Range(
                    start=lsp.Position(line=0, character=1),
                    end=lsp.Position(line=1, character=2),
                ),
                message=(
                    "This function is not type checked. "
                    "Consider adding parameter or return type annotations."
                ),
            ),
        )

    def test_coverage_percentage(self) -> None:
        coverage_result = to_coverage_result(
            CoveredAndUncoveredLines({1, 2}, {3, 4}),
            uncovered_ranges=[
                CodeRange(
                    CodePosition(line=3, column=3), CodePosition(line=4, column=4)
                )
            ],
        )
        self.assertEqual(coverage_result.covered_percent, 50.0)
        self.assertEqual(len(coverage_result.uncovered_ranges), 1)

    @setup.async_test
    async def test_server_connection_lost(self) -> None:
        test_path = Path("/foo.py")
        server_state = ServerState(
            diagnostics={test_path: []},
        )

        bytes_writer = MemoryBytesWriter()
        server_handler = PyreServerHandler(
            server_start_options_reader=_create_server_start_options_reader(
                binary="/bin/pyre",
                server_identifier="foo",
                start_arguments=start.Arguments(
                    base_arguments=backend_arguments.BaseArguments(
                        source_paths=backend_arguments.SimpleSourcePath(),
                        log_path="/log/path",
                        global_root="/global/root",
                    )
                ),
                ide_features=configuration_module.IdeFeatures(),
            ),
            client_output_channel=TextWriter(bytes_writer),
            server_state=server_state,
        )

        with self.assertRaises(asyncio.IncompleteReadError):
            await server_handler.subscribe_to_type_error(
                # Intentionally inject a broken server response
                create_memory_text_reader("derp"),
                create_memory_text_writer(),
            )
        client_visible_messages = bytes_writer.items()
        self.assertTrue(len(client_visible_messages) > 0)
        self.assertEqual(
            await lsp.read_json_rpc(
                TextReader(
                    MemoryBytesReader(
                        # The server may send several status updates but we are
                        # mostly interested in the last message, which contains
                        # the diagnostics.
                        client_visible_messages[-1]
                    )
                )
            ),
            json_rpc.Request(
                method="textDocument/publishDiagnostics",
                parameters=json_rpc.ByNameParameters(
                    {"uri": "file:///foo.py", "diagnostics": []}
                ),
            ),
        )
        self.assertEqual(server_state.diagnostics, {})

    @setup.async_test
    async def test_send_message_to_status_bar(self) -> None:
        bytes_writer = MemoryBytesWriter()
        server_handler = PyreServerHandler(
            server_start_options_reader=_create_server_start_options_reader(
                binary="/bin/pyre",
                server_identifier="foo",
                start_arguments=start.Arguments(
                    base_arguments=backend_arguments.BaseArguments(
                        source_paths=backend_arguments.SimpleSourcePath(),
                        log_path="/log/path",
                        global_root="/global/root",
                    )
                ),
                ide_features=configuration_module.IdeFeatures(),
            ),
            client_output_channel=TextWriter(bytes_writer),
            server_state=ServerState(
                client_capabilities=lsp.ClientCapabilities(
                    window=lsp.WindowClientCapabilities(
                        status=lsp.ShowStatusRequestClientCapabilities(),
                    ),
                )
            ),
        )
        await server_handler.show_status_message_to_client(
            message="derp", level=lsp.MessageType.WARNING
        )

        client_visible_messages = bytes_writer.items()
        self.assertTrue(len(client_visible_messages) > 0)
        self.assertEqual(
            await lsp.read_json_rpc(
                TextReader(MemoryBytesReader(client_visible_messages[-1]))
            ),
            json_rpc.Request(
                method="window/showStatus",
                id=0,
                parameters=json_rpc.ByNameParameters({"type": 2, "message": "derp"}),
            ),
        )

    @setup.async_test
    async def test_type_coverage_request(self) -> None:
        with tempfile.NamedTemporaryFile(suffix=".py") as tmpfile:
            tmpfile.write(b"def foo(x):\n  pass\n")
            tmpfile.flush()
            test_path = Path(tmpfile.name)
            bytes_writer = MemoryBytesWriter()
            fake_pyre_manager = BackgroundTaskManager(WaitForeverBackgroundTask())
            fake_pyre_query_manager = BackgroundTaskManager(WaitForeverBackgroundTask())
            server = PyreServer(
                input_channel=create_memory_text_reader(""),
                output_channel=TextWriter(bytes_writer),
                state=ServerState(),
                pyre_manager=fake_pyre_manager,
                pyre_query_manager=fake_pyre_query_manager,
            )

            await server.process_type_coverage_request(
                lsp.TypeCoverageTextDocumentParameters(
                    text_document=lsp.TextDocumentIdentifier(
                        uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                    )
                ),
                request_id=1,
            )
            # A diagnostic update is sent via the output channel
            self.assertEqual(len(bytes_writer.items()), 1)
            self.assertIn(b"Consider adding type annotations", bytes_writer.items()[0])
            self.assertNotIn(b"100.0", bytes_writer.items()[0])

    @setup.async_test
    async def test_type_coverage_request_strict(self) -> None:
        with tempfile.NamedTemporaryFile(suffix=".py") as tmpfile:
            tmpfile.write(b"def foo(x):\n  pass\n")
            tmpfile.flush()
            test_path = Path(tmpfile.name)
            bytes_writer = MemoryBytesWriter()
            fake_pyre_manager = BackgroundTaskManager(WaitForeverBackgroundTask())
            fake_pyre_query_manager = BackgroundTaskManager(WaitForeverBackgroundTask())
            server = PyreServer(
                input_channel=create_memory_text_reader(""),
                output_channel=TextWriter(bytes_writer),
                state=ServerState(strict_default=True),
                pyre_manager=fake_pyre_manager,
                pyre_query_manager=fake_pyre_query_manager,
            )

            await server.process_type_coverage_request(
                lsp.TypeCoverageTextDocumentParameters(
                    text_document=lsp.TextDocumentIdentifier(
                        uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                    )
                ),
                request_id=1,
            )
            # A diagnostic update is sent via the output channel, with 100% coverage.
            self.assertEqual(len(bytes_writer.items()), 1)
            self.assertIn(b"100.0", bytes_writer.items()[0])

    @setup.async_test
    async def test_type_coverage_request_exclude(self) -> None:
        with tempfile.NamedTemporaryFile(prefix="exclude_me", suffix=".py") as tmpfile:
            tmpfile.write(b"def foo(x):\n  pass\n")
            tmpfile.flush()
            test_path = Path(tmpfile.name)
            bytes_writer = MemoryBytesWriter()
            fake_pyre_manager = BackgroundTaskManager(WaitForeverBackgroundTask())
            fake_pyre_query_manager = BackgroundTaskManager(WaitForeverBackgroundTask())
            server = PyreServer(
                input_channel=create_memory_text_reader(""),
                output_channel=TextWriter(bytes_writer),
                state=ServerState(excludes=[".*exclude_me.*"]),
                pyre_manager=fake_pyre_manager,
                pyre_query_manager=fake_pyre_query_manager,
            )

            await server.process_type_coverage_request(
                lsp.TypeCoverageTextDocumentParameters(
                    text_document=lsp.TextDocumentIdentifier(
                        uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                    )
                ),
                request_id=1,
            )
            # A diagnostic update is sent via the output channel, with 100% coverage.
            self.assertEqual(len(bytes_writer.items()), 1)
            self.assertIn(b"file is not type checked", bytes_writer.items()[0])


class PyreQueryStateTest(testslide.TestCase):
    def test_hover_response_for_position(self) -> None:
        pyre_query_state = PyreQueryState(
            {
                Path("test.py"): LocationTypeLookup(
                    [
                        (lsp.Position(4, 4), lsp.Position(4, 15), "str"),
                        (lsp.Position(8, 16), lsp.Position(8, 17), "int"),
                    ]
                ),
            }
        )

        self.assertEqual(
            pyre_query_state.hover_response_for_position(
                Path("test.py"), lsp.LspPosition(3, 5)
            ),
            lsp.HoverResponse(contents="```str```"),
        )
        self.assertEqual(
            pyre_query_state.hover_response_for_position(
                Path("test.py"), lsp.LspPosition(7, 16)
            ),
            lsp.HoverResponse(contents="```int```"),
        )
        self.assertEqual(
            pyre_query_state.hover_response_for_position(
                Path("test.py"), lsp.LspPosition(99, 99)
            ),
            lsp.HoverResponse(contents=""),
        )
        self.assertEqual(
            pyre_query_state.hover_response_for_position(
                Path("non_existent.py"), lsp.LspPosition(3, 4)
            ),
            lsp.HoverResponse(contents=""),
        )


class PyreQueryHandlerTest(testslide.TestCase):
    @setup.async_test
    async def test_query_types(self) -> None:
        json_output = """
        {
            "response": [
                {
                    "path": "test.py",
                    "types": [
                        {
                            "location": {
                                "start": {"line": 4, "column": 4},
                                "stop": {"line": 4, "column": 15}
                            },
                            "annotation": "str"
                        },
                        {
                            "location": {
                                "start": {"line": 8, "column": 16},
                                "stop": {"line": 8, "column": 17}
                            },
                            "annotation": "int"
                        }
                    ]
                },
                {
                    "path": "test2.py",
                    "types": [
                        {
                            "location": {
                                "start": {"line": 5, "column": 4},
                                "stop": {"line": 5, "column": 15}
                            },
                            "annotation": "str"
                        }
                    ]
                }
            ]
        }
        """
        pyre_query_manager = PyreQueryHandler(
            state=PyreQueryState(path_to_location_type_lookup={}),
            server_start_options_reader=_create_server_start_options_reader(
                binary="/bin/pyre",
                server_identifier="foo",
                start_arguments=start.Arguments(
                    base_arguments=backend_arguments.BaseArguments(
                        source_paths=backend_arguments.SimpleSourcePath(),
                        log_path="/log/path",
                        global_root="/global/root",
                    )
                ),
                ide_features=configuration_module.IdeFeatures(hover_enabled=True),
            ),
        )
        memory_bytes_writer = MemoryBytesWriter()
        flat_json = "".join(json_output.splitlines())
        input_channel = create_memory_text_reader(f'["Query", {flat_json}]\n')
        output_channel = TextWriter(memory_bytes_writer)

        result = await pyre_query_manager._query_types(
            [Path("test.py")], input_channel, output_channel
        )

        self.assertEqual(
            result,
            {
                Path("test.py"): LocationTypeLookup(
                    [
                        (lsp.Position(4, 4), lsp.Position(4, 15), "str"),
                        (lsp.Position(8, 16), lsp.Position(8, 17), "int"),
                    ]
                ),
                Path("test2.py"): LocationTypeLookup(
                    [(lsp.Position(5, 4), lsp.Position(5, 15), "str")]
                ),
            },
        )
        self.assertEqual(
            memory_bytes_writer.items(), [b'["Query", "types(\'test.py\')"]\n']
        )

    @setup.async_test
    async def test_query_types__bad_json(self) -> None:
        pyre_query_manager = PyreQueryHandler(
            state=PyreQueryState(),
            server_start_options_reader=_create_server_start_options_reader(
                binary="/bin/pyre",
                server_identifier="foo",
                start_arguments=start.Arguments(
                    base_arguments=backend_arguments.BaseArguments(
                        source_paths=backend_arguments.SimpleSourcePath(),
                        log_path="/log/path",
                        global_root="/global/root",
                    )
                ),
                ide_features=configuration_module.IdeFeatures(hover_enabled=True),
            ),
        )

        input_channel = create_memory_text_reader("""{ "error": "Oops" }\n""")
        memory_bytes_writer = MemoryBytesWriter()
        output_channel = TextWriter(memory_bytes_writer)
        result = await pyre_query_manager._query_types(
            [Path("test.py")], input_channel, output_channel
        )

        self.assertEqual(result, None)

    @setup.async_test
    async def test_does_not_run_when_hover_is_disabled(self) -> None:
        pyre_query_handler = PyreQueryHandler(
            state=PyreQueryState(),
            server_start_options_reader=_create_server_start_options_reader(
                binary="/bin/pyre",
                server_identifier="foo",
                start_arguments=start.Arguments(
                    base_arguments=backend_arguments.BaseArguments(
                        source_paths=backend_arguments.SimpleSourcePath(),
                        log_path="/log/path",
                        global_root="/global/root",
                    )
                ),
                ide_features=configuration_module.IdeFeatures(hover_enabled=False),
            ),
        )

        # Should complete right away.
        await pyre_query_handler.run()
        await asyncio.sleep(0)

        pyre_query_handler = PyreQueryHandler(
            state=PyreQueryState(),
            server_start_options_reader=_create_server_start_options_reader(
                binary="/bin/pyre",
                server_identifier="foo",
                start_arguments=start.Arguments(
                    base_arguments=backend_arguments.BaseArguments(
                        source_paths=backend_arguments.SimpleSourcePath(),
                        log_path="/log/path",
                        global_root="/global/root",
                    )
                ),
                ide_features=None,
            ),
        )

        # Should complete right away.
        await pyre_query_handler.run()
        await asyncio.sleep(0)

        pyre_query_handler = PyreQueryHandler(
            state=PyreQueryState(),
            server_start_options_reader=_create_server_start_options_reader(
                binary="/bin/pyre",
                server_identifier="foo",
                start_arguments=start.Arguments(
                    base_arguments=backend_arguments.BaseArguments(
                        source_paths=backend_arguments.SimpleSourcePath(),
                        log_path="/log/path",
                        global_root="/global/root",
                    )
                ),
                ide_features=configuration_module.IdeFeatures(hover_enabled=True),
            ),
        )
        pyre_query_manager = BackgroundTaskManager(pyre_query_handler)
        await pyre_query_manager.ensure_task_running()
        await asyncio.sleep(0)

        self.assertTrue(pyre_query_manager.is_task_running())
        await pyre_query_manager.ensure_task_stop()
