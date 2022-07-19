# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio
import json
import sys
import tempfile
import textwrap
from contextlib import contextmanager
from pathlib import Path
from typing import Iterable, Iterator, Optional, Sequence
from unittest.mock import CallableMixin, patch

import testslide
from libcst.metadata import CodePosition, CodeRange

from ... import configuration as configuration_module, error, json_rpc
from ...commands.language_server_protocol import SymbolKind
from ...coverage_collector import CoveredAndUncoveredLines
from ...tests import setup
from .. import (
    async_server_connection,
    backend_arguments,
    language_server_protocol as lsp,
    start,
    subscription,
)
from ..async_server_connection import (
    BackgroundTask,
    BackgroundTaskManager,
    create_memory_text_reader,
    create_memory_text_writer,
    MemoryBytesReader,
    MemoryBytesWriter,
    TextReader,
    TextWriter,
)
from ..persistent import (
    CONSECUTIVE_START_ATTEMPT_THRESHOLD,
    DefinitionLocationQuery,
    InitializationExit,
    InitializationFailure,
    InitializationSuccess,
    LocationTypeLookup,
    path_to_coverage_response,
    PyreQueryHandler,
    PyreQueryState,
    PyreServer,
    PyreServerHandler,
    PyreServerShutdown,
    PyreServerStartOptions,
    PyreServerStartOptionsReader,
    ReferencesQuery,
    ServerState,
    to_coverage_result,
    try_initialize,
    type_error_to_diagnostic,
    type_errors_to_diagnostics,
    TypeCoverageQuery,
    TypesQuery,
    uncovered_range_to_diagnostic,
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


def _fake_option_reader() -> PyreServerStartOptionsReader:
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


class PersistentTest(testslide.TestCase):
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
            input_channel, output_channel, _fake_option_reader()
        )
        self.assertIsInstance(result, InitializationSuccess)

    @setup.async_test
    async def test_try_initialize_failure__not_a_request(self) -> None:
        input_channel = await _create_input_channel_with_requests(
            [json_rpc.Request(method="derp", parameters=None)]
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(
            input_channel, output_channel, _fake_option_reader()
        )
        self.assertIsInstance(result, InitializationFailure)

    @setup.async_test
    async def test_try_initialize_failure__invalid_parameters(self) -> None:
        input_channel = await _create_input_channel_with_requests(
            [json_rpc.Request(id=0, method="initialize", parameters=None)]
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(
            input_channel, output_channel, _fake_option_reader()
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
            input_channel, output_channel, _fake_option_reader()
        )
        self.assertIsInstance(result, InitializationFailure)

    @setup.async_test
    async def test_try_initialize_exit(self) -> None:
        input_channel = await _create_input_channel_with_requests(
            [json_rpc.Request(method="exit", parameters=None)]
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(
            input_channel, output_channel, _fake_option_reader()
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
            input_channel, output_channel, _fake_option_reader()
        )
        self.assertIsInstance(result, InitializationExit)

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
            server.state.query_state.queries.qsize(),
            1,
        )
        self.assertEqual(
            server.state.query_state.queries.get_nowait(),
            TypesQuery(test_path0),
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
            server.state.query_state.queries.qsize(),
            1,
        )
        self.assertEqual(
            server.state.query_state.queries.get_nowait(),
            TypesQuery(test_path1),
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
            server.state.query_state.queries.qsize(),
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
            subscription.StatusUpdate(kind="Rebuilding")
        )
        await server_handler.handle_status_update_subscription(
            subscription.StatusUpdate(kind="Rechecking")
        )
        await server_handler.handle_type_error_subscription(
            subscription.TypeErrors(
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
    async def test_subscription_error(self) -> None:
        def fake_server_start_options_reader() -> PyreServerStartOptions:
            # Server start option is not relevant to this test
            raise NotImplementedError

        server_handler = PyreServerHandler(
            server_start_options_reader=fake_server_start_options_reader,
            client_output_channel=TextWriter(MemoryBytesWriter()),
            server_state=ServerState(),
        )
        with self.assertRaises(PyreServerShutdown):
            await server_handler.handle_error_subscription(
                subscription.Error(message="Doom Eternal")
            )

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
            subscription.StatusUpdate(kind="Rebuilding")
        )
        await server_handler.handle_status_update_subscription(
            subscription.StatusUpdate(kind="Rechecking")
        )

        client_messages = [x.decode("utf-8") for x in bytes_writer.items()]
        self.assertTrue(len(client_messages) >= 2)
        # Clear out diagnostics for rebuilding status
        self.assertIn('"diagnostics": []', client_messages[0])
        self.assertIn('"diagnostics": []', client_messages[1])

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
        self.assertEqual(server.state.query_state.queries.qsize(), 1)
        self.assertEqual(
            server.state.query_state.queries.get_nowait(), TypesQuery(test_path)
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
        self.assertEqual(server.state.query_state.queries.qsize(), 1)
        self.assertEqual(
            server.state.query_state.queries.get_nowait(), TypesQuery(test_path)
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
        self.assertEqual(server.state.query_state.queries.qsize(), 0)

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
        test_path = Path("/foo")
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

        self.assertEqual(
            server.state.query_state.queries.get_nowait(),
            TypeCoverageQuery(1, test_path),
        )

    @setup.async_test
    async def test_definition(self) -> None:
        def assert_definition_response(
            definitions: Sequence[lsp.LspDefinitionResponse],
        ) -> None:
            client_messages = memory_bytes_writer.items()
            expected_response = json_rpc.SuccessResponse(
                id=42,
                result=lsp.LspDefinitionResponse.cached_schema().dump(
                    definitions, many=True
                ),
            )
            response_string = json.dumps(expected_response.json())
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
            ),
            pyre_manager=fake_task_manager,
            pyre_query_manager=fake_task_manager2,
        )

        await fake_task_manager.ensure_task_running()

        await server.process_definition_request(
            lsp.DefinitionTextDocumentParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                ),
                position=lsp.LspPosition(line=3, character=4),
            ),
            request_id=42,
        )
        await asyncio.sleep(0)

        self.assertTrue(fake_task_manager.is_task_running())
        self.assertEqual(len(memory_bytes_writer.items()), 0)
        self.assertEqual(server.state.query_state.queries.qsize(), 1)
        self.assertEqual(
            server.state.query_state.queries.get_nowait(),
            DefinitionLocationQuery(
                id=42,
                path=test_path,
                position=lsp.LspPosition(line=3, character=4).to_pyre_position(),
            ),
        )

        await server.process_definition_request(
            lsp.DefinitionTextDocumentParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(not_tracked_path).unparse(),
                ),
                position=lsp.LspPosition(line=3, character=4),
            ),
            request_id=42,
        )
        await asyncio.sleep(0)

        self.assertTrue(fake_task_manager.is_task_running())
        assert_definition_response([])

    @setup.async_test
    async def test_document_symbols_request(self) -> None:
        self.maxDiff = None
        with tempfile.NamedTemporaryFile(suffix=".py") as temporary_file:
            temporary_file.write(b"def foo(x):\n  pass\n")
            temporary_file.flush()
            test_path = Path(temporary_file.name)
            fake_task_manager = BackgroundTaskManager(WaitForeverBackgroundTask())
            fake_task_manager2 = BackgroundTaskManager(WaitForeverBackgroundTask())
            memory_bytes_writer: MemoryBytesWriter = MemoryBytesWriter()
            server = PyreServer(
                input_channel=create_memory_text_reader(""),
                output_channel=TextWriter(memory_bytes_writer),
                state=ServerState(
                    opened_documents={test_path},
                ),
                pyre_manager=fake_task_manager,
                pyre_query_manager=fake_task_manager2,
            )
            await fake_task_manager.ensure_task_running()
            await server.process_document_symbols_request(
                lsp.DocumentSymbolsTextDocumentParameters(
                    text_document=lsp.TextDocumentIdentifier(uri=test_path.as_uri())
                ),
                request_id=42,
            )
            await asyncio.sleep(0)
            self.assertTrue(fake_task_manager.is_task_running())

            client_messages = memory_bytes_writer.items()
            if (sys.version_info.major, sys.version_info.minor) >= (3, 8):
                end_line, end_character = 1, 6
            else:
                end_line, end_character = 0, 3
            expected_response = json_rpc.SuccessResponse(
                id=42,
                result=[
                    {
                        "detail": "",
                        "name": "foo",
                        "range": {
                            "start": {"line": 0, "character": 0},
                            "end": {"line": end_line, "character": end_character},
                        },
                        "kind": SymbolKind.FUNCTION.value,
                        "selectionRange": {
                            "start": {"line": 0, "character": 0},
                            "end": {"line": end_line, "character": end_character},
                        },
                        "children": [],
                    }
                ],
            )
            response_string = json.dumps(expected_response.json())
            actual = client_messages[-1].decode()
            self.assertRegex(
                actual, f"Content-Length: {len(response_string)}\r\n\r\n.*"
            )
            actual_json = json.loads(actual[actual.index("{") :])
            self.assertDictEqual(actual_json, expected_response.json())

    @setup.async_test
    async def test_references(self) -> None:
        def assert_references_response(
            references: Sequence[lsp.ReferencesResponse],
        ) -> None:
            client_messages = memory_bytes_writer.items()
            expected_response = json_rpc.SuccessResponse(
                id=42,
                result=lsp.ReferencesResponse.cached_schema().dump(
                    references, many=True
                ),
            )
            response_string = json.dumps(expected_response.json())
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
            ),
            pyre_manager=fake_task_manager,
            pyre_query_manager=fake_task_manager2,
        )

        await fake_task_manager.ensure_task_running()

        await server.process_find_all_references_request(
            lsp.ReferencesTextDocumentParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                ),
                position=lsp.LspPosition(line=3, character=4),
            ),
            request_id=42,
        )
        await asyncio.sleep(0)

        self.assertTrue(fake_task_manager.is_task_running())
        self.assertEqual(len(memory_bytes_writer.items()), 0)
        self.assertEqual(server.state.query_state.queries.qsize(), 1)
        self.assertEqual(
            server.state.query_state.queries.get_nowait(),
            ReferencesQuery(
                id=42,
                path=test_path,
                position=lsp.LspPosition(line=3, character=4).to_pyre_position(),
            ),
        )

        await server.process_find_all_references_request(
            lsp.ReferencesTextDocumentParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(not_tracked_path).unparse(),
                ),
                position=lsp.LspPosition(line=3, character=4),
            ),
            request_id=42,
        )
        await asyncio.sleep(0)

        self.assertTrue(fake_task_manager.is_task_running())
        assert_references_response([])

    def test_path_to_coverage_response(self) -> None:
        with tempfile.NamedTemporaryFile(suffix=".py") as temporary_file:
            path = Path(temporary_file.name)
            source = """
            def uncovered(x):
                print(x)
            """
            path.write_text(textwrap.dedent(source))
            self.assertEqual(
                lsp.TypeCoverageResponse(
                    covered_percent=50.0,
                    uncovered_ranges=[
                        lsp.Diagnostic(
                            range=lsp.Range(
                                start=lsp.Position(line=1, character=0),
                                end=lsp.Position(line=2, character=12),
                            ),
                            message="This function is not type checked. Consider adding parameter or return type annotations.",
                            severity=None,
                            code=None,
                            source=None,
                        )
                    ],
                    default_message="Consider adding type annotations.",
                ),
                path_to_coverage_response(path, strict_default=False),
            )

    def test_path_to_coverage_response__invalid_syntax(self) -> None:
        with tempfile.NamedTemporaryFile(suffix=".py") as temporary_file:
            path = Path(temporary_file.name)
            source = """
            def invalid_syntax
            """
            path.write_text(textwrap.dedent(source))
            self.assertIsNone(
                path_to_coverage_response(path, strict_default=False),
            )


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


@contextmanager
def patch_connect_in_text_mode(
    input_channel: TextReader, output_channel: TextWriter
) -> Iterator[CallableMixin]:
    with patch.object(async_server_connection, "connect_in_text_mode") as mock:

        class MockedConnection:
            async def __aenter__(self):
                return (
                    input_channel,
                    output_channel,
                )

            async def __aexit__(self, exc_type, exc, tb):
                pass

        mock.return_value = MockedConnection()
        yield mock


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
        bytes_writer = MemoryBytesWriter()
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
            client_output_channel=TextWriter(bytes_writer),
        )
        memory_bytes_writer = MemoryBytesWriter()
        flat_json = "".join(json_output.splitlines())
        input_channel = create_memory_text_reader(f'["Query", {flat_json}]\n')
        output_channel = TextWriter(memory_bytes_writer)

        with patch_connect_in_text_mode(input_channel, output_channel):
            result = await pyre_query_manager._query_types(
                [Path("test.py")], Path("fake_socket_path")
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
        bytes_writer = MemoryBytesWriter()
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
            client_output_channel=TextWriter(bytes_writer),
        )

        input_channel = create_memory_text_reader("""{ "error": "Oops" }\n""")
        memory_bytes_writer = MemoryBytesWriter()
        output_channel = TextWriter(memory_bytes_writer)
        with patch_connect_in_text_mode(input_channel, output_channel):
            result = await pyre_query_manager._query_types(
                [Path("test.py")], Path("fake_socket_path")
            )

        self.assertEqual(result, None)

    @setup.async_test
    async def test_query_type_coverage(self) -> None:
        with tempfile.NamedTemporaryFile(suffix=".py") as tmpfile:
            tmpfile.write(b"def foo(x):\n  pass\n")
            tmpfile.flush()
            test_path = Path(tmpfile.name)
            bytes_writer = MemoryBytesWriter()
            server_start_options_reader = _fake_option_reader()
            pyre_query_manager = PyreQueryHandler(
                state=PyreQueryState(),
                server_start_options_reader=server_start_options_reader,
                client_output_channel=TextWriter(bytes_writer),
            )
            strict = False
            input_channel = create_memory_text_reader(
                '["Query", {"response": ["test"]}]\n'
            )
            memory_bytes_writer = MemoryBytesWriter()
            output_channel = TextWriter(memory_bytes_writer)
            with patch_connect_in_text_mode(input_channel, output_channel):
                result = await pyre_query_manager._query_type_coverage(
                    path=test_path,
                    strict_default=strict,
                    socket_path=Path("fake_socket_path"),
                    expression_level_coverage_enabled=False,
                )
            self.assertEqual(len(memory_bytes_writer.items()), 1)
            self.assertTrue(
                memory_bytes_writer.items()[0].startswith(
                    b'["Query", "modules_of_path('
                )
            )
            self.assertTrue(result is not None)
            self.assertEqual(len(result.uncovered_ranges), 1)
            self.assertTrue(result.covered_percent < 100.0)

    @setup.async_test
    async def test_query_type_coverage__bad_json(self) -> None:
        pyre_query_manager = PyreQueryHandler(
            state=PyreQueryState(),
            server_start_options_reader=_fake_option_reader(),
            client_output_channel=TextWriter(MemoryBytesWriter()),
        )
        input_channel = create_memory_text_reader('{ "error": "Oops" }\n')
        output_channel = TextWriter(MemoryBytesWriter())
        with patch_connect_in_text_mode(input_channel, output_channel):
            result = await pyre_query_manager._query_type_coverage(
                path=Path("test.py"),
                strict_default=False,
                socket_path=Path("fake_socket_path"),
                expression_level_coverage_enabled=False,
            )
        self.assertTrue(result is None)

    @setup.async_test
    async def test_query_type_coverage__strict(self) -> None:
        with tempfile.NamedTemporaryFile(suffix=".py") as tmpfile:
            tmpfile.write(b"def foo(x):\n  pass\n")
            tmpfile.flush()
            test_path = Path(tmpfile.name)
            pyre_query_manager = PyreQueryHandler(
                state=PyreQueryState(),
                server_start_options_reader=_fake_option_reader(),
                client_output_channel=TextWriter(MemoryBytesWriter()),
            )
            strict = True
            input_channel = create_memory_text_reader(
                '["Query", {"response": ["test"]}]\n'
            )
            output_channel = TextWriter(MemoryBytesWriter())
            with patch_connect_in_text_mode(input_channel, output_channel):
                result = await pyre_query_manager._query_type_coverage(
                    path=test_path,
                    strict_default=strict,
                    socket_path=Path("fake_socket_path"),
                    expression_level_coverage_enabled=False,
                )
            self.assertTrue(result is not None)
            self.assertEqual(len(result.uncovered_ranges), 0)
            self.assertEqual(result.covered_percent, 100.0)

    @setup.async_test
    async def test_query_type_coverage__not_typechecked(self) -> None:
        pyre_query_manager = PyreQueryHandler(
            state=PyreQueryState(),
            server_start_options_reader=_fake_option_reader(),
            client_output_channel=TextWriter(MemoryBytesWriter()),
        )
        input_channel = create_memory_text_reader('["Query", {"response": []}]\n')
        output_channel = TextWriter(MemoryBytesWriter())
        with patch_connect_in_text_mode(input_channel, output_channel):
            result = await pyre_query_manager._query_type_coverage(
                path=Path("test.py"),
                strict_default=False,
                socket_path=Path("fake_socket_path"),
                expression_level_coverage_enabled=False,
            )
        self.assertTrue(result is not None)
        self.assertEqual(result.covered_percent, 0.0)
        self.assertEqual(len(result.uncovered_ranges), 1)
        self.assertEqual(
            result.uncovered_ranges[0].message, "This file is not type checked by Pyre."
        )

    @setup.async_test
    async def test_query_expression_coverage(self) -> None:
        with tempfile.NamedTemporaryFile(suffix=".py") as tmpfile:
            tmpfile.write(b"def foo(x):\n  pass\n")
            tmpfile.flush()
            test_path = Path(tmpfile.name)
            bytes_writer = MemoryBytesWriter()
            server_start_options_reader = _fake_option_reader()
            pyre_query_manager = PyreQueryHandler(
                state=PyreQueryState(),
                server_start_options_reader=server_start_options_reader,
                client_output_channel=TextWriter(bytes_writer),
            )
            strict = False
            input_channel = create_memory_text_reader(
                '["Query", {"response": ["test"]}]\n ["Query", {"response": [["CoverageAtPath",{"path":"/fake/path.py","total_expressions":1,"coverage_gaps":[]}]]}]\n'
            )
            memory_bytes_writer = MemoryBytesWriter()
            output_channel = TextWriter(memory_bytes_writer)
            with patch_connect_in_text_mode(input_channel, output_channel):
                result = await pyre_query_manager._query_type_coverage(
                    path=test_path,
                    strict_default=strict,
                    socket_path=Path("fake_socket_path"),
                    expression_level_coverage_enabled=True,
                )
            self.assertEqual(len(memory_bytes_writer.items()), 2)
            self.assertTrue(
                memory_bytes_writer.items()[0].startswith(
                    b'["Query", "modules_of_path('
                )
            )
            self.assertTrue(result is not None)
            self.assertEqual(len(result.uncovered_ranges), 0)
            self.assertTrue(result.covered_percent == 100.0)

    @setup.async_test
    async def test_query_expression_coverage_gaps(self) -> None:
        with tempfile.NamedTemporaryFile(suffix=".py") as tmpfile:
            tmpfile.write(b"def foo(x):\n  pass\n")
            tmpfile.flush()
            test_path = Path(tmpfile.name)
            bytes_writer = MemoryBytesWriter()
            server_start_options_reader = _fake_option_reader()
            pyre_query_manager = PyreQueryHandler(
                state=PyreQueryState(),
                server_start_options_reader=server_start_options_reader,
                client_output_channel=TextWriter(bytes_writer),
            )
            strict = False
            input_channel = create_memory_text_reader(
                '["Query", {"response": ["test"]}]\n ["Query", {"response": [["CoverageAtPath",{"path":"/fake/path.py","total_expressions":4,"coverage_gaps":[{"location": {"start": {"line": 11, "column": 16}, "stop": {"line": 11, "column": 17}}, "type_": "typing.Any", "reason": ["TypeIsAny"]}]}]]}]\n'
            )
            memory_bytes_writer = MemoryBytesWriter()
            output_channel = TextWriter(memory_bytes_writer)
            with patch_connect_in_text_mode(input_channel, output_channel):
                result = await pyre_query_manager._query_type_coverage(
                    path=test_path,
                    strict_default=strict,
                    socket_path=Path("fake_socket_path"),
                    expression_level_coverage_enabled=True,
                )
            self.assertEqual(len(memory_bytes_writer.items()), 2)
            self.assertTrue(
                memory_bytes_writer.items()[0].startswith(
                    b'["Query", "modules_of_path('
                )
            )
            self.assertTrue(result is not None)
            self.assertEqual(len(result.uncovered_ranges), 1)
            self.assertTrue(result.covered_percent == 75.0)

    @setup.async_test
    async def test_query_expression_coverage__bad_json(self) -> None:
        pyre_query_manager = PyreQueryHandler(
            state=PyreQueryState(),
            server_start_options_reader=_fake_option_reader(),
            client_output_channel=TextWriter(MemoryBytesWriter()),
        )
        input_channel = create_memory_text_reader(
            '{ "error": "Oops" }\n["Query", {"response": [["ErrorAtPath",{"path":"/fake/path.py","error":"oops"}]]}]\n'
        )
        output_channel = TextWriter(MemoryBytesWriter())
        with patch_connect_in_text_mode(input_channel, output_channel):
            result = await pyre_query_manager._query_type_coverage(
                path=Path("test.py"),
                strict_default=False,
                socket_path=Path("fake_socket_path"),
                expression_level_coverage_enabled=True,
            )
        self.assertTrue(result is None)

    @setup.async_test
    async def test_query_expression_coverage__strict(self) -> None:
        with tempfile.NamedTemporaryFile(suffix=".py") as tmpfile:
            tmpfile.write(b"def foo(x):\n  pass\n")
            tmpfile.flush()
            test_path = Path(tmpfile.name)
            pyre_query_manager = PyreQueryHandler(
                state=PyreQueryState(),
                server_start_options_reader=_fake_option_reader(),
                client_output_channel=TextWriter(MemoryBytesWriter()),
            )
            strict = True
            input_channel = create_memory_text_reader(
                '["Query", {"response": ["test"]}]\n["Query", {"response": [["CoverageAtPath",{"path":"/fake/path.py","total_expressions":0,"coverage_gaps":[]}]]}]\n'
            )
            output_channel = TextWriter(MemoryBytesWriter())
            with patch_connect_in_text_mode(input_channel, output_channel):
                result = await pyre_query_manager._query_type_coverage(
                    path=test_path,
                    strict_default=strict,
                    socket_path=Path("fake_socket_path"),
                    expression_level_coverage_enabled=True,
                )
            self.assertTrue(result is not None)
            self.assertEqual(len(result.uncovered_ranges), 0)
            self.assertEqual(result.covered_percent, 100.0)

    @setup.async_test
    async def test_query_expression_coverage__not_typechecked(self) -> None:
        pyre_query_manager = PyreQueryHandler(
            state=PyreQueryState(),
            server_start_options_reader=_fake_option_reader(),
            client_output_channel=TextWriter(MemoryBytesWriter()),
        )
        input_channel = create_memory_text_reader(
            '["Query", {"response": []}]\n["Query", {"response": [["CoverageAtPath",{"path":"/fake/test.py","total_expressions":0,"coverage_gaps":[]}]]}]\n'
        )
        output_channel = TextWriter(MemoryBytesWriter())
        with patch_connect_in_text_mode(input_channel, output_channel):
            result = await pyre_query_manager._query_type_coverage(
                path=Path("test.py"),
                strict_default=False,
                socket_path=Path("fake_socket_path"),
                expression_level_coverage_enabled=True,
            )
        self.assertTrue(result is not None)
        self.assertEqual(result.covered_percent, 100.0)
        self.assertEqual(len(result.uncovered_ranges), 0)

    @setup.async_test
    async def test_query_definition_location(self) -> None:
        json_output = """
        {
            "response": [
                {
                    "path": "/foo.py",
                    "range": {
                        "start": {
                            "line": 9,
                            "character": 6
                        },
                        "end": {
                            "line": 10,
                            "character": 11
                        }
                    }
                }
            ]
        }
        """
        client_output_writer = MemoryBytesWriter()
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
            client_output_channel=TextWriter(client_output_writer),
        )
        memory_bytes_writer = MemoryBytesWriter()
        flat_json = "".join(json_output.splitlines())
        input_channel = create_memory_text_reader(f'["Query", {flat_json}]\n')
        output_channel = TextWriter(memory_bytes_writer)

        with patch_connect_in_text_mode(input_channel, output_channel):
            await pyre_query_manager._query_and_send_definition_location(
                query=DefinitionLocationQuery(
                    id=99,
                    path=Path("bar.py"),
                    position=lsp.Position(line=42, character=10),
                ),
                socket_path=Path("fake_socket_path"),
                enabled_telemetry_event=False,
            )

        self.assertEqual(
            memory_bytes_writer.items(),
            [
                b'["Query", "location_of_definition(path=\'bar.py\','
                b' line=42, column=10)"]\n'
            ],
        )
        self.assertEqual(len(client_output_writer.items()), 1)
        response = client_output_writer.items()[0].splitlines()[2]
        result = json.loads(response)["result"]
        self.assertEqual(
            lsp.LspDefinitionResponse.cached_schema().load(result, many=True),
            [
                lsp.LspDefinitionResponse(
                    uri="/foo.py",
                    range=lsp.LspRange(
                        start=lsp.LspPosition(line=8, character=6),
                        end=lsp.LspPosition(line=9, character=11),
                    ),
                )
            ],
        )

    @setup.async_test
    async def test_query_definition_location__bad_json(self) -> None:
        client_output_writer = MemoryBytesWriter()
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
            client_output_channel=TextWriter(client_output_writer),
        )

        input_channel = create_memory_text_reader("""{ "error": "Oops" }\n""")
        memory_bytes_writer = MemoryBytesWriter()
        output_channel = TextWriter(memory_bytes_writer)
        with patch_connect_in_text_mode(input_channel, output_channel):
            await pyre_query_manager._query_and_send_definition_location(
                query=DefinitionLocationQuery(
                    id=99,
                    path=Path("bar.py"),
                    position=lsp.Position(line=42, character=10),
                ),
                socket_path=Path("fake_socket_path"),
                enabled_telemetry_event=False,
            )

        self.assertEqual(len(client_output_writer.items()), 1)
        response = client_output_writer.items()[0].splitlines()[2]
        self.assertEqual(json.loads(response)["result"], [])

    @setup.async_test
    async def test_query_references(self) -> None:
        json_output = """
        {
            "response": [
                {
                    "path": "/foo.py",
                    "range": {
                        "start": {
                            "line": 9,
                            "character": 6
                        },
                        "end": {
                            "line": 10,
                            "character": 11
                        }
                    }
                },
                {
                    "path": "/bar.py",
                    "range": {
                        "start": {
                            "line": 2,
                            "character": 3
                        },
                        "end": {
                            "line": 2,
                            "character": 4
                        }
                    }
                }
            ]
        }
        """
        client_output_writer = MemoryBytesWriter()
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
                ide_features=configuration_module.IdeFeatures(
                    find_all_references_enabled=True
                ),
            ),
            client_output_channel=TextWriter(client_output_writer),
        )
        memory_bytes_writer = MemoryBytesWriter()
        flat_json = "".join(json_output.splitlines())
        input_channel = create_memory_text_reader(f'["Query", {flat_json}]\n')
        output_channel = TextWriter(memory_bytes_writer)

        with patch_connect_in_text_mode(input_channel, output_channel):
            await pyre_query_manager._handle_find_all_references_query(
                query=ReferencesQuery(
                    id=99,
                    path=Path("bar.py"),
                    position=lsp.Position(line=42, character=10),
                ),
                socket_path=Path("fake_socket_path"),
            )

        self.assertEqual(
            memory_bytes_writer.items(),
            [
                b'["Query", "find_references(path=\'bar.py\','
                b' line=42, column=10)"]\n'
            ],
        )
        self.assertEqual(len(client_output_writer.items()), 1)
        response = client_output_writer.items()[0].splitlines()[2]
        result = json.loads(response)["result"]
        self.assertEqual(
            lsp.LspDefinitionResponse.cached_schema().load(result, many=True),
            [
                lsp.LspDefinitionResponse(
                    uri="/foo.py",
                    range=lsp.LspRange(
                        start=lsp.LspPosition(line=8, character=6),
                        end=lsp.LspPosition(line=9, character=11),
                    ),
                ),
                lsp.LspDefinitionResponse(
                    uri="/bar.py",
                    range=lsp.LspRange(
                        start=lsp.LspPosition(line=1, character=3),
                        end=lsp.LspPosition(line=1, character=4),
                    ),
                ),
            ],
        )

    @setup.async_test
    async def test_query_references__bad_json(self) -> None:
        client_output_writer = MemoryBytesWriter()
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
                ide_features=configuration_module.IdeFeatures(
                    find_all_references_enabled=True
                ),
            ),
            client_output_channel=TextWriter(client_output_writer),
        )

        input_channel = create_memory_text_reader("""{ "error": "Oops" }\n""")
        memory_bytes_writer = MemoryBytesWriter()
        output_channel = TextWriter(memory_bytes_writer)
        with patch_connect_in_text_mode(input_channel, output_channel):
            await pyre_query_manager._handle_find_all_references_query(
                query=ReferencesQuery(
                    id=99,
                    path=Path("bar.py"),
                    position=lsp.Position(line=42, character=10),
                ),
                socket_path=Path("fake_socket_path"),
            )

        self.assertEqual(len(client_output_writer.items()), 1)
        response = client_output_writer.items()[0].splitlines()[2]
        self.assertEqual(json.loads(response)["result"], [])
