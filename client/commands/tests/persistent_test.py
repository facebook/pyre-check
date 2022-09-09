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
from typing import Callable, Iterable, Iterator, List, Optional, Sequence, Set, Tuple
from unittest.mock import CallableMixin, patch

import testslide
from libcst.metadata import CodePosition, CodeRange

from ... import error, json_rpc
from ...commands.language_server_protocol import SymbolKind
from ...coverage_collector import CoveredAndUncoveredLines
from ...tests import setup
from .. import (
    backend_arguments,
    background,
    connections,
    language_server_protocol as lsp,
    start,
    subscription,
)
from ..connections import (
    AsyncTextReader,
    AsyncTextWriter,
    create_memory_text_reader,
    create_memory_text_writer,
    MemoryBytesReader,
    MemoryBytesWriter,
)
from ..persistent import (
    AbstractRequestHandler,
    CONSECUTIVE_START_ATTEMPT_THRESHOLD,
    DefinitionAvailability,
    HoverAvailability,
    InitializationExit,
    InitializationFailure,
    InitializationSuccess,
    LanguageServerFeatures,
    path_to_coverage_response,
    PyreDaemonLaunchAndSubscribeHandler,
    PyreDaemonShutdown,
    PyreServer,
    PyreServerOptions,
    PyreServerOptionsReader,
    read_lsp_request,
    ReferencesAvailability,
    RequestHandler,
    ServerState,
    to_coverage_result,
    try_initialize,
    type_error_to_diagnostic,
    type_errors_to_diagnostics,
    TypeCoverageAvailability,
    TypeErrorsAvailability,
    uncovered_range_to_diagnostic,
)
from .language_server_protocol_test import ExceptionRaisingBytesWriter


DEFAULT_BINARY = "/bin/pyre"
DEFAULT_SERVER_IDENTIFIER = "server_identifier"
DEFAULT_START_ARGUMENTS: start.Arguments = start.Arguments(
    base_arguments=backend_arguments.BaseArguments(
        source_paths=backend_arguments.SimpleSourcePath(),
        log_path="/log/path",
        global_root="/global/root",
    ),
    socket_path=Path("irrelevant_socket_path.sock"),
)
DEFAULT_FEATURES: LanguageServerFeatures = LanguageServerFeatures(
    type_coverage=TypeCoverageAvailability.FUNCTION_LEVEL
)
DEFAULT_IS_STRICT = False
DEFAULT_EXCLUDES: Optional[Sequence[str]] = None
DEFAULT_ENABLE_TELEMETRY: bool = False


def _create_server_options(
    binary: str = DEFAULT_BINARY,
    server_identifier: str = DEFAULT_SERVER_IDENTIFIER,
    start_arguments: start.Arguments = DEFAULT_START_ARGUMENTS,
    language_server_features: LanguageServerFeatures = DEFAULT_FEATURES,
    strict_default: bool = DEFAULT_IS_STRICT,
    excludes: Optional[Sequence[str]] = DEFAULT_EXCLUDES,
    enabled_telemetry_event: bool = DEFAULT_ENABLE_TELEMETRY,
) -> PyreServerOptions:
    return PyreServerOptions(
        binary,
        server_identifier,
        start_arguments,
        language_server_features,
        strict_default,
        excludes if excludes else [],
        enabled_telemetry_event,
    )


def _create_server_options_reader(
    binary: str = DEFAULT_BINARY,
    server_identifier: str = DEFAULT_SERVER_IDENTIFIER,
    start_arguments: start.Arguments = DEFAULT_START_ARGUMENTS,
    language_server_features: LanguageServerFeatures = DEFAULT_FEATURES,
    strict_default: bool = DEFAULT_IS_STRICT,
    excludes: Optional[Sequence[str]] = DEFAULT_EXCLUDES,
    enabled_telemetry_event: bool = DEFAULT_ENABLE_TELEMETRY,
) -> PyreServerOptionsReader:
    return lambda: _create_server_options(
        binary,
        server_identifier,
        start_arguments,
        language_server_features,
        strict_default,
        excludes,
        enabled_telemetry_event,
    )


def _create_server_state_with_options(
    binary: str = DEFAULT_BINARY,
    server_identifier: str = DEFAULT_SERVER_IDENTIFIER,
    start_arguments: start.Arguments = DEFAULT_START_ARGUMENTS,
    language_server_features: LanguageServerFeatures = DEFAULT_FEATURES,
    strict_default: bool = DEFAULT_IS_STRICT,
    excludes: Optional[Sequence[str]] = DEFAULT_EXCLUDES,
    enabled_telemetry_event: bool = DEFAULT_ENABLE_TELEMETRY,
) -> ServerState:
    return ServerState(
        _create_server_options(
            binary,
            server_identifier,
            start_arguments,
            language_server_features,
            strict_default,
            excludes,
            enabled_telemetry_event,
        )
    )


mock_server_options_reader: PyreServerOptionsReader = _create_server_options_reader()
mock_initial_server_options: PyreServerOptions = mock_server_options_reader()
mock_server_state: ServerState = ServerState(server_options=mock_initial_server_options)


class MockRequestHandler(AbstractRequestHandler):
    def __init__(
        self,
        mock_type_coverage: Optional[lsp.TypeCoverageResponse] = None,
        mock_hover_response: Optional[lsp.LspHoverResponse] = None,
        mock_definition_response: Optional[List[lsp.LspDefinitionResponse]] = None,
        mock_references_response: Optional[List[lsp.LspDefinitionResponse]] = None,
    ) -> None:
        self.requests: List[object] = []
        self.mock_type_coverage = mock_type_coverage
        self.mock_hover_response = mock_hover_response
        self.mock_definition_response = mock_definition_response
        self.mock_references_response = mock_references_response

    async def get_type_coverage(
        self,
        path: Path,
    ) -> Optional[lsp.TypeCoverageResponse]:
        self.requests.append({"path": path})
        return self.mock_type_coverage

    async def get_hover(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> lsp.LspHoverResponse:
        self.requests.append({"path": path, "position": position})
        if self.mock_hover_response is None:
            raise ValueError("You need to set hover response in the mock handler")
        else:
            return self.mock_hover_response

    async def get_definition_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> List[lsp.LspDefinitionResponse]:
        self.requests.append({"path": path, "position": position})
        if self.mock_definition_response is None:
            raise ValueError("You need to set hover response in the mock handler")
        else:
            return self.mock_definition_response

    async def get_reference_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> List[lsp.LspDefinitionResponse]:
        self.requests.append({"path": path, "position": position})
        if self.mock_references_response is None:
            raise ValueError("You need to set hover response in the mock handler")
        else:
            return self.mock_references_response

    async def update_overlay(
        self,
        path: Path,
        code: str,
    ) -> None:
        self.requests.append({"path": path, "code": code})


async def _create_server_for_request_test(
    opened_documents: Set[Path],
    handler: MockRequestHandler,
    server_options: PyreServerOptions = mock_initial_server_options,
) -> Tuple[PyreServer, MemoryBytesWriter]:
    # set up the system under test
    fake_task_manager = background.TaskManager(WaitForeverBackgroundTask())
    output_writer: MemoryBytesWriter = MemoryBytesWriter()
    server = PyreServer(
        input_channel=create_memory_text_reader(""),
        output_channel=AsyncTextWriter(output_writer),
        server_state=ServerState(
            server_options=server_options,
            opened_documents=opened_documents,
        ),
        pyre_manager=fake_task_manager,
        handler=handler,
    )
    await fake_task_manager.ensure_task_running()
    return server, output_writer


def _extract_json_from_json_rpc_message(
    raw_message: bytes,
) -> str:
    """
    Return the content-length of a json rpc message
    """
    content_length_portion, json_portion = raw_message.split(b"\r\n\r\n")
    CONTENT_LENGTH_PREFIX = b"Content-Length: "
    if not content_length_portion.startswith(CONTENT_LENGTH_PREFIX):
        raise ValueError(
            f"Did not get expected content length header, but {content_length_portion!r}"
        )
    content_length = int(content_length_portion[len(CONTENT_LENGTH_PREFIX) :])
    if not len(json_portion) == content_length:
        raise ValueError(
            f"Expected content length {content_length} to match length "
            f"{len(json_portion)} of json mssage {json_portion!r}"
        )
    return json_portion.decode()


async def _create_input_channel_with_requests(
    requests: Iterable[json_rpc.Request],
) -> AsyncTextReader:
    bytes_writer = MemoryBytesWriter()
    for request in requests:
        await lsp.write_json_rpc(AsyncTextWriter(bytes_writer), request)
    return AsyncTextReader(MemoryBytesReader(b"\n".join(bytes_writer.items())))


DEFAULT_REQUEST_ID: int = 42


def _success_response_json(
    result: object,
    request_id: int = DEFAULT_REQUEST_ID,
) -> str:
    return json.dumps(json_rpc.SuccessResponse(id=request_id, result=result).json())


class NoOpBackgroundTask(background.Task):
    async def run(self) -> None:
        pass


class WaitForeverBackgroundTask(background.Task):
    async def run(self) -> None:
        await asyncio.Event().wait()


class PersistentTest(testslide.TestCase):
    @setup.async_test
    async def test_read_lsp_request_success(self) -> None:
        expected_request = json_rpc.Request(
            id=0,
            method="derp",
        )
        input_channel = await _create_input_channel_with_requests([expected_request])
        bytes_writer = MemoryBytesWriter()
        output_channel = AsyncTextWriter(bytes_writer)
        actual_request = await read_lsp_request(input_channel, output_channel)
        self.assertEquals(actual_request, expected_request)
        self.assertEqual(len(bytes_writer.items()), 0)

    @setup.async_test
    async def test_read_lsp_request_fail_channel_closed(self) -> None:
        input_channel = create_memory_text_reader("")
        bytes_writer = MemoryBytesWriter()
        output_channel = AsyncTextWriter(bytes_writer)
        with self.assertRaises(lsp.ReadChannelClosedError):
            await read_lsp_request(input_channel, output_channel)
        self.assertEqual(len(bytes_writer.items()), 0)

    @setup.async_test
    async def test_read_lsp_request_fail_incomplete_read(self) -> None:
        input_channel = create_memory_text_reader("derp\n")
        bytes_writer = MemoryBytesWriter()
        output_channel = AsyncTextWriter(bytes_writer)
        with self.assertRaises(lsp.ReadChannelClosedError):
            await read_lsp_request(input_channel, output_channel)
        # One message for one failed read
        self.assertEqual(len(bytes_writer.items()), 1)

    @setup.async_test
    async def test_read_lsp_request_success_after_failed_reads(self) -> None:
        expected_request = json_rpc.Request(
            id=0,
            method="derp",
        )
        input_channel = create_memory_text_reader(
            f"foo\r\n\r\nbar\r\n\r\n{lsp.json_rpc_payload(expected_request)}"
        )
        bytes_writer = MemoryBytesWriter()
        output_channel = AsyncTextWriter(bytes_writer)
        actual_request = await read_lsp_request(input_channel, output_channel)
        self.assertEquals(actual_request, expected_request)
        # Two messages for two failed reads
        self.assertEqual(len(bytes_writer.items()), 2)

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
        bytes_writer = MemoryBytesWriter()
        result = await try_initialize(
            input_channel, AsyncTextWriter(bytes_writer), mock_initial_server_options
        )
        self.assertIsInstance(result, InitializationSuccess)
        self.assertEqual(len(bytes_writer.items()), 1)

    @setup.async_test
    async def test_try_initialize_failure__not_a_request(self) -> None:
        input_channel = await _create_input_channel_with_requests(
            [json_rpc.Request(method="derp", parameters=None)]
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(
            input_channel, output_channel, mock_initial_server_options
        )
        self.assertIsInstance(result, InitializationFailure)

    @setup.async_test
    async def test_try_initialize_failure__invalid_parameters(self) -> None:
        input_channel = await _create_input_channel_with_requests(
            [json_rpc.Request(id=0, method="initialize", parameters=None)]
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(
            input_channel, output_channel, mock_initial_server_options
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
            input_channel, output_channel, mock_initial_server_options
        )
        self.assertIsInstance(result, InitializationFailure)

    @setup.async_test
    async def test_try_initialize_exit(self) -> None:
        input_channel = await _create_input_channel_with_requests(
            [json_rpc.Request(method="exit", parameters=None)]
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(
            input_channel, output_channel, mock_initial_server_options
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
            input_channel, output_channel, mock_initial_server_options
        )
        self.assertIsInstance(result, InitializationExit)

    @setup.async_test
    async def test_try_initialize_exit__shutdown_without_exit(self) -> None:
        input_channel = await _create_input_channel_with_requests(
            [
                json_rpc.Request(
                    id=0,
                    method="initialize",
                    parameters=json_rpc.ByNameParameters(
                        {
                            "processId": 42,
                            "rootUri": None,
                            "capabilities": {},
                        }
                    ),
                ),
                json_rpc.Request(method="shutdown", parameters=None),
            ]
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(
            input_channel, output_channel, mock_initial_server_options
        )
        self.assertIsInstance(result, InitializationExit)

    @setup.async_test
    async def test_try_initialize_exit__without_anything(self) -> None:
        result = await try_initialize(
            create_memory_text_reader(""),
            create_memory_text_writer(),
            mock_initial_server_options,
        )
        self.assertIsInstance(result, InitializationExit)


class PyreDaemonLaunchAndSubscribeHandlerTest(testslide.TestCase):
    @setup.async_test
    async def test_subscription_protocol(self) -> None:
        server_state = ServerState(
            client_capabilities=lsp.ClientCapabilities(
                window=lsp.WindowClientCapabilities(
                    status=lsp.ShowStatusRequestClientCapabilities(),
                ),
            ),
            server_options=mock_initial_server_options,
        )
        bytes_writer = MemoryBytesWriter()

        def fake_server_options_reader() -> PyreServerOptions:
            # Server start option is not relevant to this test
            raise NotImplementedError()

        server_handler = PyreDaemonLaunchAndSubscribeHandler(
            server_options_reader=fake_server_options_reader,
            client_output_channel=AsyncTextWriter(bytes_writer),
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
    async def test_subscription_type_errors_disabled(self) -> None:
        server_state = ServerState(
            client_capabilities=lsp.ClientCapabilities(
                window=lsp.WindowClientCapabilities(
                    status=lsp.ShowStatusRequestClientCapabilities(),
                ),
            ),
            server_options=_create_server_options(
                language_server_features=LanguageServerFeatures(
                    type_errors=TypeErrorsAvailability.DISABLED,
                )
            ),
        )
        bytes_writer = MemoryBytesWriter()

        def fake_server_options_reader() -> PyreServerOptions:
            # Server start option is not relevant to this test
            raise NotImplementedError()

        server_handler = PyreDaemonLaunchAndSubscribeHandler(
            server_options_reader=fake_server_options_reader,
            client_output_channel=AsyncTextWriter(bytes_writer),
            server_state=server_state,
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
        # When the type errors feature is disabled, we should ignore errors
        self.assertEqual(len(client_messages), 0)

    @setup.async_test
    async def test_subscription_error(self) -> None:
        def fake_server_options_reader() -> PyreServerOptions:
            # Server start option is not relevant to this test
            raise NotImplementedError()

        server_handler = PyreDaemonLaunchAndSubscribeHandler(
            server_options_reader=fake_server_options_reader,
            client_output_channel=AsyncTextWriter(MemoryBytesWriter()),
            server_state=mock_server_state,
        )
        with self.assertRaises(PyreDaemonShutdown):
            await server_handler.handle_error_subscription(
                subscription.Error(message="Doom Eternal")
            )

    @setup.async_test
    async def test_busy_status_clear_diagnostics(self) -> None:
        path = Path("foo.py")
        server_state = ServerState(
            server_options=mock_initial_server_options, diagnostics={path: []}
        )
        bytes_writer = MemoryBytesWriter()

        def fake_server_options_reader() -> PyreServerOptions:
            # Server start option is not relevant to this test
            raise NotImplementedError()

        server_handler = PyreDaemonLaunchAndSubscribeHandler(
            server_options_reader=fake_server_options_reader,
            client_output_channel=AsyncTextWriter(bytes_writer),
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
        fake_task_manager = background.TaskManager(WaitForeverBackgroundTask())
        server = PyreServer(
            input_channel=create_memory_text_reader(""),
            output_channel=create_memory_text_writer(),
            server_state=mock_server_state,
            pyre_manager=fake_task_manager,
            handler=MockRequestHandler(),
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
        fake_task_manager = background.TaskManager(WaitForeverBackgroundTask())
        server = PyreServer(
            input_channel=create_memory_text_reader(""),
            output_channel=create_memory_text_writer(),
            server_state=ServerState(
                server_options=mock_initial_server_options,
                consecutive_start_failure=CONSECUTIVE_START_ATTEMPT_THRESHOLD,
            ),
            pyre_manager=fake_task_manager,
            handler=MockRequestHandler(),
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
        fake_task_manager = background.TaskManager(WaitForeverBackgroundTask())
        server = PyreServer(
            input_channel=create_memory_text_reader(""),
            output_channel=create_memory_text_writer(),
            server_state=ServerState(
                server_options=mock_initial_server_options, opened_documents={test_path}
            ),
            pyre_manager=fake_task_manager,
            handler=MockRequestHandler(),
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
        fake_task_manager = background.TaskManager(WaitForeverBackgroundTask())
        server = PyreServer(
            input_channel=create_memory_text_reader(""),
            output_channel=create_memory_text_writer(),
            server_state=ServerState(
                server_options=mock_initial_server_options,
                opened_documents={test_path},
                consecutive_start_failure=CONSECUTIVE_START_ATTEMPT_THRESHOLD,
            ),
            pyre_manager=fake_task_manager,
            handler=MockRequestHandler(),
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
        fake_task_manager = background.TaskManager(WaitForeverBackgroundTask())
        server = PyreServer(
            input_channel=create_memory_text_reader(""),
            output_channel=create_memory_text_writer(),
            server_state=ServerState(
                server_options=mock_initial_server_options, opened_documents={test_path}
            ),
            pyre_manager=fake_task_manager,
            handler=MockRequestHandler(),
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
                range=lsp.LspRange(
                    start=lsp.LspPosition(line=0, character=1),
                    end=lsp.LspPosition(line=1, character=2),
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
                        range=lsp.LspRange(
                            start=lsp.LspPosition(line=0, character=1),
                            end=lsp.LspPosition(line=1, character=2),
                        ),
                        message="foo_description",
                        severity=lsp.DiagnosticSeverity.ERROR,
                        code=None,
                        source="Pyre",
                    ),
                    lsp.Diagnostic(
                        range=lsp.LspRange(
                            start=lsp.LspPosition(line=1, character=2),
                            end=lsp.LspPosition(line=2, character=3),
                        ),
                        message="foo_description2",
                        severity=lsp.DiagnosticSeverity.ERROR,
                        code=None,
                        source="Pyre",
                    ),
                ],
                Path("/bar.py"): [
                    lsp.Diagnostic(
                        range=lsp.LspRange(
                            start=lsp.LspPosition(line=3, character=4),
                            end=lsp.LspPosition(line=4, character=5),
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
                range=lsp.LspRange(
                    start=lsp.LspPosition(line=0, character=1),
                    end=lsp.LspPosition(line=1, character=2),
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
    async def test_connections_lost(self) -> None:
        test_path = Path("/foo.py")
        server_state = ServerState(
            server_options=mock_initial_server_options,
            diagnostics={test_path: []},
        )

        bytes_writer = MemoryBytesWriter()
        server_handler = PyreDaemonLaunchAndSubscribeHandler(
            server_options_reader=_create_server_options_reader(
                binary="/bin/pyre",
                server_identifier="foo",
                start_arguments=start.Arguments(
                    base_arguments=backend_arguments.BaseArguments(
                        source_paths=backend_arguments.SimpleSourcePath(),
                        log_path="/log/path",
                        global_root="/global/root",
                    ),
                    socket_path=Path("irrelevant_socket_path.sock"),
                ),
            ),
            client_output_channel=AsyncTextWriter(bytes_writer),
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
                AsyncTextReader(
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
        server_handler = PyreDaemonLaunchAndSubscribeHandler(
            server_options_reader=_create_server_options_reader(
                binary="/bin/pyre",
                server_identifier="foo",
                start_arguments=start.Arguments(
                    base_arguments=backend_arguments.BaseArguments(
                        source_paths=backend_arguments.SimpleSourcePath(),
                        log_path="/log/path",
                        global_root="/global/root",
                    ),
                    socket_path=Path("irrelevant_socket_path.sock"),
                ),
            ),
            client_output_channel=AsyncTextWriter(bytes_writer),
            server_state=ServerState(
                client_capabilities=lsp.ClientCapabilities(
                    window=lsp.WindowClientCapabilities(
                        status=lsp.ShowStatusRequestClientCapabilities(),
                    ),
                ),
                server_options=mock_initial_server_options,
            ),
        )
        await server_handler.show_status_message_to_client(
            message="derp", level=lsp.MessageType.WARNING
        )

        client_visible_messages = bytes_writer.items()
        self.assertTrue(len(client_visible_messages) > 0)
        self.assertEqual(
            await lsp.read_json_rpc(
                AsyncTextReader(MemoryBytesReader(client_visible_messages[-1]))
            ),
            json_rpc.Request(
                method="window/showStatus",
                id=0,
                parameters=json_rpc.ByNameParameters({"type": 2, "message": "derp"}),
            ),
        )


class PyreServerTest(testslide.TestCase):
    @setup.async_test
    async def test_exit(self) -> None:
        server_state = mock_server_state
        noop_task_manager = background.TaskManager(NoOpBackgroundTask())
        input_channel = await _create_input_channel_with_requests(
            [
                json_rpc.Request(method="shutdown", parameters=None),
                json_rpc.Request(method="exit", parameters=None),
            ]
        )
        server = PyreServer(
            input_channel=input_channel,
            output_channel=create_memory_text_writer(),
            server_state=server_state,
            pyre_manager=noop_task_manager,
            handler=MockRequestHandler(),
        )

        exit_code = await server.run()
        self.assertEqual(exit_code, 0)

    @setup.async_test
    async def test_exit_unknown_request_after_shutdown(self) -> None:
        server_state = mock_server_state
        noop_task_manager = background.TaskManager(NoOpBackgroundTask())
        input_channel = await _create_input_channel_with_requests(
            [
                json_rpc.Request(method="shutdown", parameters=None),
                json_rpc.Request(method="derp", parameters=None),
                json_rpc.Request(method="exit", parameters=None),
            ]
        )
        server = PyreServer(
            input_channel=input_channel,
            output_channel=create_memory_text_writer(),
            server_state=server_state,
            pyre_manager=noop_task_manager,
            handler=MockRequestHandler(),
        )

        exit_code = await server.run()
        self.assertEqual(exit_code, 0)

    @setup.async_test
    async def test_exit_gracefully_after_shutdown(self) -> None:
        server_state = mock_server_state
        noop_task_manager = background.TaskManager(NoOpBackgroundTask())
        input_channel = await _create_input_channel_with_requests(
            [
                json_rpc.Request(method="shutdown", parameters=None),
            ]
        )
        server = PyreServer(
            # Feed only a shutdown request to input channel
            input_channel=input_channel,
            # Always rasing in the output channel
            output_channel=AsyncTextWriter(
                ExceptionRaisingBytesWriter(ConnectionResetError())
            ),
            server_state=server_state,
            pyre_manager=noop_task_manager,
            handler=MockRequestHandler(),
        )

        exit_code = await server.run()
        self.assertEqual(exit_code, 0)

    @setup.async_test
    async def test_exit_gracefully_on_channel_closure(self) -> None:
        server_state = mock_server_state
        noop_task_manager = background.TaskManager(NoOpBackgroundTask())
        server = PyreServer(
            # Feed nothing to input channel
            input_channel=create_memory_text_reader(""),
            # Always rasing in the output channel
            output_channel=AsyncTextWriter(
                ExceptionRaisingBytesWriter(ConnectionResetError())
            ),
            server_state=server_state,
            pyre_manager=noop_task_manager,
            handler=MockRequestHandler(),
        )

        exit_code = await server.run()
        self.assertEqual(exit_code, 0)

    @setup.async_test
    async def test_open_close(self) -> None:
        server_state = mock_server_state
        server = PyreServer(
            input_channel=create_memory_text_reader(""),
            output_channel=create_memory_text_writer(),
            server_state=server_state,
            pyre_manager=background.TaskManager(NoOpBackgroundTask()),
            handler=MockRequestHandler(),
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

        await server.process_close_request(
            lsp.DidCloseTextDocumentParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(test_path0).unparse()
                )
            )
        )
        self.assertNotIn(test_path0, server_state.opened_documents)

    @setup.async_test
    async def test_type_coverage_request(self) -> None:
        test_path = Path("/foo")
        output_writer = MemoryBytesWriter()
        fake_pyre_manager = background.TaskManager(WaitForeverBackgroundTask())
        handler = MockRequestHandler(
            mock_type_coverage=lsp.TypeCoverageResponse(
                covered_percent=42.42,
                uncovered_ranges=[],
                default_message="pyre is on fire",
            )
        )
        server = PyreServer(
            input_channel=create_memory_text_reader(""),
            output_channel=AsyncTextWriter(output_writer),
            server_state=mock_server_state,
            pyre_manager=fake_pyre_manager,
            handler=handler,
        )
        await server.process_type_coverage_request(
            lsp.TypeCoverageParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                )
            ),
            request_id=1,
        )
        self.assertEqual(
            handler.requests,
            [{"path": test_path}],
        )
        self.assertEqual(
            output_writer.items(),
            [
                b'Content-Length: 124\r\n\r\n{"jsonrpc": "2.0", "id": 1, "result": {"coveredPe'
                b'rcent": 42.42, "uncoveredRanges": [], "defaultMessage": "pyre is on fire"}}'
            ],
        )

    @setup.async_test
    async def test_type_coverage_request__None_response(self) -> None:
        test_path = Path("/foo")
        output_writer = MemoryBytesWriter()
        fake_pyre_manager = background.TaskManager(WaitForeverBackgroundTask())
        handler = MockRequestHandler(mock_type_coverage=None)
        server = PyreServer(
            input_channel=create_memory_text_reader(""),
            output_channel=AsyncTextWriter(output_writer),
            server_state=mock_server_state,
            pyre_manager=fake_pyre_manager,
            handler=handler,
        )
        await server.process_type_coverage_request(
            lsp.TypeCoverageParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                )
            ),
            request_id=1,
        )
        self.assertEqual(
            handler.requests,
            [{"path": test_path}],
        )
        self.assertEqual(output_writer.items(), [])

    def _assert_json_equal(
        self,
        actual_json_string: str,
        expected_json_string: str,
    ) -> None:
        self.assertEqual(
            json.loads(actual_json_string),
            json.loads(expected_json_string),
        )

    def _expect_success_message(
        self,
        result: object,
        request_id: int = DEFAULT_REQUEST_ID,
    ) -> Callable[[str], None]:
        return lambda actual_json_string: self._assert_json_equal(
            actual_json_string,
            _success_response_json(
                result=result,
                request_id=request_id,
            ),
        )

    def _expect_telemetry_event(
        self,
        operation: str,
        result: object,
    ) -> Callable[[str], None]:
        def expectation(actual_json_string: str) -> None:
            actual_telemetry = json.loads(actual_json_string)
            self.assertEqual(actual_telemetry["method"], "telemetry/event")
            telemetry_params = actual_telemetry["params"]
            self.assertEqual(telemetry_params["operation"], operation)
            self.assertEqual(telemetry_params["response"], result)

        return expectation

    def _assert_output_messages(
        self,
        output_writer: MemoryBytesWriter,
        expectations: List[Callable[[str], None]],
    ) -> None:
        self.assertEqual(
            len(output_writer.items()),
            len(expectations),
        )
        for raw_message, expectation in zip(output_writer.items(), expectations):
            json_string = _extract_json_from_json_rpc_message(raw_message)
            expectation(json_string)

    @setup.async_test
    async def test_hover__basic(self) -> None:
        tracked_path = Path("/tracked.py")
        lsp_line = 3
        daemon_line = lsp_line + 1
        expected_response = lsp.LspHoverResponse(contents="```foo.Foo```")
        for enabled_telemetry_event in (True, False):
            handler = MockRequestHandler(
                mock_hover_response=expected_response,
            )
            server, output_writer = await _create_server_for_request_test(
                opened_documents={tracked_path},
                handler=handler,
                server_options=_create_server_options(
                    enabled_telemetry_event=enabled_telemetry_event
                ),
            )
            await server.process_hover_request(
                parameters=lsp.HoverParameters(
                    text_document=lsp.TextDocumentIdentifier(
                        uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                    ),
                    position=lsp.LspPosition(line=lsp_line, character=4),
                ),
                request_id=DEFAULT_REQUEST_ID,
            )
            self.assertEqual(
                handler.requests,
                [
                    {
                        "path": tracked_path,
                        "position": lsp.PyrePosition(line=daemon_line, character=4),
                    }
                ],
            )
            raw_expected_response = lsp.LspHoverResponse.cached_schema().dump(
                expected_response
            )
            expect_correct_response = self._expect_success_message(
                raw_expected_response
            )
            if enabled_telemetry_event:
                expectations = [
                    expect_correct_response,
                    self._expect_telemetry_event(
                        operation="hover",
                        result=raw_expected_response,
                    ),
                ]
            else:
                expectations = [expect_correct_response]
            self._assert_output_messages(
                output_writer,
                expectations,
            )

    @setup.async_test
    async def test_hover__unopened(self) -> None:
        tracked_path = Path("/tracked.py")
        untracked_path = Path("/not_tracked.py")
        lsp_line = 3
        handler = MockRequestHandler()
        server, output_writer = await _create_server_for_request_test(
            opened_documents={tracked_path},
            handler=handler,
        )
        await server.process_hover_request(
            parameters=lsp.HoverParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(untracked_path).unparse(),
                ),
                position=lsp.LspPosition(line=lsp_line, character=4),
            ),
            request_id=DEFAULT_REQUEST_ID,
        )
        self.assertEqual(handler.requests, [])
        self._assert_output_messages(
            output_writer,
            [
                self._expect_success_message(
                    result=lsp.LspHoverResponse.cached_schema().dump(
                        lsp.LspHoverResponse.empty()
                    ),
                )
            ],
        )

    @setup.async_test
    async def test_definition__basic(self) -> None:
        tracked_path = Path("/tracked.py")
        lsp_line = 3
        daemon_line = lsp_line + 1
        expected_response = [
            lsp.LspDefinitionResponse(
                uri="file:///path/to/foo.py",
                range=lsp.LspRange(
                    start=lsp.LspPosition(line=5, character=6),
                    end=lsp.LspPosition(line=5, character=9),
                ),
            )
        ]
        for enabled_telemetry_event in (True, False):
            handler = MockRequestHandler(
                mock_definition_response=expected_response,
            )
            server, output_writer = await _create_server_for_request_test(
                opened_documents={tracked_path},
                handler=handler,
                server_options=_create_server_options(
                    enabled_telemetry_event=enabled_telemetry_event
                ),
            )
            await server.process_definition_request(
                parameters=lsp.DefinitionParameters(
                    text_document=lsp.TextDocumentIdentifier(
                        uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                    ),
                    position=lsp.LspPosition(line=lsp_line, character=4),
                ),
                request_id=DEFAULT_REQUEST_ID,
            )
            self.assertEqual(
                handler.requests,
                [
                    {
                        "path": tracked_path,
                        "position": lsp.PyrePosition(line=daemon_line, character=4),
                    }
                ],
            )
            raw_expected_response = lsp.LspDefinitionResponse.cached_schema().dump(
                expected_response, many=True
            )
            expect_correct_response = self._expect_success_message(
                raw_expected_response
            )
            if enabled_telemetry_event:
                expectations = [
                    expect_correct_response,
                    self._expect_telemetry_event(
                        operation="definition",
                        result=raw_expected_response,
                    ),
                ]
            else:
                expectations = [expect_correct_response]
            self._assert_output_messages(
                output_writer,
                expectations,
            )

    @setup.async_test
    async def test_definition__shadow(self) -> None:
        tracked_path = Path("/tracked.py")
        lsp_line = 3
        daemon_line = lsp_line + 1
        expected_editor_response = []
        expected_telemetry_response = [
            lsp.LspDefinitionResponse(
                uri="file:///path/to/foo.py",
                range=lsp.LspRange(
                    start=lsp.LspPosition(line=5, character=6),
                    end=lsp.LspPosition(line=5, character=9),
                ),
            )
        ]
        handler = MockRequestHandler(
            mock_definition_response=expected_telemetry_response,
        )
        server, output_writer = await _create_server_for_request_test(
            opened_documents={tracked_path},
            handler=handler,
            server_options=_create_server_options(
                enabled_telemetry_event=True,
                language_server_features=LanguageServerFeatures(
                    definition=DefinitionAvailability.SHADOW
                ),
            ),
        )
        await server.process_definition_request(
            parameters=lsp.DefinitionParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                ),
                position=lsp.LspPosition(line=lsp_line, character=4),
            ),
            request_id=DEFAULT_REQUEST_ID,
        )
        self.assertEqual(
            handler.requests,
            [
                {
                    "path": tracked_path,
                    "position": lsp.PyrePosition(line=daemon_line, character=4),
                }
            ],
        )
        self._assert_output_messages(
            output_writer,
            [
                self._expect_success_message(
                    lsp.LspDefinitionResponse.cached_schema().dump(
                        expected_editor_response, many=True
                    )
                ),
                self._expect_telemetry_event(
                    operation="definition",
                    result=lsp.LspDefinitionResponse.cached_schema().dump(
                        expected_telemetry_response, many=True
                    ),
                ),
            ],
        )

    async def test_definition__unopened(self) -> None:
        tracked_path = Path("/tracked.py")
        untracked_path = Path("/not_tracked.py")
        lsp_line = 3
        handler = MockRequestHandler()
        server, output_writer = await _create_server_for_request_test(
            opened_documents={tracked_path},
            handler=handler,
        )
        await server.process_definition_request(
            parameters=lsp.DefinitionParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(untracked_path).unparse(),
                ),
                position=lsp.LspPosition(line=lsp_line, character=4),
            ),
            request_id=DEFAULT_REQUEST_ID,
        )
        self.assertEqual(handler.requests, [])
        self._assert_output_messages(
            output_writer,
            [self._expect_success_message([])],
        )

    @setup.async_test
    async def test_references__basic(self) -> None:
        tracked_path = Path("/tracked.py")
        lsp_line = 3
        daemon_line = lsp_line + 1
        expected_response = [
            lsp.LspDefinitionResponse(
                uri="file:///path/to/foo.py",
                range=lsp.LspRange(
                    start=lsp.LspPosition(line=5, character=6),
                    end=lsp.LspPosition(line=5, character=9),
                ),
            )
        ]
        handler = MockRequestHandler(
            mock_references_response=expected_response,
        )
        server, output_writer = await _create_server_for_request_test(
            opened_documents={tracked_path},
            handler=handler,
        )
        await server.process_find_all_references_request(
            lsp.ReferencesParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                ),
                position=lsp.LspPosition(line=lsp_line, character=4),
            ),
            request_id=DEFAULT_REQUEST_ID,
        )
        self.assertEqual(
            handler.requests,
            [
                {
                    "path": tracked_path,
                    "position": lsp.PyrePosition(line=daemon_line, character=4),
                }
            ],
        )
        self._assert_output_messages(
            output_writer,
            [
                self._expect_success_message(
                    result=lsp.LspDefinitionResponse.cached_schema().dump(
                        expected_response, many=True
                    ),
                )
            ],
        )

    @setup.async_test
    async def test_references__unopened(self) -> None:
        tracked_path = Path("/tracked.py")
        untracked_path = Path("/not_tracked.py")
        lsp_line = 3
        handler = MockRequestHandler()
        server, output_writer = await _create_server_for_request_test(
            opened_documents={tracked_path},
            handler=handler,
        )
        await server.process_find_all_references_request(
            lsp.ReferencesParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(untracked_path).unparse(),
                ),
                position=lsp.LspPosition(line=lsp_line, character=4),
            ),
            request_id=DEFAULT_REQUEST_ID,
        )
        self.assertEqual(handler.requests, [])
        self._assert_output_messages(
            output_writer, [self._expect_success_message(result=[])]
        )

    @setup.async_test
    async def test_document_symbols_request(self) -> None:
        self.maxDiff = None
        with tempfile.NamedTemporaryFile(suffix=".py") as temporary_file:
            temporary_file.write(b"def foo(x):\n  pass\n")
            temporary_file.flush()
            test_path = Path(temporary_file.name)
            server, output_writer = await _create_server_for_request_test(
                opened_documents={test_path},
                handler=MockRequestHandler(),
            )
            await server.process_document_symbols_request(
                lsp.DocumentSymbolsParameters(
                    text_document=lsp.TextDocumentIdentifier(uri=test_path.as_uri())
                ),
                request_id=DEFAULT_REQUEST_ID,
            )
            if (sys.version_info.major, sys.version_info.minor) >= (3, 8):
                end_line, end_character = 1, 6
            else:
                end_line, end_character = 0, 3
            self._assert_output_messages(
                output_writer,
                [
                    self._expect_success_message(
                        result=[
                            {
                                "detail": "",
                                "name": "foo",
                                "range": {
                                    "start": {"line": 0, "character": 0},
                                    "end": {
                                        "line": end_line,
                                        "character": end_character,
                                    },
                                },
                                "kind": SymbolKind.FUNCTION.value,
                                "selectionRange": {
                                    "start": {"line": 0, "character": 0},
                                    "end": {
                                        "line": end_line,
                                        "character": end_character,
                                    },
                                },
                                "children": [],
                            }
                        ],
                    )
                ],
            )

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
                            range=lsp.LspRange(
                                start=lsp.LspPosition(line=1, character=0),
                                end=lsp.LspPosition(line=2, character=12),
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


@contextmanager
def patch_connect_async(
    input_channel: AsyncTextReader, output_channel: AsyncTextWriter
) -> Iterator[CallableMixin]:
    with patch.object(connections, "connect_async") as mock:

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


class RequestHandlerTest(testslide.TestCase):
    @setup.async_test
    async def test_get_type_coverage__basic(self) -> None:
        with tempfile.NamedTemporaryFile(suffix=".py") as tmpfile:
            tmpfile.write(b"def foo(x):\n  pass\n")
            tmpfile.flush()
            test_path = Path(tmpfile.name)
            pyre_query_manager = RequestHandler(
                server_state=_create_server_state_with_options(strict_default=False),
            )
            input_channel = create_memory_text_reader(
                '["Query", {"response": ["test"]}]\n'
            )
            memory_bytes_writer = MemoryBytesWriter()
            output_channel = AsyncTextWriter(memory_bytes_writer)
            with patch_connect_async(input_channel, output_channel):
                result = await pyre_query_manager.get_type_coverage(path=test_path)
            self.assertEqual(len(memory_bytes_writer.items()), 1)
            self.assertTrue(
                memory_bytes_writer.items()[0].startswith(
                    b'["QueryWithOverlay", {"query_text": "modules_of_path('
                )
            )
            self.assertTrue(result is not None)
            self.assertEqual(len(result.uncovered_ranges), 1)
            self.assertTrue(result.covered_percent < 100.0)

    @setup.async_test
    async def test_get_type_coverage__bad_json(self) -> None:
        pyre_query_manager = RequestHandler(
            server_state=_create_server_state_with_options(strict_default=False),
        )
        input_channel = create_memory_text_reader('{ "error": "Oops" }\n')
        output_channel = AsyncTextWriter(MemoryBytesWriter())
        with patch_connect_async(input_channel, output_channel):
            result = await pyre_query_manager.get_type_coverage(
                path=Path("test.py"),
            )
        self.assertTrue(result is None)

    @setup.async_test
    async def test_get_type_coverage__strict(self) -> None:
        with tempfile.NamedTemporaryFile(suffix=".py") as tmpfile:
            tmpfile.write(b"def foo(x):\n  pass\n")
            tmpfile.flush()
            test_path = Path(tmpfile.name)
            pyre_query_manager = RequestHandler(
                server_state=_create_server_state_with_options(strict_default=True),
            )
            input_channel = create_memory_text_reader(
                '["Query", {"response": ["test"]}]\n'
            )
            output_channel = AsyncTextWriter(MemoryBytesWriter())
            with patch_connect_async(input_channel, output_channel):
                result = await pyre_query_manager.get_type_coverage(path=test_path)
            self.assertTrue(result is not None)
            self.assertEqual(len(result.uncovered_ranges), 0)
            self.assertEqual(result.covered_percent, 100.0)

    @setup.async_test
    async def test_get_type_coverage__not_typechecked(self) -> None:
        pyre_query_manager = RequestHandler(
            server_state=_create_server_state_with_options(strict_default=False),
        )
        input_channel = create_memory_text_reader('["Query", {"response": []}]\n')
        output_channel = AsyncTextWriter(MemoryBytesWriter())
        with patch_connect_async(input_channel, output_channel):
            result = await pyre_query_manager.get_type_coverage(path=Path("test.py"))
        self.assertTrue(result is not None)
        self.assertEqual(result.covered_percent, 0.0)
        self.assertEqual(len(result.uncovered_ranges), 1)
        self.assertEqual(
            result.uncovered_ranges[0].message, "This file is not type checked by Pyre."
        )

    @setup.async_test
    async def test_get_type_coverage__expression_level(self) -> None:
        with tempfile.NamedTemporaryFile(suffix=".py") as tmpfile:
            tmpfile.write(b"def foo(x):\n  pass\n")
            tmpfile.flush()
            test_path = Path(tmpfile.name)
            pyre_query_manager = RequestHandler(
                server_state=_create_server_state_with_options(
                    strict_default=False,
                    language_server_features=LanguageServerFeatures(
                        type_coverage=TypeCoverageAvailability.EXPRESSION_LEVEL
                    ),
                ),
            )
            input_channel = create_memory_text_reader(
                '["Query", {"response": ["test"]}]\n ["Query", {"response": [["CoverageAtPath",{"path":"/fake/path.py","total_expressions":1,"coverage_gaps":[]}]]}]\n'
            )
            memory_bytes_writer = MemoryBytesWriter()
            output_channel = AsyncTextWriter(memory_bytes_writer)
            with patch_connect_async(input_channel, output_channel):
                result = await pyre_query_manager.get_type_coverage(path=test_path)
            self.assertEqual(len(memory_bytes_writer.items()), 2)
            self.assertTrue(
                memory_bytes_writer.items()[0].startswith(
                    b'["QueryWithOverlay", {"query_text": "modules_of_path('
                )
            )
            self.assertTrue(result is not None)
            self.assertEqual(len(result.uncovered_ranges), 0)
            self.assertTrue(result.covered_percent == 100.0)

    @setup.async_test
    async def test_get_type_coverage__expression_level__gaps(self) -> None:
        with tempfile.NamedTemporaryFile(suffix=".py") as tmpfile:
            tmpfile.write(b"def foo(x):\n  pass\n")
            tmpfile.flush()
            test_path = Path(tmpfile.name)
            pyre_query_manager = RequestHandler(
                server_state=_create_server_state_with_options(
                    strict_default=False,
                    language_server_features=LanguageServerFeatures(
                        type_coverage=TypeCoverageAvailability.EXPRESSION_LEVEL
                    ),
                ),
            )
            input_channel = create_memory_text_reader(
                '["Query", {"response": ["test"]}]\n ["Query", {"response": [["CoverageAtPath",{"path":"/fake/path.py","total_expressions":4,"coverage_gaps":[{"location": {"start": {"line": 11, "column": 16}, "stop": {"line": 11, "column": 17}}, "function_name":"foo","type_": "typing.Any", "reason": ["TypeIsAny"]}]}]]}]\n'
            )
            memory_bytes_writer = MemoryBytesWriter()
            output_channel = AsyncTextWriter(memory_bytes_writer)
            with patch_connect_async(input_channel, output_channel):
                result = await pyre_query_manager.get_type_coverage(
                    path=test_path,
                )
            self.assertEqual(len(memory_bytes_writer.items()), 2)
            self.assertTrue(
                memory_bytes_writer.items()[0].startswith(
                    b'["QueryWithOverlay", {"query_text": "modules_of_path('
                )
            )
            self.assertTrue(result is not None)
            self.assertEqual(len(result.uncovered_ranges), 1)
            self.assertTrue(result.covered_percent == 75.0)

    @setup.async_test
    async def test_get_type_coverage__expression_level__bad_json(self) -> None:
        pyre_query_manager = RequestHandler(
            server_state=_create_server_state_with_options(
                strict_default=False,
                language_server_features=LanguageServerFeatures(
                    type_coverage=TypeCoverageAvailability.EXPRESSION_LEVEL
                ),
            ),
        )
        input_channel = create_memory_text_reader(
            '{ "error": "Oops" }\n["Query", {"response": [["ErrorAtPath",{"path":"/fake/path.py","error":"oops"}]]}]\n'
        )
        output_channel = AsyncTextWriter(MemoryBytesWriter())
        with patch_connect_async(input_channel, output_channel):
            result = await pyre_query_manager.get_type_coverage(
                path=Path("test.py"),
            )
        self.assertTrue(result is None)

    @setup.async_test
    async def test_get_type_coverage__expression_level__strict(self) -> None:
        with tempfile.NamedTemporaryFile(suffix=".py") as tmpfile:
            tmpfile.write(b"def foo(x):\n  pass\n")
            tmpfile.flush()
            test_path = Path(tmpfile.name)
            pyre_query_manager = RequestHandler(
                server_state=_create_server_state_with_options(
                    strict_default=True,
                    language_server_features=LanguageServerFeatures(
                        type_coverage=TypeCoverageAvailability.EXPRESSION_LEVEL
                    ),
                ),
            )
            input_channel = create_memory_text_reader(
                '["Query", {"response": ["test"]}]\n["Query", {"response": [["CoverageAtPath",{"path":"/fake/path.py","total_expressions":0,"coverage_gaps":[]}]]}]\n'
            )
            output_channel = AsyncTextWriter(MemoryBytesWriter())
            with patch_connect_async(input_channel, output_channel):
                result = await pyre_query_manager.get_type_coverage(
                    path=test_path,
                )
            self.assertTrue(result is not None)
            self.assertEqual(len(result.uncovered_ranges), 0)
            self.assertEqual(result.covered_percent, 100.0)

    @setup.async_test
    async def test_query_hover(self) -> None:
        json_output = """{ "response": {"contents": "```foo.bar.Bar```"} }"""
        pyre_query_manager = RequestHandler(
            server_state=_create_server_state_with_options(
                language_server_features=LanguageServerFeatures(
                    hover=HoverAvailability.ENABLED
                ),
            ),
        )
        memory_bytes_writer = MemoryBytesWriter()
        flat_json = "".join(json_output.splitlines())
        input_channel = create_memory_text_reader(f'["Query", {flat_json}]\n')
        output_channel = AsyncTextWriter(memory_bytes_writer)

        with patch_connect_async(input_channel, output_channel):
            result = await pyre_query_manager.get_hover(
                path=Path("bar.py"), position=lsp.PyrePosition(line=42, character=10)
            )

        self.assertEqual(
            result,
            lsp.LspHoverResponse(contents="```foo.bar.Bar```"),
        )
        self.assertEqual(
            memory_bytes_writer.items(),
            [
                b'["QueryWithOverlay", {"query_text": "hover_info_for_position(path=\'bar.py\','
                b' line=42, column=10)", "overlay_id": null}]\n'
            ],
        )

    @setup.async_test
    async def test_query_hover__bad_json(self) -> None:
        pyre_query_manager = RequestHandler(
            server_state=_create_server_state_with_options(
                language_server_features=LanguageServerFeatures(
                    hover=HoverAvailability.ENABLED
                ),
            ),
        )

        input_channel = create_memory_text_reader("""{ "error": "Oops" }\n""")
        memory_bytes_writer = MemoryBytesWriter()
        output_channel = AsyncTextWriter(memory_bytes_writer)
        with patch_connect_async(input_channel, output_channel):
            result = await pyre_query_manager.get_hover(
                path=Path("bar.py"),
                position=lsp.PyrePosition(line=42, character=10),
            )

        self.assertEqual(
            result,
            lsp.LspHoverResponse.empty(),
        )

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
        pyre_query_manager = RequestHandler(
            server_state=_create_server_state_with_options(
                language_server_features=LanguageServerFeatures(
                    hover=HoverAvailability.ENABLED
                ),
            ),
        )
        memory_bytes_writer = MemoryBytesWriter()
        flat_json = "".join(json_output.splitlines())
        input_channel = create_memory_text_reader(f'["Query", {flat_json}]\n')
        output_channel = AsyncTextWriter(memory_bytes_writer)

        with patch_connect_async(input_channel, output_channel):
            response = await pyre_query_manager.get_definition_locations(
                path=Path("bar.py"),
                position=lsp.PyrePosition(line=42, character=10),
            )

        self.assertEqual(
            memory_bytes_writer.items(),
            [
                b'["QueryWithOverlay", {"query_text": "location_of_definition(path=\'bar.py\','
                b' line=42, column=10)", "overlay_id": null}]\n'
            ],
        )
        self.assertEqual(
            response,
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
        pyre_query_manager = RequestHandler(
            server_state=_create_server_state_with_options(
                language_server_features=LanguageServerFeatures(
                    hover=HoverAvailability.ENABLED
                )
            ),
        )

        input_channel = create_memory_text_reader("""{ "error": "Oops" }\n""")
        memory_bytes_writer = MemoryBytesWriter()
        output_channel = AsyncTextWriter(memory_bytes_writer)
        with patch_connect_async(input_channel, output_channel):
            response = await pyre_query_manager.get_definition_locations(
                path=Path("bar.py"),
                position=lsp.PyrePosition(line=42, character=10),
            )

        self.assertEqual(
            response,
            [],
        )

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
        pyre_query_manager = RequestHandler(
            server_state=_create_server_state_with_options(
                language_server_features=LanguageServerFeatures(
                    references=ReferencesAvailability.ENABLED,
                ),
            ),
        )
        memory_bytes_writer = MemoryBytesWriter()
        flat_json = "".join(json_output.splitlines())
        input_channel = create_memory_text_reader(f'["Query", {flat_json}]\n')
        output_channel = AsyncTextWriter(memory_bytes_writer)

        with patch_connect_async(input_channel, output_channel):
            result = await pyre_query_manager.get_reference_locations(
                path=Path("bar.py"),
                position=lsp.PyrePosition(line=42, character=10),
            )

        self.assertEqual(
            memory_bytes_writer.items(),
            [
                b'["QueryWithOverlay", {"query_text": "find_references(path=\'bar.py\','
                b' line=42, column=10)", "overlay_id": null}]\n'
            ],
        )
        self.assertEqual(
            result,
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
        pyre_query_manager = RequestHandler(
            server_state=_create_server_state_with_options(
                language_server_features=LanguageServerFeatures(
                    references=ReferencesAvailability.ENABLED,
                ),
            ),
        )
        input_channel = create_memory_text_reader("""{ "error": "Oops" }\n""")
        memory_bytes_writer = MemoryBytesWriter()
        output_channel = AsyncTextWriter(memory_bytes_writer)
        with patch_connect_async(input_channel, output_channel):
            result = await pyre_query_manager.get_reference_locations(
                path=Path("bar.py"),
                position=lsp.PyrePosition(line=42, character=10),
            )

        self.assertEqual(result, [])
