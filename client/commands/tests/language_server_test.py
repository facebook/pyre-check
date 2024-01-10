# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import abc
import asyncio
import json
import sys
import tempfile
from pathlib import Path
from typing import Callable, Dict, List, Optional, Union

import testslide

from ... import backend_arguments, background_tasks, error, json_rpc
from ...language_server import connections, features, protocol as lsp
from ...tests import setup
from .. import (
    daemon_querier,
    initialization as init,
    launch_and_subscribe_handler,
    persistent,
    pyre_language_server,
    pyre_server_options,
    server_state as state,
    start,
    status_message_handler,
    subscription,
    type_error_handler,
)

from ..tests import server_setup


class BlockingPyreLanguageServer(pyre_language_server.PyreLanguageServerApi):
    async def process_definition_request(
        self,
        parameters: lsp.DefinitionParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> Optional[Exception]:
        await asyncio.Event().wait()
        return None

    async def process_hover_request(
        self,
        parameters: lsp.HoverParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        return

    async def write_telemetry(
        self,
        parameters: Dict[str, object],
        activity_key: Optional[Dict[str, object]],
    ) -> None:
        raise NotImplementedError()

    def get_language_server_features(self) -> features.LanguageServerFeatures:
        raise NotImplementedError()

    async def update_overlay_if_needed(self, document_path: Path) -> float:
        raise NotImplementedError()

    def sample_source_code(
        self,
        document_path: Path,
        position: lsp.LspPosition,
    ) -> Optional[str]:
        raise NotImplementedError()

    async def process_open_request(
        self,
        parameters: lsp.DidOpenTextDocumentParameters,
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    async def process_close_request(
        self, parameters: lsp.DidCloseTextDocumentParameters
    ) -> None:
        raise NotImplementedError()

    async def process_did_change_request(
        self,
        parameters: lsp.DidChangeTextDocumentParameters,
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    async def process_did_save_request(
        self,
        parameters: lsp.DidSaveTextDocumentParameters,
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    async def process_type_coverage_request(
        self,
        parameters: lsp.TypeCoverageParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    async def _get_definition_result(
        self, document_path: Path, position: lsp.LspPosition
    ) -> pyre_language_server.QueryResultWithDurations[List[Dict[str, object]]]:
        raise NotImplementedError()

    async def process_completion_request(
        self,
        parameters: lsp.CompletionParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> Optional[Exception]:
        raise NotImplementedError()

    async def process_document_symbols_request(
        self,
        parameters: lsp.DocumentSymbolsParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    async def process_find_all_references_request(
        self,
        parameters: lsp.ReferencesParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    async def process_call_hierarchy_request(
        self,
        parameters: lsp.CallHierarchyParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    async def process_call_hierarchy_incoming_call(
        self,
        parameters: lsp.CallHierarchyIncomingCallParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    async def process_call_hierarchy_outgoing_call(
        self,
        parameters: lsp.CallHierarchyOutgoingCallParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    async def process_rename_request(
        self,
        parameters: lsp.RenameParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    async def process_shutdown_request(self, request_id: Union[int, str, None]) -> None:
        raise NotImplementedError()


class ReadLspRequestTest(testslide.TestCase):
    @setup.async_test
    async def test_read_lsp_request_success(self) -> None:
        expected_request = json_rpc.Request(
            id=0,
            method="derp",
        )
        input_channel = await server_setup.create_input_channel_with_requests(
            [expected_request]
        )
        bytes_writer = connections.MemoryBytesWriter()
        output_channel = connections.AsyncTextWriter(bytes_writer)
        actual_request = await pyre_language_server.read_lsp_request(
            input_channel, output_channel
        )
        self.assertEqual(actual_request, expected_request)
        self.assertEqual(len(bytes_writer.items()), 0)

    @setup.async_test
    async def test_read_lsp_request_fail_channel_closed(self) -> None:
        input_channel = connections.create_memory_text_reader("")
        bytes_writer = connections.MemoryBytesWriter()
        output_channel = connections.AsyncTextWriter(bytes_writer)
        with self.assertRaises(lsp.ReadChannelClosedError):
            await pyre_language_server.read_lsp_request(input_channel, output_channel)
        self.assertEqual(len(bytes_writer.items()), 0)

    @setup.async_test
    async def test_read_lsp_request_fail_incomplete_read(self) -> None:
        input_channel = connections.create_memory_text_reader("derp\n")
        bytes_writer = connections.MemoryBytesWriter()
        output_channel = connections.AsyncTextWriter(bytes_writer)
        with self.assertRaises(lsp.ReadChannelClosedError):
            await pyre_language_server.read_lsp_request(input_channel, output_channel)
        # One message for one failed read
        self.assertEqual(len(bytes_writer.items()), 1)

    @setup.async_test
    async def test_read_lsp_request_success_after_failed_reads(self) -> None:
        expected_request = json_rpc.Request(
            id=0,
            method="derp",
        )
        input_channel = connections.create_memory_text_reader(
            f"foo\r\n\r\nbar\r\n\r\n{lsp.json_rpc_payload(expected_request)}"
        )
        bytes_writer = connections.MemoryBytesWriter()
        output_channel = connections.AsyncTextWriter(bytes_writer)
        actual_request = await pyre_language_server.read_lsp_request(
            input_channel, output_channel
        )
        self.assertEqual(actual_request, expected_request)
        # Two messages for two failed reads
        self.assertEqual(len(bytes_writer.items()), 2)


class InitializeTest(testslide.TestCase):
    @setup.async_test
    async def test_try_initialize_success(self) -> None:
        input_channel = await server_setup.create_input_channel_with_requests(
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
        bytes_writer = connections.MemoryBytesWriter()
        result = await init.async_try_initialize(
            input_channel,
            connections.AsyncTextWriter(bytes_writer),
            compute_initialize_result=lambda parameters: persistent.process_initialize_request(
                parameters,
                server_setup.DEFAULT_FEATURES,
            ),
        )
        self.assertIsInstance(result, init.InitializationSuccess)
        self.assertEqual(len(bytes_writer.items()), 1)

    @setup.async_test
    async def test_try_initialize_failure__not_a_request(self) -> None:
        input_channel = await server_setup.create_input_channel_with_requests(
            [json_rpc.Request(method="derp", parameters=None)]
        )
        output_channel = connections.create_memory_text_writer()
        result = await init.async_try_initialize(
            input_channel,
            output_channel,
            compute_initialize_result=lambda parameters: persistent.process_initialize_request(
                parameters,
                server_setup.DEFAULT_FEATURES,
            ),
        )
        self.assertIsInstance(result, init.InitializationFailure)

    @setup.async_test
    async def test_try_initialize_failure__invalid_parameters(self) -> None:
        input_channel = await server_setup.create_input_channel_with_requests(
            [json_rpc.Request(id=0, method="initialize", parameters=None)]
        )
        output_channel = connections.create_memory_text_writer()
        result = await init.async_try_initialize(
            input_channel,
            output_channel,
            compute_initialize_result=lambda parameters: persistent.process_initialize_request(
                parameters,
                server_setup.DEFAULT_FEATURES,
            ),
        )
        self.assertIsInstance(result, init.InitializationFailure)

    @setup.async_test
    async def test_try_initialize_failure__no_initialized(self) -> None:
        input_channel = await server_setup.create_input_channel_with_requests(
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
        output_channel = connections.create_memory_text_writer()
        result = await init.async_try_initialize(
            input_channel,
            output_channel,
            compute_initialize_result=lambda parameters: persistent.process_initialize_request(
                parameters,
                server_setup.DEFAULT_FEATURES,
            ),
        )
        self.assertIsInstance(result, init.InitializationFailure)

    @setup.async_test
    async def test_try_initialize_exit(self) -> None:
        input_channel = await server_setup.create_input_channel_with_requests(
            [json_rpc.Request(method="exit", parameters=None)]
        )
        output_channel = connections.create_memory_text_writer()
        result = await init.async_try_initialize(
            input_channel,
            output_channel,
            compute_initialize_result=lambda parameters: persistent.process_initialize_request(
                parameters,
                server_setup.DEFAULT_FEATURES,
            ),
        )
        self.assertIsInstance(result, init.InitializationExit)

    @setup.async_test
    async def test_try_initialize_exit__shutdown_after_initialize(self) -> None:
        input_channel = await server_setup.create_input_channel_with_requests(
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
        output_channel = connections.create_memory_text_writer()
        result = await init.async_try_initialize(
            input_channel,
            output_channel,
            compute_initialize_result=lambda parameters: persistent.process_initialize_request(
                parameters,
                server_setup.DEFAULT_FEATURES,
            ),
        )
        self.assertIsInstance(result, init.InitializationExit)

    @setup.async_test
    async def test_try_initialize_exit__shutdown_without_exit(self) -> None:
        input_channel = await server_setup.create_input_channel_with_requests(
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
        output_channel = connections.create_memory_text_writer()
        result = await init.async_try_initialize(
            input_channel,
            output_channel,
            compute_initialize_result=lambda parameters: persistent.process_initialize_request(
                parameters,
                server_setup.DEFAULT_FEATURES,
            ),
        )
        self.assertIsInstance(result, init.InitializationExit)

    @setup.async_test
    async def test_try_initialize_exit__without_anything(self) -> None:
        result = await init.async_try_initialize(
            connections.create_memory_text_reader(""),
            connections.create_memory_text_writer(),
            compute_initialize_result=lambda parameters: persistent.process_initialize_request(
                parameters,
                server_setup.DEFAULT_FEATURES,
            ),
        )
        self.assertIsInstance(result, init.InitializationExit)


class DiagnosticHelperFunctionsTest(testslide.TestCase):
    def test_type_diagnostics(self) -> None:
        self.assertEqual(
            type_error_handler.type_error_to_diagnostic(
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
            type_error_handler.type_errors_to_diagnostics(
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


class PyreLanguageServerDispatcherTest(testslide.TestCase):
    @staticmethod
    def _by_name_parameters(
        parameters: Union[
            lsp.DidSaveTextDocumentParameters,
            lsp.DidOpenTextDocumentParameters,
            lsp.HoverParameters,
            lsp.DefinitionParameters,
        ],
    ) -> json_rpc.ByNameParameters:
        return json_rpc.ByNameParameters(values=json.loads(parameters.to_json()))

    @setup.async_test
    async def test_exit(self) -> None:
        server_state = server_setup.mock_server_state
        input_channel = await server_setup.create_input_channel_with_requests(
            [
                json_rpc.Request(method="shutdown", parameters=None),
                json_rpc.Request(method="exit", parameters=None),
            ]
        )
        dispatcher, _ = server_setup.create_pyre_language_server_dispatcher(
            input_channel=input_channel,
            server_state=server_state,
            querier=server_setup.MockDaemonQuerier(),
            daemon_manager=background_tasks.TaskManager(
                server_setup.NoOpBackgroundTask()
            ),
        )
        exit_code = await dispatcher.run()
        self.assertEqual(exit_code, 0)

    @setup.async_test
    async def test_exit_unknown_request_after_shutdown(self) -> None:
        server_state = server_setup.mock_server_state
        input_channel = await server_setup.create_input_channel_with_requests(
            [
                json_rpc.Request(method="shutdown", parameters=None),
                json_rpc.Request(method="derp", parameters=None),
                json_rpc.Request(method="exit", parameters=None),
            ]
        )
        dispatcher, _ = server_setup.create_pyre_language_server_dispatcher(
            input_channel=input_channel,
            server_state=server_state,
            querier=server_setup.MockDaemonQuerier(),
            daemon_manager=background_tasks.TaskManager(
                server_setup.NoOpBackgroundTask()
            ),
        )
        exit_code = await dispatcher.run()
        self.assertEqual(exit_code, 0)

    @setup.async_test
    async def test_exit_gracefully_after_shutdown(self) -> None:
        server_state = server_setup.mock_server_state
        input_channel = await server_setup.create_input_channel_with_requests(
            [
                json_rpc.Request(method="shutdown", parameters=None),
            ]
        )
        dispatcher, _ = server_setup.create_pyre_language_server_dispatcher(
            input_channel=input_channel,
            server_state=server_state,
            querier=server_setup.MockDaemonQuerier(),
            daemon_manager=background_tasks.TaskManager(
                server_setup.NoOpBackgroundTask()
            ),
        )
        exit_code = await dispatcher.run()
        self.assertEqual(exit_code, 0)

    @setup.async_test
    async def test_exit_gracefully_on_channel_closure(self) -> None:
        server_state = server_setup.mock_server_state
        noop_task_manager = background_tasks.TaskManager(
            server_setup.NoOpBackgroundTask()
        )
        dispatcher, output_writer = server_setup.create_pyre_language_server_dispatcher(
            # Feed nothing to input channel
            input_channel=connections.create_memory_text_reader(""),
            server_state=server_state,
            daemon_manager=noop_task_manager,
            querier=server_setup.MockDaemonQuerier(),
        )
        exit_code = await dispatcher.run()
        self.assertEqual(exit_code, 0)
        self.assertEqual(len(output_writer.items()), 0)

    @setup.async_test
    async def test_dispatch_request_triggers_restart(self) -> None:
        """
        Check that the dispatch_request method restarts the daemon on some
        example requests.
        """
        test_path = Path("/foo.py")
        for request in [
            json_rpc.Request(
                method="textDocument/didSave",
                parameters=self._by_name_parameters(
                    lsp.DidSaveTextDocumentParameters(
                        text_document=lsp.TextDocumentIdentifier(
                            uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                        )
                    )
                ),
            ),
            json_rpc.Request(
                method="textDocument/didSave",
                parameters=self._by_name_parameters(
                    lsp.DidOpenTextDocumentParameters(
                        text_document=lsp.TextDocumentItem(
                            language_id="python",
                            text="",
                            uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                            version=0,
                        )
                    )
                ),
            ),
        ]:
            fake_task_manager = background_tasks.TaskManager(
                server_setup.WaitForeverBackgroundTask()
            )
            dispatcher, _ = server_setup.create_pyre_language_server_dispatcher(
                input_channel=connections.create_memory_text_reader(""),
                server_state=state.ServerState(
                    server_options=server_setup.mock_initial_server_options,
                    opened_documents={
                        test_path: state.OpenedDocumentState(
                            code=server_setup.DEFAULT_FILE_CONTENTS
                        )
                    },
                ),
                daemon_manager=fake_task_manager,
                querier=server_setup.MockDaemonQuerier(),
            )
            self.assertFalse(fake_task_manager.is_task_running())

            await dispatcher.dispatch_request(request)
            await asyncio.sleep(0)
            self.assertTrue(fake_task_manager.is_task_running())

    @setup.async_test
    async def test_dispatch_request_triggers_restart__limit_reached(self) -> None:
        """
        Check on some example requests that, if we've reached our restart limit,
        we skip trying to restart the daemon connection.
        """
        test_path = Path("/foo.py")
        for request in [
            json_rpc.Request(
                method="textDocument/didSave",
                parameters=self._by_name_parameters(
                    lsp.DidSaveTextDocumentParameters(
                        text_document=lsp.TextDocumentIdentifier(
                            uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                        )
                    )
                ),
            ),
            json_rpc.Request(
                method="textDocument/didSave",
                parameters=self._by_name_parameters(
                    lsp.DidOpenTextDocumentParameters(
                        text_document=lsp.TextDocumentItem(
                            language_id="python",
                            text="",
                            uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                            version=0,
                        )
                    )
                ),
            ),
        ]:
            fake_task_manager = background_tasks.TaskManager(
                server_setup.WaitForeverBackgroundTask()
            )
            dispatcher, _ = server_setup.create_pyre_language_server_dispatcher(
                input_channel=connections.create_memory_text_reader(""),
                server_state=state.ServerState(
                    server_options=server_setup.mock_initial_server_options,
                    opened_documents={
                        test_path: state.OpenedDocumentState(
                            code=server_setup.DEFAULT_FILE_CONTENTS
                        )
                    },
                    consecutive_start_failure=launch_and_subscribe_handler.CONSECUTIVE_START_ATTEMPT_THRESHOLD,
                ),
                daemon_manager=fake_task_manager,
                querier=server_setup.MockDaemonQuerier(),
            )
            self.assertFalse(fake_task_manager.is_task_running())

            print(request)
            await dispatcher.dispatch_request(request)
            await asyncio.sleep(0)
            self.assertFalse(fake_task_manager.is_task_running())

    @setup.async_test
    async def test_dispatch_requests_concurrently(self) -> None:
        """
        Dispatches a blocking request then a non-blocking request and ensures the
        second one goes through.
        """
        test_path = Path("/foo.py")
        requests = [
            json_rpc.Request(
                method="textDocument/definition",
                parameters=self._by_name_parameters(
                    lsp.DefinitionParameters(
                        text_document=lsp.TextDocumentIdentifier(
                            uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                        ),
                        position=lsp.LspPosition(line=1, character=1),
                    )
                ),
            ),
            json_rpc.Request(
                method="textDocument/hover",
                parameters=self._by_name_parameters(
                    lsp.HoverParameters(
                        text_document=lsp.TextDocumentIdentifier(
                            uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                        ),
                        position=lsp.LspPosition(line=1, character=1),
                    )
                ),
            ),
        ]
        output_channel = connections.AsyncTextWriter(connections.MemoryBytesWriter())
        server_state = state.ServerState(
            server_options=server_setup.mock_initial_server_options,
            opened_documents={
                test_path: state.OpenedDocumentState(
                    code=server_setup.DEFAULT_FILE_CONTENTS
                )
            },
            consecutive_start_failure=launch_and_subscribe_handler.CONSECUTIVE_START_ATTEMPT_THRESHOLD,
        )
        dispatcher = pyre_language_server.PyreLanguageServerDispatcher(
            input_channel=connections.create_memory_text_reader(""),
            output_channel=output_channel,
            server_state=server_state,
            daemon_manager=background_tasks.TaskManager(
                server_setup.NoOpBackgroundTask()
            ),
            api=BlockingPyreLanguageServer(),
        )
        await asyncio.gather(
            *[dispatcher.dispatch_request(request) for request in requests]
        )
        self.assertEqual(len(dispatcher.outstanding_tasks), 1)
        self.assertFalse(dispatcher.outstanding_tasks.pop().done())

    @setup.async_test
    async def test_request_swallows_exceptions(self) -> None:
        """This is not the ideal behavior. We should be raising these exceptions."""

        class RaisingLanguageServer(BlockingPyreLanguageServer):
            async def process_definition_request(
                self,
                parameters: lsp.DefinitionParameters,
                request_id: Union[int, str, None],
                activity_key: Optional[Dict[str, object]] = None,
            ) -> Optional[Exception]:
                raise Exception("Test")

        output_channel = connections.AsyncTextWriter(connections.MemoryBytesWriter())
        server_state = state.ServerState(
            server_options=server_setup.mock_initial_server_options,
            opened_documents={},
            consecutive_start_failure=launch_and_subscribe_handler.CONSECUTIVE_START_ATTEMPT_THRESHOLD,
        )
        dispatcher = pyre_language_server.PyreLanguageServerDispatcher(
            input_channel=connections.create_memory_text_reader(""),
            output_channel=output_channel,
            server_state=server_state,
            daemon_manager=background_tasks.TaskManager(
                server_setup.NoOpBackgroundTask()
            ),
            api=RaisingLanguageServer(),
        )
        # See no exceptions raised
        await dispatcher.dispatch_request(
            json_rpc.Request(
                method="textDocument/definition",
                parameters=self._by_name_parameters(
                    lsp.DefinitionParameters(
                        text_document=lsp.TextDocumentIdentifier(uri=""),
                        position=lsp.LspPosition(line=1, character=1),
                    )
                ),
            )
        )


class ClientTypeErrorHandlerTest(testslide.TestCase):
    @setup.async_test
    async def test_clear_type_errors_for_client(self) -> None:
        server_state = server_setup.create_server_state_with_options()
        server_state.diagnostics = {
            Path("foo.py"): [],
        }
        bytes_writer = connections.MemoryBytesWriter()
        client_output_channel = connections.AsyncTextWriter(bytes_writer)

        error_handler = type_error_handler.ClientTypeErrorHandler(
            client_output_channel=client_output_channel,
            server_state=server_state,
        )
        await error_handler.clear_type_errors_for_client()

        client_messages = [x.decode("utf-8") for x in bytes_writer.items()]
        self.assertEqual(len(client_messages), 1)
        print(client_messages)
        message = client_messages[0]
        self.assertIn("textDocument/publishDiagnostics", message)
        self.assertIn('{"uri": "file:///foo.py", "diagnostics": []}}', message)

    @setup.async_test
    async def test_update_type_errors_and_show_type_errors_to_client(self) -> None:
        server_state = server_setup.create_server_state_with_options()
        bytes_writer = connections.MemoryBytesWriter()
        client_output_channel = connections.AsyncTextWriter(bytes_writer)

        error_handler = type_error_handler.ClientTypeErrorHandler(
            client_output_channel=client_output_channel,
            server_state=server_state,
        )
        error_handler.update_type_errors(
            [
                error.Error(
                    line=1,
                    column=1,
                    stop_line=2,
                    stop_column=2,
                    path=Path("derp.py"),
                    code=42,
                    name="name",
                    description="first error",
                ),
                error.Error(
                    line=1,
                    column=1,
                    stop_line=2,
                    stop_column=2,
                    path=Path("derp.py"),
                    code=42,
                    name="name",
                    description="second error",
                ),
            ]
        )
        await error_handler.show_type_errors_to_client()

        self.assertDictEqual(
            server_state.diagnostics,
            {
                Path("derp.py"): [
                    lsp.Diagnostic(
                        range=lsp.LspRange(
                            start=lsp.LspPosition(line=0, character=1),
                            end=lsp.LspPosition(line=1, character=2),
                        ),
                        message="first error",
                        severity=lsp.DiagnosticSeverity.ERROR,
                        code=None,
                        source="Pyre",
                    ),
                    lsp.Diagnostic(
                        range=lsp.LspRange(
                            start=lsp.LspPosition(line=0, character=1),
                            end=lsp.LspPosition(line=1, character=2),
                        ),
                        message="second error",
                        severity=lsp.DiagnosticSeverity.ERROR,
                        code=None,
                        source="Pyre",
                    ),
                ]
            },
        )
        client_messages = [x.decode("utf-8") for x in bytes_writer.items()]
        print(client_messages)
        self.assertEqual(len(client_messages), 1)
        message = client_messages[0]
        self.assertIn("textDocument/publishDiagnostics", message)

    @setup.async_test
    async def test_show_overlay_type_errors__non_empty(self) -> None:
        server_state = server_setup.create_server_state_with_options()
        bytes_writer = connections.MemoryBytesWriter()
        client_output_channel = connections.AsyncTextWriter(bytes_writer)

        error_handler = type_error_handler.ClientTypeErrorHandler(
            client_output_channel=client_output_channel,
            server_state=server_state,
        )
        await error_handler.show_overlay_type_errors(
            Path("derp.py"),
            [
                error.Error(
                    line=1,
                    column=1,
                    stop_line=2,
                    stop_column=2,
                    path=Path("derp.py"),
                    code=42,
                    name="name",
                    description="first error",
                ),
                error.Error(
                    line=1,
                    column=1,
                    stop_line=2,
                    stop_column=2,
                    path=Path("derp.py"),
                    code=42,
                    name="name",
                    description="second error",
                ),
            ],
        )
        client_messages = [x.decode("utf-8") for x in bytes_writer.items()]
        print(client_messages)
        self.assertEqual(len(client_messages), 1)
        message = client_messages[0]
        self.assertIn("textDocument/publishDiagnostics", message)
        self.assertIn("derp.py", message)
        self.assertIn("first error", message)

    @setup.async_test
    async def test_show_overlay_type_errors__empty(self) -> None:
        server_state = server_setup.create_server_state_with_options()
        bytes_writer = connections.MemoryBytesWriter()
        client_output_channel = connections.AsyncTextWriter(bytes_writer)

        error_handler = type_error_handler.ClientTypeErrorHandler(
            client_output_channel=client_output_channel,
            server_state=server_state,
        )
        await error_handler.show_overlay_type_errors(Path("derp.py"), [])
        client_messages = [x.decode("utf-8") for x in bytes_writer.items()]
        print(client_messages)
        self.assertEqual(len(client_messages), 1)
        message = client_messages[0]
        self.assertIn("textDocument/publishDiagnostics", message)
        self.assertIn("derp.py", message)
        self.assertIn('"diagnostics": []', message)


class PyreDaemonLaunchAndSubscribeHandlerTest(testslide.TestCase):
    @setup.async_test
    async def test_subscription_protocol(self) -> None:
        server_state = state.ServerState(
            client_capabilities=lsp.ClientCapabilities(
                window=lsp.WindowClientCapabilities(
                    status=lsp.ShowStatusRequestClientCapabilities(),
                ),
            ),
            server_options=server_setup.mock_initial_server_options,
        )
        bytes_writer = connections.MemoryBytesWriter()

        def fake_server_options_reader() -> pyre_server_options.PyreServerOptions:
            # Server start option is not relevant to this test
            raise NotImplementedError()

        client_output_channel = connections.AsyncTextWriter(bytes_writer)

        server_handler = persistent.PyrePersistentDaemonLaunchAndSubscribeHandler(
            server_options_reader=fake_server_options_reader,
            server_state=server_state,
            client_status_message_handler=status_message_handler.ClientStatusMessageHandler(
                client_output_channel, server_state
            ),
            client_type_error_handler=type_error_handler.ClientTypeErrorHandler(
                client_output_channel, server_state
            ),
        )
        await server_handler.handle_status_update_event(
            subscription.StatusUpdate(kind="Rebuilding")
        )
        await server_handler.handle_status_update_event(
            subscription.StatusUpdate(kind="Rechecking")
        )
        await server_handler.handle_type_error_event(
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
        self.assertTrue(len(client_messages) == 4)
        # Forward the rebuild status message
        self.assertIn("window/showStatus", client_messages[0])
        # Forward the recheck status message
        self.assertIn("window/showStatus", client_messages[1])
        # Clear out diagnostics for subsequent type errors
        self.assertIn("textDocument/publishDiagnostics", client_messages[2])
        # Notify the user that incremental check has finished
        self.assertIn("window/showStatus", client_messages[3])

    @setup.async_test
    async def test_subscription_protocol_no_status_updates(self) -> None:
        server_state = state.ServerState(
            client_capabilities=lsp.ClientCapabilities(
                window=lsp.WindowClientCapabilities(
                    status=lsp.ShowStatusRequestClientCapabilities(),
                ),
            ),
            server_options=server_setup.create_server_options(
                language_server_features=features.LanguageServerFeatures(
                    status_updates=features.StatusUpdatesAvailability.DISABLED,
                )
            ),
        )
        bytes_writer = connections.MemoryBytesWriter()

        def fake_server_options_reader() -> pyre_server_options.PyreServerOptions:
            # Server start option is not relevant to this test
            raise NotImplementedError()

        client_output_channel = connections.AsyncTextWriter(bytes_writer)

        server_handler = persistent.PyrePersistentDaemonLaunchAndSubscribeHandler(
            server_options_reader=fake_server_options_reader,
            server_state=server_state,
            client_status_message_handler=status_message_handler.ClientStatusMessageHandler(
                client_output_channel, server_state
            ),
            client_type_error_handler=type_error_handler.ClientTypeErrorHandler(
                client_output_channel, server_state
            ),
        )
        await server_handler.handle_status_update_event(
            subscription.StatusUpdate(kind="Rebuilding")
        )
        await server_handler.handle_status_update_event(
            subscription.StatusUpdate(kind="Rechecking")
        )
        await server_handler.handle_type_error_event(
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
        self.assertTrue(len(client_messages) == 1)
        self.assertIn("textDocument/publishDiagnostics", client_messages[0])

    @setup.async_test
    async def test_subscription_error(self) -> None:
        def fake_server_options_reader() -> pyre_server_options.PyreServerOptions:
            # Server start option is not relevant to this test
            raise NotImplementedError()

        client_output_channel = connections.AsyncTextWriter(
            connections.MemoryBytesWriter()
        )
        server_handler = persistent.PyrePersistentDaemonLaunchAndSubscribeHandler(
            server_options_reader=fake_server_options_reader,
            server_state=server_setup.mock_server_state,
            client_status_message_handler=status_message_handler.ClientStatusMessageHandler(
                client_output_channel, server_setup.mock_server_state
            ),
            client_type_error_handler=type_error_handler.ClientTypeErrorHandler(
                client_output_channel, server_setup.mock_server_state
            ),
        )
        with self.assertRaises(launch_and_subscribe_handler.PyreDaemonShutdown):
            await server_handler.handle_error_event(
                subscription.Error(message="Doom Eternal")
            )

    @setup.async_test
    async def test_busy_status_clear_diagnostics(self) -> None:
        path = Path("foo.py")
        server_state = state.ServerState(
            server_options=server_setup.mock_initial_server_options,
            diagnostics={path: []},
        )
        bytes_writer = connections.MemoryBytesWriter()

        def fake_server_options_reader() -> pyre_server_options.PyreServerOptions:
            # Server start option is not relevant to this test
            raise NotImplementedError()

        client_output_channel = connections.AsyncTextWriter(bytes_writer)
        server_handler = persistent.PyrePersistentDaemonLaunchAndSubscribeHandler(
            server_options_reader=fake_server_options_reader,
            server_state=server_state,
            client_status_message_handler=status_message_handler.ClientStatusMessageHandler(
                client_output_channel, server_state
            ),
            client_type_error_handler=type_error_handler.ClientTypeErrorHandler(
                client_output_channel, server_state
            ),
        )
        await server_handler.handle_status_update_event(
            subscription.StatusUpdate(kind="Rebuilding")
        )
        await server_handler.handle_status_update_event(
            subscription.StatusUpdate(kind="Rechecking")
        )

        client_messages = [x.decode("utf-8") for x in bytes_writer.items()]
        self.assertTrue(len(client_messages) == 2)
        # Clear out diagnostics for rebuilding status
        self.assertIn('"diagnostics": []', client_messages[0])
        self.assertIn('"diagnostics": []', client_messages[1])

    @setup.async_test
    async def test_busy_status_no_clear_diagnostics_if_no_type_errors(self) -> None:
        path = Path("foo.py")
        server_state = state.ServerState(
            server_options=server_setup.create_server_options(
                language_server_features=features.LanguageServerFeatures(
                    type_errors=features.TypeErrorsAvailability.DISABLED,
                ),
            ),
            diagnostics={path: []},
        )
        bytes_writer = connections.MemoryBytesWriter()
        client_output_channel = connections.AsyncTextWriter(bytes_writer)

        def fake_server_options_reader() -> pyre_server_options.PyreServerOptions:
            # Server start option is not relevant to this test
            raise NotImplementedError()

        server_handler = persistent.PyrePersistentDaemonLaunchAndSubscribeHandler(
            server_options_reader=fake_server_options_reader,
            server_state=server_state,
            client_status_message_handler=status_message_handler.ClientStatusMessageHandler(
                client_output_channel, server_state
            ),
            client_type_error_handler=type_error_handler.ClientTypeErrorHandler(
                client_output_channel, server_state
            ),
        )
        await server_handler.handle_status_update_event(
            subscription.StatusUpdate(kind="Rebuilding")
        )
        await server_handler.handle_status_update_event(
            subscription.StatusUpdate(kind="Rechecking")
        )

        client_messages = [x.decode("utf-8") for x in bytes_writer.items()]
        self.assertTrue(len(client_messages) == 0)

    @setup.async_test
    async def test_connections_lost(self) -> None:
        test_path = Path("/foo.py")
        server_state = state.ServerState(
            server_options=server_setup.mock_initial_server_options,
            diagnostics={test_path: []},
        )

        bytes_writer = connections.MemoryBytesWriter()
        client_output_channel = connections.AsyncTextWriter(bytes_writer)
        server_handler = persistent.PyrePersistentDaemonLaunchAndSubscribeHandler(
            server_options_reader=lambda: server_setup.create_server_options(
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
            server_state=server_state,
            client_status_message_handler=status_message_handler.ClientStatusMessageHandler(
                client_output_channel, server_state
            ),
            client_type_error_handler=type_error_handler.ClientTypeErrorHandler(
                client_output_channel, server_state
            ),
        )

        with self.assertRaises(asyncio.IncompleteReadError):
            await server_handler.subscribe(
                # Intentionally inject a broken server response
                connections.create_memory_text_reader("derp"),
                connections.create_memory_text_writer(),
            )
        client_visible_messages = bytes_writer.items()
        self.assertTrue(len(client_visible_messages) > 0)
        self.assertEqual(
            await lsp.read_json_rpc(
                connections.AsyncTextReader(
                    connections.MemoryBytesReader(
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
        bytes_writer = connections.MemoryBytesWriter()
        client_output_channel = connections.AsyncTextWriter(bytes_writer)
        server_state = state.ServerState(
            client_capabilities=lsp.ClientCapabilities(
                window=lsp.WindowClientCapabilities(
                    status=lsp.ShowStatusRequestClientCapabilities(),
                ),
            ),
            server_options=server_setup.mock_initial_server_options,
        )
        server_handler = persistent.PyrePersistentDaemonLaunchAndSubscribeHandler(
            server_options_reader=lambda: server_setup.create_server_options(
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
            server_state=server_state,
            client_status_message_handler=status_message_handler.ClientStatusMessageHandler(
                client_output_channel, server_state
            ),
            client_type_error_handler=type_error_handler.ClientTypeErrorHandler(
                client_output_channel, server_state
            ),
        )
        await server_handler.client_status_message_handler.show_status_message_to_client(
            message="derp", level=lsp.MessageType.WARNING
        )

        client_visible_messages = bytes_writer.items()
        self.assertTrue(len(client_visible_messages) > 0)
        self.assertEqual(
            await lsp.read_json_rpc(
                connections.AsyncTextReader(
                    connections.MemoryBytesReader(client_visible_messages[-1])
                )
            ),
            json_rpc.Request(
                method="window/showStatus",
                id=0,
                parameters=json_rpc.ByNameParameters({"type": 2, "message": "derp"}),
            ),
        )


class ApiTestCase(testslide.TestCase, abc.ABC):
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
        request_id: int = server_setup.DEFAULT_REQUEST_ID,
    ) -> Callable[[str], None]:
        return lambda actual_json_string: self._assert_json_equal(
            actual_json_string,
            server_setup.success_response_json(
                result=result,
                request_id=request_id,
            ),
        )

    def _expect_diagnostics(
        self,
        uri: str,
        diagnostics: List[lsp.Diagnostic],
    ) -> Callable[[str], None]:
        def expectation(actual_json_string: str) -> None:
            actual_output = json.loads(actual_json_string)
            self.assertEqual(actual_output["method"], "textDocument/publishDiagnostics")
            parameters = actual_output["params"]
            self.assertEqual(parameters["uri"], uri)
            self.assertEqual(
                parameters["diagnostics"],
                [diagnostic.to_dict() for diagnostic in diagnostics],
            )

        return expectation

    def _expect_telemetry_event(
        self,
        operation: str,
        result: Optional[object],
        additional_keys: Optional[Dict[str, object]] = None,
    ) -> Callable[[str], None]:
        """
        operation -  to compare the `operation` key with
        result - to compare the `response` key with
        additional_keys - specify these to test specific keys in the recorded telemetry json
        """

        def expectation(actual_json_string: str) -> None:
            actual_telemetry = json.loads(actual_json_string)
            self.assertEqual(actual_telemetry["method"], "telemetry/event")
            telemetry_params = actual_telemetry["params"]
            self.assertEqual(telemetry_params["operation"], operation)
            if result is not None:
                self.assertEqual(telemetry_params["response"], result)
            if additional_keys:
                for key, expected in additional_keys.items():
                    self.assertEqual(telemetry_params[key], expected)

        return expectation

    def _assert_output_messages(
        self,
        output_writer: connections.MemoryBytesWriter,
        expectations: List[Callable[[str], None]],
    ) -> None:
        self.assertEqual(
            len(output_writer.items()),
            len(expectations),
        )
        for raw_message, expectation in zip(output_writer.items(), expectations):
            json_string = server_setup.extract_json_from_json_rpc_message(raw_message)
            expectation(json_string)


class SaveAndOpenTest(ApiTestCase):
    @setup.async_test
    async def test_save_adds_path_to_queue(self) -> None:
        test_path = Path("/root/test.py")
        api = server_setup.create_pyre_language_server_api(
            output_channel=connections.create_memory_text_writer(),
            server_state=state.ServerState(
                server_options=server_setup.mock_initial_server_options,
                opened_documents={
                    test_path: state.OpenedDocumentState(
                        code=server_setup.DEFAULT_FILE_CONTENTS
                    )
                },
            ),
            daemon_querier=server_setup.MockDaemonQuerier(),
        )
        await api.process_did_save_request(
            lsp.DidSaveTextDocumentParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                )
            )
        )
        await asyncio.sleep(0)

    @setup.async_test
    async def test_open_close(self) -> None:
        server_state = server_setup.mock_server_state
        api = server_setup.create_pyre_language_server_api(
            output_channel=connections.create_memory_text_writer(),
            server_state=server_state,
            daemon_querier=server_setup.MockDaemonQuerier(),
        )
        test_path0 = Path("/foo/bar")
        test_path1 = Path("/foo/baz")

        await api.process_open_request(
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

        await api.process_open_request(
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

        await api.process_close_request(
            lsp.DidCloseTextDocumentParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(test_path0).unparse()
                )
            )
        )
        self.assertNotIn(test_path0, server_state.opened_documents)


class TypeCoverageTest(ApiTestCase):
    @setup.async_test
    async def test_type_coverage_request(self) -> None:
        test_path = Path("/foo")
        output_writer = connections.MemoryBytesWriter()
        querier = server_setup.MockDaemonQuerier(
            mock_type_coverage=lsp.TypeCoverageResponse(
                covered_percent=42.42,
                uncovered_ranges=[],
                default_message="pyre is on fire",
            )
        )
        api = server_setup.create_pyre_language_server_api(
            output_channel=connections.AsyncTextWriter(output_writer),
            server_state=server_setup.mock_server_state,
            daemon_querier=querier,
        )
        await api.process_type_coverage_request(
            lsp.TypeCoverageParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                )
            ),
            request_id=1,
        )
        self.assertEqual(
            querier.requests,
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
        output_writer = connections.MemoryBytesWriter()
        querier = server_setup.MockDaemonQuerier(mock_type_coverage=None)
        api = server_setup.create_pyre_language_server_api(
            output_channel=connections.AsyncTextWriter(output_writer),
            server_state=server_setup.mock_server_state,
            daemon_querier=querier,
        )
        await api.process_type_coverage_request(
            lsp.TypeCoverageParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(test_path).unparse(),
                )
            ),
            request_id=1,
        )
        self.assertEqual(
            querier.requests,
            [{"path": test_path}],
        )
        self.assertEqual(output_writer.items(), [])


class DidChangeTest(ApiTestCase):
    @setup.async_test
    async def test_did_change__basic(self) -> None:
        tracked_path = Path("/tracked.py")
        for telemetry in (
            features.TelemetryAvailability.ENABLED,
            features.TelemetryAvailability.DISABLED,
        ):
            querier = server_setup.MockDaemonQuerier()
            setup = server_setup.create_pyre_language_server_api_setup(
                opened_documents={
                    tracked_path: state.OpenedDocumentState(
                        code=server_setup.DEFAULT_FILE_CONTENTS
                    )
                },
                querier=querier,
                server_options=server_setup.create_server_options(
                    language_server_features=features.LanguageServerFeatures(
                        telemetry=telemetry
                    ),
                ),
            )
            api = setup.api
            output_writer = setup.output_writer
            await api.process_did_change_request(
                parameters=lsp.DidChangeTextDocumentParameters(
                    text_document=lsp.TextDocumentIdentifier(
                        uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                    ),
                    content_changes=[lsp.ContentChange(text="reveal_type(1)")],
                ),
            )
            # When unsaved changes is not enabled, we should send no requests.
            self.assertEqual(
                querier.requests,
                [],
            )
            if telemetry.is_enabled():
                expectations = [
                    self._expect_telemetry_event(
                        operation="didChange",
                        result=None,
                    ),
                ]
            else:
                expectations = []
            self._assert_output_messages(
                output_writer,
                expectations,
            )

    @setup.async_test
    async def test_did_change__with_type_errors(self) -> None:
        unsaved_file_content = "# some example code"
        tracked_path = Path("/tracked.py")
        for telemetry in (
            features.TelemetryAvailability.ENABLED,
            features.TelemetryAvailability.DISABLED,
        ):
            querier = server_setup.MockDaemonQuerier(
                mock_type_errors={
                    tracked_path: [
                        error.Error(
                            line=1,
                            column=1,
                            stop_line=2,
                            stop_column=2,
                            path=Path("/tracked.py"),
                            code=42,
                            name="name",
                            description="description",
                        ),
                    ]
                }
            )
            setup = server_setup.create_pyre_language_server_api_setup(
                opened_documents={
                    tracked_path: state.OpenedDocumentState(
                        code=unsaved_file_content,
                    )
                },
                querier=querier,
                server_options=server_setup.create_server_options(
                    language_server_features=features.LanguageServerFeatures(
                        unsaved_changes=features.UnsavedChangesAvailability.ENABLED,
                        telemetry=telemetry,
                    ),
                ),
            )
            api = setup.api
            output_writer = setup.output_writer
            await api.process_did_change_request(
                parameters=lsp.DidChangeTextDocumentParameters(
                    text_document=lsp.TextDocumentIdentifier(
                        uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                    ),
                    content_changes=[lsp.ContentChange(text=unsaved_file_content)],
                ),
            )
            # When unsaved changes is not enabled, we should send no requests.
            self.assertEqual(
                querier.requests,
                [
                    {"path": tracked_path, "code": unsaved_file_content},
                ],
            )
            expect_diagnostics = self._expect_diagnostics(
                uri="file:///tracked.py",
                diagnostics=[
                    lsp.Diagnostic(
                        range=lsp.LspRange(
                            start=lsp.LspPosition(line=0, character=1),
                            end=lsp.LspPosition(line=1, character=2),
                        ),
                        message="description",
                        severity=lsp.DiagnosticSeverity.ERROR,
                        code=None,
                        source="Pyre",
                    )
                ],
            )
            if telemetry.is_enabled():
                expectations = [
                    self._expect_telemetry_event(
                        operation="didChange",
                        result=None,
                    ),
                    expect_diagnostics,
                    self._expect_telemetry_event(
                        operation="typeErrors",
                        result=None,
                    ),
                ]
            else:
                expectations = [expect_diagnostics]
            self._assert_output_messages(
                output_writer,
                expectations,
            )

    @setup.async_test
    async def test_did_change__no_type_errors(self) -> None:
        tracked_path = Path("/tracked.py")
        for telemetry in (
            features.TelemetryAvailability.ENABLED,
            features.TelemetryAvailability.DISABLED,
        ):
            querier = server_setup.MockDaemonQuerier(
                mock_type_errors={},
            )
            setup = server_setup.create_pyre_language_server_api_setup(
                opened_documents={
                    tracked_path: state.OpenedDocumentState(
                        code=server_setup.DEFAULT_FILE_CONTENTS
                    )
                },
                querier=querier,
                server_options=server_setup.create_server_options(
                    language_server_features=features.LanguageServerFeatures(
                        unsaved_changes=features.UnsavedChangesAvailability.ENABLED,
                        telemetry=telemetry,
                    ),
                ),
            )
            api = setup.api
            output_writer = setup.output_writer
            await api.process_did_change_request(
                parameters=lsp.DidChangeTextDocumentParameters(
                    text_document=lsp.TextDocumentIdentifier(
                        uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                    ),
                    content_changes=[
                        lsp.ContentChange(text=server_setup.DEFAULT_FILE_CONTENTS)
                    ],
                ),
            )
            # When unsaved changes is not enabled, we should send no requests.
            self.assertEqual(
                querier.requests,
                [
                    {"path": tracked_path, "code": server_setup.DEFAULT_FILE_CONTENTS},
                ],
            )
            expect_diagnostics = self._expect_diagnostics(
                uri="file:///tracked.py",
                diagnostics=[],
            )
            if telemetry.is_enabled():
                expectations = [
                    self._expect_telemetry_event(
                        operation="didChange",
                        result=None,
                    ),
                    expect_diagnostics,
                    self._expect_telemetry_event(
                        operation="typeErrors",
                        result=None,
                    ),
                ]
            else:
                expectations = [expect_diagnostics]
            self._assert_output_messages(
                output_writer,
                expectations,
            )

    @setup.async_test
    async def test_did_change__debounce(self) -> None:
        unsaved_file_content_list = [f"# some example code {i}" for i in range(3)]
        tracked_path = Path("/tracked.py")
        querier = server_setup.MockDaemonQuerier(
            mock_type_errors={
                tracked_path: [
                    error.Error(
                        line=1,
                        column=1,
                        stop_line=2,
                        stop_column=2,
                        path=Path("/tracked.py"),
                        code=42,
                        name="name",
                        description="description",
                    ),
                ]
            }
        )
        setup = server_setup.create_pyre_language_server_api_setup(
            opened_documents={
                tracked_path: state.OpenedDocumentState(
                    code=unsaved_file_content_list[0],
                )
            },
            querier=querier,
            server_options=server_setup.create_server_options(
                language_server_features=features.LanguageServerFeatures(
                    unsaved_changes=features.UnsavedChangesAvailability.ENABLED,
                    type_errors=features.TypeErrorsAvailability.ENABLED,
                    telemetry=features.TelemetryAvailability.ENABLED,
                ),
            ),
        )
        api = setup.api
        output_writer = setup.output_writer
        for i in range(len(unsaved_file_content_list)):
            await api.process_did_change_request(
                parameters=lsp.DidChangeTextDocumentParameters(
                    text_document=lsp.TextDocumentIdentifier(
                        uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                    ),
                    content_changes=[
                        lsp.ContentChange(text=unsaved_file_content_list[i])
                    ],
                ),
            )
            setup.server_state.status_tracker.set_status(
                state.ConnectionStatus.INCREMENTAL_CHECK
            )
        self.assertEqual(
            querier.requests,
            [
                # we expect to be able to send only the first request, since the
                # remaining requests will put the server in a non-ready state
                {"path": tracked_path, "code": unsaved_file_content_list[0]}
            ],
        )
        expect_diagnostics = self._expect_diagnostics(
            uri="file:///tracked.py",
            diagnostics=[
                lsp.Diagnostic(
                    range=lsp.LspRange(
                        start=lsp.LspPosition(line=0, character=1),
                        end=lsp.LspPosition(line=1, character=2),
                    ),
                    message="description",
                    severity=lsp.DiagnosticSeverity.ERROR,
                    code=None,
                    source="Pyre",
                )
            ],
        )
        expectations = [
            self._expect_telemetry_event(
                operation="didChange",
                result=None,
            ),
            expect_diagnostics,
            self._expect_telemetry_event(
                operation="typeErrors",
                result=None,
            ),
            self._expect_telemetry_event(
                operation="didChange",
                result=None,
            ),
            self._expect_telemetry_event(
                operation="didChange",
                result=None,
            ),
        ]
        self._assert_output_messages(
            output_writer,
            expectations,
        )


class HoverTest(ApiTestCase):
    @setup.async_test
    async def test_hover__basic(self) -> None:
        tracked_path = Path("/tracked.py")
        lsp_line = 3
        daemon_line = lsp_line + 1
        expected_response = lsp.LspHoverResponse(
            contents=server_setup.DEFAULT_FILE_CONTENTS
        )
        for telemetry in (
            features.TelemetryAvailability.ENABLED,
            features.TelemetryAvailability.DISABLED,
        ):
            querier = server_setup.MockDaemonQuerier(
                mock_hover_response=daemon_querier.GetHoverResponse(
                    source=daemon_querier.DaemonQuerierSource.PYRE_DAEMON,
                    data=expected_response,
                )
            )
            setup = server_setup.create_pyre_language_server_api_setup(
                opened_documents={
                    tracked_path: state.OpenedDocumentState(
                        code=server_setup.DEFAULT_FILE_CONTENTS
                    )
                },
                querier=querier,
                index_querier=querier,
                server_options=server_setup.create_server_options(
                    language_server_features=features.LanguageServerFeatures(
                        telemetry=telemetry
                    ),
                ),
            )
            api = setup.api
            output_writer = setup.output_writer
            await api.process_hover_request(
                parameters=lsp.HoverParameters(
                    text_document=lsp.TextDocumentIdentifier(
                        uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                    ),
                    position=lsp.LspPosition(line=lsp_line, character=4),
                ),
                request_id=server_setup.DEFAULT_REQUEST_ID,
            )
            self.assertEqual(
                querier.requests,
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
            if telemetry.is_enabled():
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
    async def test_hover__with_overlays(self) -> None:
        tracked_path = Path("/tracked.py")
        lsp_line = 3
        daemon_line = lsp_line + 1
        expected_response = lsp.LspHoverResponse(
            contents=server_setup.DEFAULT_FILE_CONTENTS
        )
        for telemetry in (
            features.TelemetryAvailability.ENABLED,
            features.TelemetryAvailability.DISABLED,
        ):
            querier = server_setup.MockDaemonQuerier(
                mock_hover_response=daemon_querier.GetHoverResponse(
                    source=daemon_querier.DaemonQuerierSource.PYRE_DAEMON,
                    data=expected_response,
                ),
            )
            setup = server_setup.create_pyre_language_server_api_setup(
                opened_documents={
                    tracked_path: state.OpenedDocumentState(
                        code=server_setup.DEFAULT_FILE_CONTENTS
                    )
                },
                querier=querier,
                index_querier=querier,
                server_options=server_setup.create_server_options(
                    language_server_features=features.LanguageServerFeatures(
                        unsaved_changes=features.UnsavedChangesAvailability.ENABLED,
                        telemetry=telemetry,
                    ),
                ),
            )
            api = setup.api
            output_writer = setup.output_writer
            await api.process_hover_request(
                parameters=lsp.HoverParameters(
                    text_document=lsp.TextDocumentIdentifier(
                        uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                    ),
                    position=lsp.LspPosition(line=lsp_line, character=4),
                ),
                request_id=server_setup.DEFAULT_REQUEST_ID,
            )
            self.assertEqual(
                querier.requests,
                [
                    {"path": tracked_path, "code": server_setup.DEFAULT_FILE_CONTENTS},
                    {
                        "path": tracked_path,
                        "position": lsp.PyrePosition(line=daemon_line, character=4),
                    },
                ],
            )
            raw_expected_response = lsp.LspHoverResponse.cached_schema().dump(
                expected_response
            )
            expect_correct_response = self._expect_success_message(
                raw_expected_response
            )
            if telemetry.is_enabled():
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
        querier = server_setup.MockDaemonQuerier()
        setup = server_setup.create_pyre_language_server_api_setup(
            opened_documents={
                tracked_path: state.OpenedDocumentState(
                    code=server_setup.DEFAULT_FILE_CONTENTS
                )
            },
            querier=querier,
        )
        api = setup.api
        output_writer = setup.output_writer
        await api.process_hover_request(
            parameters=lsp.HoverParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(untracked_path).unparse(),
                ),
                position=lsp.LspPosition(line=lsp_line, character=4),
            ),
            request_id=server_setup.DEFAULT_REQUEST_ID,
        )
        self.assertEqual(querier.requests, [])
        self._assert_output_messages(
            output_writer,
            [
                self._expect_success_message(
                    result=None,
                )
            ],
        )


class DefinitionTest(ApiTestCase):
    @setup.async_test
    async def test_definition__basic(self) -> None:
        tracked_path = Path("/tracked.py")
        lsp_line = 3
        daemon_line = lsp_line + 1
        expected_response = daemon_querier.GetDefinitionLocationsResponse(
            source=daemon_querier.DaemonQuerierSource.PYRE_DAEMON,
            data=[
                lsp.LspLocation(
                    uri="file:///path/to/foo.py",
                    range=lsp.LspRange(
                        start=lsp.LspPosition(line=5, character=6),
                        end=lsp.LspPosition(line=5, character=9),
                    ),
                )
            ],
        )
        for telemetry in (
            features.TelemetryAvailability.ENABLED,
            features.TelemetryAvailability.DISABLED,
        ):
            querier = server_setup.MockDaemonQuerier(
                mock_definition_response=expected_response,
            )
            setup = server_setup.create_pyre_language_server_api_setup(
                opened_documents={
                    tracked_path: state.OpenedDocumentState(
                        code=server_setup.DEFAULT_FILE_CONTENTS
                    )
                },
                querier=querier,
                index_querier=querier,
                server_options=server_setup.create_server_options(
                    language_server_features=features.LanguageServerFeatures(
                        telemetry=telemetry
                    ),
                ),
            )
            api = setup.api
            output_writer = setup.output_writer
            await api.process_definition_request(
                parameters=lsp.DefinitionParameters(
                    text_document=lsp.TextDocumentIdentifier(
                        uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                    ),
                    position=lsp.LspPosition(line=lsp_line, character=4),
                ),
                request_id=server_setup.DEFAULT_REQUEST_ID,
            )
            self.assertEqual(
                querier.requests,
                [
                    {
                        "path": tracked_path,
                        "position": lsp.PyrePosition(line=daemon_line, character=4),
                    },
                ],
            )
            raw_expected_response = lsp.LspLocation.cached_schema().dump(
                expected_response.data, many=True
            )
            expect_correct_response = self._expect_success_message(
                raw_expected_response
            )
            if telemetry.is_enabled():
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
    async def test_definition__with_overlays(self) -> None:
        tracked_path = Path("/tracked.py")
        lsp_line = 3
        daemon_line = lsp_line + 1
        expected_response = daemon_querier.GetDefinitionLocationsResponse(
            source=daemon_querier.DaemonQuerierSource.PYRE_DAEMON,
            data=[
                lsp.LspLocation(
                    uri="file:///path/to/foo.py",
                    range=lsp.LspRange(
                        start=lsp.LspPosition(line=5, character=6),
                        end=lsp.LspPosition(line=5, character=9),
                    ),
                )
            ],
        )
        for telemetry in (
            features.TelemetryAvailability.ENABLED,
            features.TelemetryAvailability.DISABLED,
        ):
            querier = server_setup.MockDaemonQuerier(
                mock_definition_response=expected_response,
            )
            setup = server_setup.create_pyre_language_server_api_setup(
                opened_documents={
                    tracked_path: state.OpenedDocumentState(
                        code=server_setup.DEFAULT_FILE_CONTENTS
                    )
                },
                querier=querier,
                index_querier=querier,
                server_options=server_setup.create_server_options(
                    language_server_features=features.LanguageServerFeatures(
                        unsaved_changes=features.UnsavedChangesAvailability.ENABLED,
                        telemetry=telemetry,
                    ),
                ),
            )
            api = setup.api
            output_writer = setup.output_writer
            await api.process_definition_request(
                parameters=lsp.DefinitionParameters(
                    text_document=lsp.TextDocumentIdentifier(
                        uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                    ),
                    position=lsp.LspPosition(line=lsp_line, character=4),
                ),
                request_id=server_setup.DEFAULT_REQUEST_ID,
            )
            self.assertEqual(
                querier.requests,
                [
                    {"path": tracked_path, "code": server_setup.DEFAULT_FILE_CONTENTS},
                    {
                        "path": tracked_path,
                        "position": lsp.PyrePosition(line=daemon_line, character=4),
                    },
                ],
            )
            raw_expected_response = lsp.LspLocation.cached_schema().dump(
                expected_response.data, many=True
            )
            expect_correct_response = self._expect_success_message(
                raw_expected_response
            )
            if telemetry.is_enabled():
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
        expected_telemetry_response = daemon_querier.GetDefinitionLocationsResponse(
            source=daemon_querier.DaemonQuerierSource.PYRE_DAEMON,
            data=[
                lsp.LspLocation(
                    uri="file:///path/to/foo.py",
                    range=lsp.LspRange(
                        start=lsp.LspPosition(line=5, character=6),
                        end=lsp.LspPosition(line=5, character=9),
                    ),
                )
            ],
        )
        querier = server_setup.MockDaemonQuerier(
            mock_definition_response=expected_telemetry_response,
        )
        setup = server_setup.create_pyre_language_server_api_setup(
            opened_documents={
                tracked_path: state.OpenedDocumentState(
                    code=server_setup.DEFAULT_FILE_CONTENTS
                )
            },
            querier=querier,
            index_querier=querier,
            server_options=server_setup.create_server_options(
                language_server_features=features.LanguageServerFeatures(
                    definition=features.DefinitionAvailability.SHADOW,
                    telemetry=features.TelemetryAvailability.ENABLED,
                ),
            ),
        )
        api = setup.api
        output_writer = setup.output_writer
        await api.process_definition_request(
            parameters=lsp.DefinitionParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                ),
                position=lsp.LspPosition(line=lsp_line, character=4),
            ),
            request_id=server_setup.DEFAULT_REQUEST_ID,
        )
        self.assertEqual(
            querier.requests,
            [
                {
                    "path": tracked_path,
                    "position": lsp.PyrePosition(line=daemon_line, character=4),
                },
            ],
        )
        self._assert_output_messages(
            output_writer,
            [
                self._expect_success_message(
                    lsp.LspLocation.cached_schema().dump(
                        expected_editor_response, many=True
                    )
                ),
                self._expect_telemetry_event(
                    operation="definition",
                    result=lsp.LspLocation.cached_schema().dump(
                        expected_telemetry_response.data,
                        many=True,
                    ),
                ),
            ],
        )

    @setup.async_test
    async def test_definition__unopened(self) -> None:
        tracked_path = Path("/tracked.py")
        untracked_path = Path("/not_tracked.py")
        lsp_line = 3
        querier = server_setup.MockDaemonQuerier()
        setup = server_setup.create_pyre_language_server_api_setup(
            opened_documents={
                tracked_path: state.OpenedDocumentState(
                    code=server_setup.DEFAULT_FILE_CONTENTS
                )
            },
            querier=querier,
            index_querier=querier,
        )
        api = setup.api
        output_writer = setup.output_writer
        await api.process_definition_request(
            parameters=lsp.DefinitionParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(untracked_path).unparse(),
                ),
                position=lsp.LspPosition(line=lsp_line, character=4),
            ),
            request_id=server_setup.DEFAULT_REQUEST_ID,
        )
        self.assertEqual(querier.requests, [])
        self._assert_output_messages(
            output_writer,
            [self._expect_success_message([])],
        )

    @setup.async_test
    async def test_definition__indexed(self) -> None:
        tracked_path = Path("/tracked.py")
        expected_editor_response = []
        expected_telemetry_response = daemon_querier.GetDefinitionLocationsResponse(
            source=daemon_querier.DaemonQuerierSource.GLEAN_INDEXER,
            data=[
                lsp.LspLocation(
                    uri="file:///path/to/foo.py",
                    range=lsp.LspRange(
                        start=lsp.LspPosition(line=5, character=6),
                        end=lsp.LspPosition(line=5, character=9),
                    ),
                )
            ],
        )
        querier = server_setup.MockDaemonQuerier(
            mock_definition_response=expected_telemetry_response,
        )
        setup = server_setup.create_pyre_language_server_api_setup(
            opened_documents={
                tracked_path: state.OpenedDocumentState(
                    code=server_setup.DEFAULT_FILE_CONTENTS
                )
            },
            querier=querier,
            index_querier=querier,
            server_options=server_setup.create_server_options(
                language_server_features=features.LanguageServerFeatures(
                    definition=features.DefinitionAvailability.SHADOW,
                    telemetry=features.TelemetryAvailability.ENABLED,
                ),
            ),
            # Pyre daemon not "READY", will serve indexed results
            connection_status=state.ConnectionStatus.STARTING,
        )
        api = setup.api
        output_writer = setup.output_writer
        await api.process_definition_request(
            parameters=lsp.DefinitionParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                ),
                position=lsp.LspPosition(line=3, character=4),
            ),
            request_id=server_setup.DEFAULT_REQUEST_ID,
        )
        self.assertEqual(
            querier.requests,
            [
                {
                    "path": tracked_path,
                    "position": lsp.PyrePosition(line=4, character=4),
                },
            ],
        )
        self._assert_output_messages(
            output_writer,
            [
                self._expect_success_message(
                    lsp.LspLocation.cached_schema().dump(
                        expected_editor_response, many=True
                    )
                ),
                self._expect_telemetry_event(
                    operation="definition",
                    result=lsp.LspLocation.cached_schema().dump(
                        expected_telemetry_response.data,
                        many=True,
                    ),
                ),
            ],
        )


class ReferencesTest(ApiTestCase):
    @setup.async_test
    async def test_references__basic(self) -> None:
        tracked_path = Path("/tracked.py")
        lsp_line = 3
        daemon_line = lsp_line + 1
        expected_response = [
            lsp.LspLocation(
                uri="file:///path/to/foo.py",
                range=lsp.LspRange(
                    start=lsp.LspPosition(line=5, character=6),
                    end=lsp.LspPosition(line=5, character=9),
                ),
            )
        ]
        querier = server_setup.MockDaemonQuerier(
            mock_references_response=expected_response,
        )
        setup = server_setup.create_pyre_language_server_api_setup(
            opened_documents={
                tracked_path: state.OpenedDocumentState(
                    code=server_setup.DEFAULT_FILE_CONTENTS
                )
            },
            querier=server_setup.MockDaemonQuerier(),
            index_querier=querier,
        )
        api = setup.api
        output_writer = setup.output_writer
        await api.process_find_all_references_request(
            lsp.ReferencesParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                ),
                position=lsp.LspPosition(line=lsp_line, character=4),
            ),
            request_id=server_setup.DEFAULT_REQUEST_ID,
        )
        self.assertEqual(
            querier.requests,
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
                    result=lsp.LspLocation.cached_schema().dump(
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
        querier = server_setup.MockDaemonQuerier()
        setup = server_setup.create_pyre_language_server_api_setup(
            opened_documents={
                tracked_path: state.OpenedDocumentState(
                    code=server_setup.DEFAULT_FILE_CONTENTS
                )
            },
            querier=querier,
            index_querier=querier,
        )
        api = setup.api
        output_writer = setup.output_writer
        await api.process_find_all_references_request(
            lsp.ReferencesParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(untracked_path).unparse(),
                ),
                position=lsp.LspPosition(line=lsp_line, character=4),
            ),
            request_id=server_setup.DEFAULT_REQUEST_ID,
        )
        self.assertEqual(querier.requests, [])
        self._assert_output_messages(
            output_writer, [self._expect_success_message(result=[])]
        )


class CallHierarchyTest(ApiTestCase):
    @setup.async_test
    async def test_call_hierarchy__basic(self) -> None:
        tracked_path = Path("/tracked.py")
        lsp_line = 3
        daemon_line = lsp_line + 1
        range = lsp.LspRange(
            lsp.LspPosition(line=lsp_line, character=0),
            lsp.LspPosition(line=lsp_line, character=8),
        )
        call_hierarchy_item = lsp.CallHierarchyItem(
            name="testname",
            kind=lsp.SymbolKind.METHOD,
            uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
            range=range,
            selection_range=range,
        )
        expected_response = [call_hierarchy_item]
        index_querier = server_setup.MockDaemonQuerier(
            mock_call_hierarchy_response=expected_response,
        )
        setup = server_setup.create_pyre_language_server_api_setup(
            opened_documents={
                tracked_path: state.OpenedDocumentState(
                    code=server_setup.DEFAULT_FILE_CONTENTS
                )
            },
            querier=server_setup.MockDaemonQuerier(),
            index_querier=index_querier,
        )
        api = setup.api
        output_writer = setup.output_writer
        await api.process_call_hierarchy_request(
            lsp.CallHierarchyParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                ),
                position=lsp.LspPosition(line=lsp_line, character=4),
            ),
            request_id=server_setup.DEFAULT_REQUEST_ID,
        )
        await api.process_call_hierarchy_incoming_call(
            parameters=lsp.CallHierarchyIncomingCallParameters(
                item=call_hierarchy_item
            ),
            request_id=server_setup.DEFAULT_REQUEST_ID,
        )
        await api.process_call_hierarchy_outgoing_call(
            parameters=lsp.CallHierarchyOutgoingCallParameters(
                item=call_hierarchy_item
            ),
            request_id=server_setup.DEFAULT_REQUEST_ID,
        )
        self.assertEqual(
            index_querier.requests,
            [
                {
                    "path": tracked_path,
                    "position": lsp.PyrePosition(line=daemon_line, character=4),
                    "relation_direction": lsp.PyreCallHierarchyRelationDirection.PARENT,
                },
                {
                    "path": tracked_path,
                    "call_hierarchy_item": call_hierarchy_item,
                    "relation_direction": lsp.PyreCallHierarchyRelationDirection.PARENT,
                },
                {
                    "path": tracked_path,
                    "call_hierarchy_item": call_hierarchy_item,
                    "relation_direction": lsp.PyreCallHierarchyRelationDirection.CHILD,
                },
            ],
        )
        self._assert_output_messages(
            output_writer,
            [
                self._expect_success_message(
                    result=lsp.CallHierarchyItem.cached_schema().dump(
                        expected_response, many=True
                    ),
                ),
                self._expect_success_message(
                    result=[
                        lsp.CallHierarchyIncomingCall.cached_schema().dump(
                            lsp.CallHierarchyIncomingCall(
                                from_=call_hierarchy_item,
                                from_ranges=[call_hierarchy_item.range],
                            )
                        )
                    ],
                ),
                self._expect_success_message(
                    result=[
                        lsp.CallHierarchyOutgoingCall.cached_schema().dump(
                            lsp.CallHierarchyOutgoingCall(
                                to=call_hierarchy_item,
                                from_ranges=[call_hierarchy_item.range],
                            )
                        )
                    ],
                ),
            ],
        )


class DocumentSymbolsTest(ApiTestCase):
    @setup.async_test
    async def test_document_symbols_request(self) -> None:
        self.maxDiff = None
        with tempfile.NamedTemporaryFile(suffix=".py") as temporary_file:
            temporary_file.write(b"def foo(x):\n  pass\n")
            temporary_file.flush()
            test_path = Path(temporary_file.name)
            setup = server_setup.create_pyre_language_server_api_setup(
                opened_documents={
                    test_path: state.OpenedDocumentState(
                        code=server_setup.DEFAULT_FILE_CONTENTS
                    )
                },
                querier=server_setup.MockDaemonQuerier(),
            )
            api = setup.api
            output_writer = setup.output_writer
            await api.process_document_symbols_request(
                lsp.DocumentSymbolsParameters(
                    text_document=lsp.TextDocumentIdentifier(uri=test_path.as_uri())
                ),
                request_id=server_setup.DEFAULT_REQUEST_ID,
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
                                "kind": lsp.SymbolKind.FUNCTION.value,
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


class RenameSymbolTest(ApiTestCase):
    def setUp(self) -> None:
        self.maxDiff = None

    @setup.async_test
    async def test_rename_symbol_request(self) -> None:
        test_code = """
foo(10)
foo(20)
"""
        tracked_path = Path("/global/root/path/to/foo.py")
        lsp_line = 1
        daemon_line = lsp_line + 1
        test_uri = "file://" + str(tracked_path)
        test_range1 = lsp.LspRange(
            start=lsp.LspPosition(line=1, character=0),
            end=lsp.LspPosition(line=1, character=3),
        )
        test_range2 = lsp.LspRange(
            start=lsp.LspPosition(line=2, character=0),
            end=lsp.LspPosition(line=2, character=3),
        )
        test_text = "test"
        local_reference_response = [
            lsp.LspLocation(
                uri=test_uri,
                range=test_range1,
            )
        ]
        global_reference_response = [
            lsp.LspLocation(
                uri=test_uri,
                range=test_range2,
            )
        ]
        workspace_edit_response = {
            "changes": {
                test_uri: [
                    lsp.TextEdit(
                        range=test_range1,
                        new_text=test_text,
                    ),
                    lsp.TextEdit(
                        range=test_range2,
                        new_text=test_text,
                    ),
                ],
            }
        }
        querier = server_setup.MockDaemonQuerier(
            mock_references_response=local_reference_response,
        )
        index_querier = server_setup.MockDaemonQuerier(
            mock_references_response=global_reference_response,
        )
        setup = server_setup.create_pyre_language_server_api_setup(
            opened_documents={tracked_path: state.OpenedDocumentState(code=test_code)},
            querier=querier,
            index_querier=index_querier,
        )
        api = setup.api
        output_writer = setup.output_writer
        await api.process_rename_request(
            lsp.RenameParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                ),
                position=lsp.LspPosition(line=lsp_line, character=2),
                new_name=test_text,
            ),
            request_id=server_setup.DEFAULT_REQUEST_ID,
        )
        self.assertEqual(
            querier.requests,
            [
                {
                    "path": tracked_path,
                    "position": lsp.PyrePosition(line=daemon_line, character=2),
                }
            ],
        )
        self._assert_output_messages(
            output_writer,
            [
                self._expect_success_message(
                    result=lsp.WorkspaceEdit.cached_schema().dump(
                        workspace_edit_response
                    ),
                )
            ],
        )

    @setup.async_test
    async def test_rename_symbol_request_dedupes_references(self) -> None:
        test_code = """
foo()
"""
        tracked_path = Path("/global/root/path/to/foo.py")
        lsp_line = 1
        daemon_line = lsp_line + 1
        test_uri = "file://" + str(tracked_path)
        test_range = lsp.LspRange(
            start=lsp.LspPosition(line=1, character=0),
            end=lsp.LspPosition(line=1, character=3),
        )
        test_text = "test"
        reference_response = [
            lsp.LspLocation(
                uri=test_uri,
                range=test_range,
            )
        ]
        workspace_edit_response = {
            "changes": {
                test_uri: [
                    lsp.TextEdit(
                        range=test_range,
                        new_text=test_text,
                    ),
                ],
            }
        }
        querier = server_setup.MockDaemonQuerier(
            mock_references_response=reference_response,
        )
        index_querier = server_setup.MockDaemonQuerier(
            mock_references_response=reference_response,
        )
        setup = server_setup.create_pyre_language_server_api_setup(
            opened_documents={tracked_path: state.OpenedDocumentState(code=test_code)},
            querier=querier,
            index_querier=index_querier,
        )
        api = setup.api
        output_writer = setup.output_writer
        await api.process_rename_request(
            lsp.RenameParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(tracked_path).unparse(),
                ),
                position=lsp.LspPosition(line=lsp_line, character=2),
                new_name=test_text,
            ),
            request_id=server_setup.DEFAULT_REQUEST_ID,
        )
        self.assertEqual(
            querier.requests,
            [
                {
                    "path": tracked_path,
                    "position": lsp.PyrePosition(line=daemon_line, character=2),
                }
            ],
        )
        self._assert_output_messages(
            output_writer,
            [
                self._expect_success_message(
                    result=lsp.WorkspaceEdit.cached_schema().dump(
                        workspace_edit_response
                    ),
                )
            ],
        )

    @setup.async_test
    async def test_rename_symbol_filters_incorrect_references(self) -> None:
        local_test_code = """
def foo(x:int) -> None:
    print(x)

foo(10)
"""
        local_test_range_1 = lsp.LspRange(
            start=lsp.LspPosition(line=1, character=4),
            end=lsp.LspPosition(line=1, character=7),
        )
        local_test_range_2 = lsp.LspRange(
            start=lsp.LspPosition(line=4, character=0),
            end=lsp.LspPosition(line=4, character=3),
        )
        test_text = "test"
        querier = server_setup.MockDaemonQuerier(
            mock_references_response=[
                lsp.LspLocation(
                    uri="file:///global/root/path/to/foo.py",
                    range=local_test_range_1,
                ),
                lsp.LspLocation(
                    uri="file:///global/root/path/to/foo.py",
                    range=local_test_range_2,
                ),
            ],
        )
        index_querier = server_setup.MockDaemonQuerier(
            mock_references_response=[
                lsp.LspLocation(
                    uri="file:///global/root/path/to/bar.py",
                    range=lsp.LspRange(
                        start=lsp.LspPosition(line=0, character=0),
                        end=lsp.LspPosition(line=0, character=3),
                    ),
                )
            ],
        )
        setup = server_setup.create_pyre_language_server_api_setup(
            opened_documents={
                Path("/global/root/path/to/foo.py"): state.OpenedDocumentState(
                    code=local_test_code
                ),
                Path("/global/root/path/to/bar.py"): state.OpenedDocumentState(
                    code="bar"
                ),
            },
            querier=querier,
            index_querier=index_querier,
        )
        api = setup.api
        output_writer = setup.output_writer
        await api.process_rename_request(
            lsp.RenameParameters(
                text_document=lsp.TextDocumentIdentifier(
                    uri=lsp.DocumentUri.from_file_path(
                        Path("/global/root/path/to/foo.py")
                    ).unparse(),
                ),
                position=lsp.LspPosition(line=4, character=2),
                new_name=test_text,
            ),
            request_id=server_setup.DEFAULT_REQUEST_ID,
        )
        self.assertEqual(
            querier.requests,
            [
                {
                    "path": Path("/global/root/path/to/foo.py"),
                    "position": lsp.PyrePosition(line=5, character=2),
                }
            ],
        )

        expected = server_setup.success_response_json(
            result=lsp.WorkspaceEdit.cached_schema().dump(
                {
                    "changes": {
                        "file:///global/root/path/to/foo.py": [
                            lsp.TextEdit(
                                range=local_test_range_1,
                                new_text=test_text,
                            ),
                            lsp.TextEdit(
                                range=local_test_range_2,
                                new_text=test_text,
                            ),
                        ],
                    }
                }
            ),
        )
        actual = server_setup.extract_json_from_json_rpc_message(
            output_writer.items()[0]
        )
        self._assert_json_equal(actual, expected)
