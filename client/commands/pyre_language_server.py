# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module is responsible for handling requests from the VScode language server and generating an appropriate response.

The response typically will be generated through the Pyre daemon, and the name PyreLanguageServer was chosen for this module
because it illustrates that this is the intermediary between the Language server and the Pyre daemon.
"""


import dataclasses
import logging
import time
from pathlib import Path
from typing import Dict, List, Optional, Union

from .. import json_rpc, log

from . import (
    background,
    commands,
    connections,
    find_symbols,
    language_server_features as features,
    language_server_protocol as lsp,
    request_handler,
    server_state as state,
)

LOG: logging.Logger = logging.getLogger(__name__)
CONSECUTIVE_START_ATTEMPT_THRESHOLD: int = 5


async def read_lsp_request(
    input_channel: connections.AsyncTextReader,
    output_channel: connections.AsyncTextWriter,
) -> json_rpc.Request:
    while True:
        try:
            message = await lsp.read_json_rpc(input_channel)
            return message
        except json_rpc.JSONRPCException as json_rpc_error:
            LOG.debug(f"Exception occurred while reading JSON RPC: {json_rpc_error}")
            await lsp.write_json_rpc_ignore_connection_error(
                output_channel,
                json_rpc.ErrorResponse(
                    id=None,
                    code=json_rpc_error.error_code(),
                    message=str(json_rpc_error),
                ),
            )


def duration_ms(
    start_time: float,
    end_time: float,
) -> int:
    return int(1000 * (end_time - start_time))


async def _wait_for_exit(
    input_channel: connections.AsyncTextReader,
    output_channel: connections.AsyncTextWriter,
) -> None:
    """
    Wait for an LSP "exit" request from the `input_channel`. This is mostly useful
    when the LSP server has received a "shutdown" request, in which case the LSP
    specification dictates that only "exit" can be sent from the client side.

    If a non-exit LSP request is received, drop it and keep waiting on another
    "exit" request.
    """
    while True:
        request = await read_lsp_request(input_channel, output_channel)
        if request.method != "exit":
            LOG.debug(f"Non-exit request received after shutdown: {request}")
            continue
        # Got an exit request. Stop the wait.
        return


@dataclasses.dataclass(frozen=True)
class PyreLanguageServer:
    # I/O Channels
    input_channel: connections.AsyncTextReader
    output_channel: connections.AsyncTextWriter

    # inside this task manager is a PyreDaemonLaunchAndSubscribeHandler
    pyre_manager: background.TaskManager
    # NOTE: The fields inside `server_state` are mutable and can be changed by `pyre_manager`
    server_state: state.ServerState

    handler: request_handler.AbstractRequestHandler

    async def write_telemetry(
        self,
        parameters: Dict[str, object],
        activity_key: Optional[Dict[str, object]],
    ) -> None:
        should_write_telemetry = (
            self.server_state.server_options.enabled_telemetry_event
        )
        if should_write_telemetry:
            await lsp.write_json_rpc_ignore_connection_error(
                self.output_channel,
                json_rpc.Request(
                    activity_key=activity_key,
                    method="telemetry/event",
                    parameters=json_rpc.ByNameParameters(parameters),
                ),
            )

    def get_language_server_features(self) -> features.LanguageServerFeatures:
        return self.server_state.server_options.language_server_features

    async def wait_for_exit(self) -> commands.ExitCode:
        await _wait_for_exit(self.input_channel, self.output_channel)
        return commands.ExitCode.SUCCESS

    async def _try_restart_pyre_server(self) -> None:
        if (
            self.server_state.consecutive_start_failure
            < CONSECUTIVE_START_ATTEMPT_THRESHOLD
        ):
            await self.pyre_manager.ensure_task_running()
        else:
            LOG.info(
                "Not restarting Pyre since failed consecutive start attempt limit"
                " has been reached."
            )

    async def process_open_request(
        self,
        parameters: lsp.DidOpenTextDocumentParameters,
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        self.server_state.opened_documents.add(document_path)
        LOG.info(f"File opened: {document_path}")

        # Attempt to trigger a background Pyre server start on each file open
        if not self.pyre_manager.is_task_running():
            await self._try_restart_pyre_server()

    async def process_close_request(
        self, parameters: lsp.DidCloseTextDocumentParameters
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        try:
            self.server_state.opened_documents.remove(document_path)
            LOG.info(f"File closed: {document_path}")
        except KeyError:
            LOG.warning(f"Trying to close an un-opened file: {document_path}")

    async def process_did_change_request(
        self,
        parameters: lsp.DidChangeTextDocumentParameters,
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )

        if document_path not in self.server_state.opened_documents:
            return

        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "didChange",
                "filePath": str(document_path),
                "server_state_open_documents_count": len(
                    self.server_state.opened_documents
                ),
                "server_state_start_status": str(
                    self.server_state.server_last_status.value
                ),
            },
            activity_key,
        )

        process_unsaved_changes = (
            self.server_state.server_options.language_server_features.unsaved_changes.is_enabled()
        )

        if process_unsaved_changes:
            await self.handler.update_overlay(
                path=document_path.resolve(),
                code=str(
                    "".join(
                        [
                            content_change.text
                            for content_change in parameters.content_changes
                        ]
                    )
                ),
            )
        # Attempt to trigger a background Pyre server start on each file change
        if not self.pyre_manager.is_task_running():
            await self._try_restart_pyre_server()

    async def process_did_save_request(
        self,
        parameters: lsp.DidSaveTextDocumentParameters,
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )

        if document_path not in self.server_state.opened_documents:
            return

        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "didSave",
                "filePath": str(document_path),
                "server_state_open_documents_count": len(
                    self.server_state.opened_documents
                ),
                "server_state_start_status": str(
                    self.server_state.server_last_status.value
                ),
            },
            activity_key,
        )

        # Attempt to trigger a background Pyre server start on each file save
        if not self.pyre_manager.is_task_running():
            await self._try_restart_pyre_server()

    async def process_type_coverage_request(
        self,
        parameters: lsp.TypeCoverageParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        response = await self.handler.get_type_coverage(path=document_path)
        if response is not None:
            await lsp.write_json_rpc(
                self.output_channel,
                json_rpc.SuccessResponse(
                    id=request_id,
                    activity_key=activity_key,
                    result=response.to_dict(),
                ),
            )

    async def process_hover_request(
        self,
        parameters: lsp.HoverParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        """Always respond to a hover request even for non-tracked paths.

        Otherwise, VS Code hover will wait for Pyre until it times out, meaning
        that messages from other hover providers will be delayed."""

        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )

        if document_path not in self.server_state.opened_documents:
            await lsp.write_json_rpc(
                self.output_channel,
                json_rpc.SuccessResponse(
                    id=request_id,
                    activity_key=activity_key,
                    result=lsp.LspHoverResponse.empty().to_dict(),
                ),
            )
        else:
            start_time = time.time()
            result = await self.handler.get_hover(
                path=document_path,
                position=parameters.position.to_pyre_position(),
            )
            raw_result = lsp.LspHoverResponse.cached_schema().dump(
                result,
            )
            await lsp.write_json_rpc(
                self.output_channel,
                json_rpc.SuccessResponse(
                    id=request_id,
                    activity_key=activity_key,
                    result=raw_result,
                ),
            )
            end_time = time.time()
            await self.write_telemetry(
                {
                    "type": "LSP",
                    "operation": "hover",
                    "filePath": str(document_path),
                    "nonEmpty": len(result.contents) > 0,
                    "response": raw_result,
                    "duration_ms": duration_ms(start_time, end_time),
                    "server_state_open_documents_count": len(
                        self.server_state.opened_documents
                    ),
                    "server_state_start_status": str(
                        self.server_state.server_last_status.value
                    ),
                },
                activity_key,
            )

    async def _get_definition_result(
        self, document_path: Path, position: lsp.LspPosition
    ) -> List[Dict[str, object]]:
        """
        Helper function to call the handler. Exists only to reduce code duplication
        due to shadow mode, please don't make more of these - we already have enough
        layers of handling.
        """
        definitions = await self.handler.get_definition_locations(
            path=document_path,
            position=position.to_pyre_position(),
        )
        return lsp.LspLocation.cached_schema().dump(
            definitions,
            many=True,
        )

    async def process_definition_request(
        self,
        parameters: lsp.DefinitionParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path: Optional[
            Path
        ] = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )

        if document_path not in self.server_state.opened_documents:
            await lsp.write_json_rpc(
                self.output_channel,
                json_rpc.SuccessResponse(
                    id=request_id,
                    activity_key=activity_key,
                    result=lsp.LspLocation.cached_schema().dump([], many=True),
                ),
            )
        else:
            start_time = time.time()
            shadow_mode = self.get_language_server_features().definition.is_shadow()
            if not shadow_mode:
                raw_result = await self._get_definition_result(
                    document_path=document_path,
                    position=parameters.position,
                )
                await lsp.write_json_rpc(
                    self.output_channel,
                    json_rpc.SuccessResponse(
                        id=request_id,
                        activity_key=activity_key,
                        result=raw_result,
                    ),
                )
            else:
                # send an empty result to the client first, then get the real
                # result so we can log it (and realistic perf) in telemetry.
                await lsp.write_json_rpc(
                    self.output_channel,
                    json_rpc.SuccessResponse(
                        id=request_id,
                        activity_key=activity_key,
                        result=lsp.LspLocation.cached_schema().dump([], many=True),
                    ),
                )
                raw_result = await self._get_definition_result(
                    document_path=document_path,
                    position=parameters.position,
                )
            end_time = time.time()
            await self.write_telemetry(
                {
                    "type": "LSP",
                    "operation": "definition",
                    "filePath": str(document_path),
                    "count": len(raw_result),
                    "response": raw_result,
                    "duration_ms": duration_ms(start_time, end_time),
                },
                activity_key,
            )

    async def process_document_symbols_request(
        self,
        parameters: lsp.DocumentSymbolsParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        if document_path not in self.server_state.opened_documents:
            raise json_rpc.InvalidRequestError(
                f"Document URI has not been opened: {parameters.text_document.uri}"
            )
        try:
            source = document_path.read_text()
            symbols = find_symbols.parse_source_and_collect_symbols(source)
            await lsp.write_json_rpc(
                self.output_channel,
                json_rpc.SuccessResponse(
                    id=request_id,
                    activity_key=activity_key,
                    result=[s.to_dict() for s in symbols],
                ),
            )
        except find_symbols.UnparseableError as error:
            raise lsp.RequestFailedError(
                f"Document URI is not parsable: {parameters.text_document.uri}"
            ) from error
        except OSError as error:
            raise lsp.RequestFailedError(
                f"Document URI is not a readable file: {parameters.text_document.uri}"
            ) from error

    async def process_find_all_references_request(
        self,
        parameters: lsp.ReferencesParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )

        if document_path not in self.server_state.opened_documents:
            await lsp.write_json_rpc(
                self.output_channel,
                json_rpc.SuccessResponse(
                    id=request_id,
                    activity_key=activity_key,
                    result=lsp.LspLocation.cached_schema().dump([], many=True),
                ),
            )
            return

        reference_locations = await self.handler.get_reference_locations(
            path=document_path,
            position=parameters.position.to_pyre_position(),
        )
        await lsp.write_json_rpc(
            self.output_channel,
            json_rpc.SuccessResponse(
                id=request_id,
                activity_key=activity_key,
                result=lsp.LspLocation.cached_schema().dump(
                    reference_locations,
                    many=True,
                ),
            ),
        )

    async def process_shutdown_request(
        self, request_id: Union[int, str, None]
    ) -> commands.ExitCode:
        await lsp.write_json_rpc_ignore_connection_error(
            self.output_channel,
            json_rpc.SuccessResponse(id=request_id, activity_key=None, result=None),
        )
        return await self.wait_for_exit()

    async def handle_request(
        self, request: json_rpc.Request
    ) -> Optional[commands.ExitCode]:
        """
        Return an exit code if the server needs to be terminated after handling
        the given request, and `None` otherwise.
        """
        if request.method == "exit":
            return commands.ExitCode.FAILURE
        elif request.method == "shutdown":
            return await self.process_shutdown_request(request.id)
        elif request.method == "textDocument/definition":
            await self.process_definition_request(
                lsp.DefinitionParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )
        elif request.method == "textDocument/didOpen":
            await self.process_open_request(
                lsp.DidOpenTextDocumentParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.activity_key,
            )
        elif request.method == "textDocument/didChange":
            await self.process_did_change_request(
                lsp.DidChangeTextDocumentParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                )
            )
        elif request.method == "textDocument/didClose":
            await self.process_close_request(
                lsp.DidCloseTextDocumentParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                )
            )
        elif request.method == "textDocument/didSave":
            await self.process_did_save_request(
                lsp.DidSaveTextDocumentParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.activity_key,
            )
        elif request.method == "textDocument/hover":
            await self.process_hover_request(
                lsp.HoverParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )
        elif request.method == "textDocument/typeCoverage":
            await self.process_type_coverage_request(
                lsp.TypeCoverageParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )
        elif request.method == "textDocument/documentSymbol":
            await self.process_document_symbols_request(
                lsp.DocumentSymbolsParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )
        elif request.method == "textDocument/references":
            await self.process_find_all_references_request(
                lsp.ReferencesParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )
        elif request.id is not None:
            raise lsp.RequestCancelledError("Request not supported yet")

    async def serve_requests(self) -> int:
        while True:
            request = await read_lsp_request(self.input_channel, self.output_channel)
            LOG.debug(f"Received LSP request: {log.truncate(str(request), 400)}")

            try:
                return_code = await self.handle_request(request)
                if return_code is not None:
                    return return_code
            except json_rpc.JSONRPCException as json_rpc_error:
                LOG.debug(
                    f"Exception occurred while processing request: {json_rpc_error}"
                )
                await lsp.write_json_rpc_ignore_connection_error(
                    self.output_channel,
                    json_rpc.ErrorResponse(
                        id=request.id,
                        activity_key=request.activity_key,
                        code=json_rpc_error.error_code(),
                        message=str(json_rpc_error),
                    ),
                )

    async def run(self) -> int:
        """
        Launch the background tasks that deal with starting and subscribing
        to a pyre server and managing a queue of requests, then run the
        language server itself.
        """
        try:
            await self.pyre_manager.ensure_task_running()
            return await self.serve_requests()
        except lsp.ReadChannelClosedError:
            # This error can happen when the connection gets closed unilaterally
            # from the language client, which causes issue when we try to access the
            # input channel. This usually signals that the language client has exited,
            # which implies that the language server should do that as well.
            LOG.info("Connection closed by LSP client.")
            return commands.ExitCode.SUCCESS
        finally:
            await self.pyre_manager.ensure_task_stop()
