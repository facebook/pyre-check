# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import logging
import traceback
import dataclasses
import enum
from pathlib import Path
from typing import Optional, AsyncIterator, Set, List, Sequence, Dict
import async_generator

from ... import (
    json_rpc,
    error,
    version,
    commands,
)

from . import (
    language_server_protocol as lsp,
    server_connection,
    async_server_connection as connection,
    start,
    stop,
    incremental,
    server_event,
)

LOG: logging.Logger = logging.getLogger(__name__)

class LSPEvent(enum.Enum):
    INITIALIZED = "initialized"
    CONNECTED = "connected"
    NOT_CONNECTED = "not connected"
    DISCONNECTED = "disconnected"
    SUSPENDED = "suspended"


CONSECUTIVE_START_ATTEMPT_THRESHOLD: int = 6


@dataclasses.dataclass
class ServerState:
    consecutive_start_failure: int = 0
    opened_documents: Set[Path] = dataclasses.field(default_factory=set)
    diagnostics: Dict[Path, List[lsp.Diagnostic]] = dataclasses.field(
        default_factory=dict
    )



class PysaServerHandler(connection.BackgroundTask):
    binary_location: str
    server_identifier: str
    pyre_arguments: start.Arguments
    client_output_channel: connection.TextWriter
    server_state: ServerState

    def __init__(
        self,
        binary_location: str,
        server_identifier: str,
        pyre_arguments: start.Arguments,
        client_output_channel: connection.TextWriter,
        server_state: ServerState,
    ) -> None:
        self.binary_location = binary_location
        self.server_identifier = server_identifier
        self.pyre_arguments = pyre_arguments
        self.client_output_channel = client_output_channel
        self.server_state = server_state

    async def show_message_to_client(
        self, message: str, level: lsp.MessageType = lsp.MessageType.INFO
    ) -> None:
        await lsp.write_json_rpc(
            self.client_output_channel,
            json_rpc.Request(
                method="window/showMessage",
                parameters=json_rpc.ByNameParameters(
                    {"type": int(level), "message": message}
                ),
            ),
        )

    async def log_and_show_message_to_client(
        self, message: str, level: lsp.MessageType = lsp.MessageType.INFO
    ) -> None:
        if level == lsp.MessageType.ERROR:
            LOG.error(message)
        elif level == lsp.MessageType.WARNING:
            LOG.warning(message)
        elif level == lsp.MessageType.INFO:
            LOG.info(message)
        else:
            LOG.debug(message)
        await self.show_message_to_client(message, level)

    def update_type_errors(self, type_errors: Sequence[error.Error]) -> None:
        LOG.info(
            "Refereshing type errors received from Pysa server. "
            f"Total number of type errors is {len(type_errors)}."
        )
        self.server_state.diagnostics = type_errors_to_diagnostics(type_errors)

    async def show_type_errors_to_client(self) -> None:
        for path in self.server_state.opened_documents:
            await _publish_diagnostics(self.client_output_channel, path, [])
            diagnostics = self.server_state.diagnostics.get(path, None)
            if diagnostics is not None:
                await _publish_diagnostics(
                    self.client_output_channel, path, diagnostics
                )

    @async_generator.asynccontextmanager
    async def _read_server_response(
        self, server_input_channel: connection.TextReader
    ) -> AsyncIterator[str]:
        try:
            raw_response = await server_input_channel.read_until(separator="\n")
            yield raw_response
        except incremental.InvalidServerResponse as error:
            LOG.error(f"Pysa  server returns invalid response: {error}")

    async def _subscribe_to_type_error(
        self,
        server_input_channel: connection.TextReader,
        server_output_channel: connection.TextWriter,
    ) -> None:
        subscription_name = f"persistent_{os.getpid()}"
        await server_output_channel.write(
            f'["SubscribeToTypeErrors", "{subscription_name}"]\n'
        )

        async with self._read_server_response(server_input_channel) as first_response:
            initial_type_errors = incremental.parse_type_error_response(first_response)
            self.update_type_errors(initial_type_errors)
            await self.show_type_errors_to_client()

        while True:
            async with self._read_server_response(
                server_input_channel
            ) as raw_subscription_response:
                subscription_response = parse_subscription_response(
                    raw_subscription_response
                )
                if subscription_name == subscription_response.name:
                    self.update_type_errors(subscription_response.body)
                    await self.show_type_errors_to_client()

    async def subscribe_to_type_error(
        self,
        server_input_channel: connection.TextReader,
        server_output_channel: connection.TextWriter,
    ) -> None:
        try:
            await self._subscribe_to_type_error(
                server_input_channel, server_output_channel
            )
        finally:
            await self.show_message_to_client(
                "Lost connection to background Pysa server.",
                level=lsp.MessageType.WARNING,
            )
            self.server_state.diagnostics = {}
            await self.show_type_errors_to_client()

    def _auxiliary_logging_info(self) -> Dict[str, Optional[str]]:
        return {
            "binary": self.binary_location,
            "log_path": self.pyre_arguments.log_path,
            "global_root": self.pyre_arguments.global_root,
            **(
                {}
                if self.pyre_arguments.local_root is None
                else {"local_root": self.pyre_arguments.relative_local_root}
            ),
        }

    async def _run(self) -> None:
        socket_path = server_connection.get_default_socket_path(
            log_directory=Path(self.pyre_arguments.log_path)
        )
        try:
            async with connection.connect_in_text_mode(socket_path) as (
                input_channel,
                output_channel,
            ):
                await self.log_and_show_message_to_client(
                    "Established connection with existing Pyre server at "
                    f"`{self.server_identifier}`."
                )
                self.server_state.consecutive_start_failure = 0
                _log_lsp_event(
                    remote_logging=self.pyre_arguments.remote_logging,
                    event=LSPEvent.CONNECTED,
                    normals={
                        "connected_to": "already_running_server",
                        **self._auxiliary_logging_info(),
                    },
                )
                await self.subscribe_to_type_error(input_channel, output_channel)
        except connection.ConnectionFailure:
            await self.log_and_show_message_to_client(
                f"Starting a new Pysa server at `{self.server_identifier}` in "
                "the background..."
            )

            start_status = await _start_pyre_server(
                self.binary_location, self.pyre_arguments
            )
            if isinstance(start_status, StartSuccess):
                await self.log_and_show_message_to_client(
                    f"Pysa server at `{self.server_identifier}` has been initialized."
                )

                async with connection.connect_in_text_mode(socket_path) as (
                    input_channel,
                    output_channel,
                ):
                    self.server_state.consecutive_start_failure = 0
                    _log_lsp_event(
                        remote_logging=self.pyre_arguments.remote_logging,
                        event=LSPEvent.CONNECTED,
                        normals={
                            "connected_to": "newly_started_server",
                            **self._auxiliary_logging_info(),
                        },
                    )
                    await self.subscribe_to_type_error(input_channel, output_channel)
            elif isinstance(start_status, StartFailure):
                self.server_state.consecutive_start_failure += 1
                if (
                    self.server_state.consecutive_start_failure
                    < CONSECUTIVE_START_ATTEMPT_THRESHOLD
                ):
                    _log_lsp_event(
                        remote_logging=self.pyre_arguments.remote_logging,
                        event=LSPEvent.NOT_CONNECTED,
                        normals={
                            **self._auxiliary_logging_info(),
                            "exception": str(start_status.detail),
                        },
                    )
                    await self.show_message_to_client(
                        f"Cannot start a new Pyre server at `{self.server_identifier}`.",
                        level=lsp.MessageType.ERROR,
                    )

                    if (
                        self.server_state.consecutive_start_failure
                        == CONSECUTIVE_START_ATTEMPT_THRESHOLD - 1
                    ):
                        # The heuristic here is that if the restart has failed
                        # this many times, it is highly likely that the restart was
                        # blocked by a defunct socket file instead of a concurrent
                        # server start, in which case removing the file could unblock
                        # the server.
                        LOG.warning(f"Removing defunct socket file {socket_path}...")
                        stop.remove_socket_if_exists(socket_path)
                else:
                    await self.show_message_to_client(
                        f"Pyre server restart at `{self.server_identifier}` has been "
                        "failing repeatedly. Disabling The Pyre plugin for now.",
                        level=lsp.MessageType.ERROR,
                    )
                    _log_lsp_event(
                        remote_logging=self.pyre_arguments.remote_logging,
                        event=LSPEvent.SUSPENDED,
                        normals=self._auxiliary_logging_info(),
                    )

            else:
                raise RuntimeError("Impossible type for `start_status`")

    async def run(self) -> None:
        try:
            await self._run()
        except Exception:
            _log_lsp_event(
                remote_logging=self.pyre_arguments.remote_logging,
                event=LSPEvent.DISCONNECTED,
                normals={
                    **self._auxiliary_logging_info(),
                    "exception": traceback.format_exc(),
                },
            )
            raise



class PysaServer:
    # I/O Channels
    input_channel: connection.TextReader
    output_channel: connection.TextWriter

    # Immutable States
    client_capabilities: lsp.ClientCapabilities

    # Mutable States
    state: ServerState
    pyre_manager: connection.BackgroundTaskManager

    def __init__(
        self,
        input_channel: connection.TextReader,
        output_channel: connection.TextWriter,
        client_capabilities: lsp.ClientCapabilities,
        state: ServerState,
        pyre_manager: connection.BackgroundTaskManager,
    ) -> None:
        self.input_channel = input_channel
        self.output_channel = output_channel
        self.client_capabilities = client_capabilities
        self.state = state
        self.pyre_manager = pyre_manager

    async def wait_for_exit(self) -> int:
        while True:
            async with _read_lsp_request(
                self.input_channel, self.output_channel
            ) as request:
                LOG.debug(f"Received post-shutdown request: {request}")

                if request.method == "exit":
                    return 0
                else:
                    raise json_rpc.InvalidRequestError("LSP server has been shut down")

    async def _try_restart_pyre_server(self) -> None:
        if self.state.consecutive_start_failure < CONSECUTIVE_START_ATTEMPT_THRESHOLD:
            await self.pyre_manager.ensure_task_running()
        else:
            LOG.info(
                "Not restarting Pysa since failed consecutive start attempt limit"
                " has been reached."
            )

    async def process_open_request(
        self, parameters: lsp.DidOpenTextDocumentParameters
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        self.state.opened_documents.add(document_path)
        LOG.info(f"File opened: {document_path}")

        # Attempt to trigger a background Pyre server start on each file open
        if not self.pyre_manager.is_task_running():
            await self._try_restart_pyre_server()
        else:
            document_diagnostics = self.state.diagnostics.get(document_path, None)
            if document_diagnostics is not None:
                LOG.info(f"Update diagnostics for {document_path}")
                await _publish_diagnostics(
                    self.output_channel, document_path, document_diagnostics
                )

    async def process_close_request(
        self, parameters: lsp.DidCloseTextDocumentParameters
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        try:
            self.state.opened_documents.remove(document_path)
            LOG.info(f"File closed: {document_path}")

            if document_path in self.state.diagnostics:
                LOG.info(f"Clear diagnostics for {document_path}")
                await _publish_diagnostics(self.output_channel, document_path, [])
        except KeyError:
            LOG.warning(f"Trying to close an un-opened file: {document_path}")

    async def process_did_save_request(
        self, parameters: lsp.DidSaveTextDocumentParameters
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )

        # Attempt to trigger a background Pyre server start on each file save
        if (
            not self.pyre_manager.is_task_running()
            and document_path in self.state.opened_documents
        ):
            await self._try_restart_pyre_server()

    async def _run(self) -> int:
        while True:
            async with _read_lsp_request(
                self.input_channel, self.output_channel
            ) as request:
                LOG.debug(f"Received LSP request: {request}")

                if request.method == "exit":
                    return commands.ExitCode.FAILURE
                elif request.method == "shutdown":
                    lsp.write_json_rpc(
                        self.output_channel,
                        json_rpc.SuccessResponse(id=request.id, result=None),
                    )
                    return await self.wait_for_exit()
                elif request.method == "textDocument/didOpen":
                    parameters = request.parameters
                    if parameters is None:
                        raise json_rpc.InvalidRequestError(
                            "Missing parameters for didOpen method"
                        )
                    await self.process_open_request(
                        lsp.DidOpenTextDocumentParameters.from_json_rpc_parameters(
                            parameters
                        )
                    )
                elif request.method == "textDocument/didClose":
                    parameters = request.parameters
                    if parameters is None:
                        raise json_rpc.InvalidRequestError(
                            "Missing parameters for didClose method"
                        )
                    await self.process_close_request(
                        lsp.DidCloseTextDocumentParameters.from_json_rpc_parameters(
                            parameters
                        )
                    )
                elif request.method == "textDocument/didSave":
                    parameters = request.parameters
                    if parameters is None:
                        raise json_rpc.InvalidRequestError(
                            "Missing parameters for didSave method"
                        )
                    await self.process_did_save_request(
                        lsp.DidSaveTextDocumentParameters.from_json_rpc_parameters(
                            parameters
                        )
                    )
                elif request.id is not None:
                    raise lsp.RequestCancelledError("Request not supported yet")

    async def run(self) -> int:
        try:
            await self.pyre_manager.ensure_task_running()
            return await self._run()
        finally:
            await self.pyre_manager.ensure_task_stop()

