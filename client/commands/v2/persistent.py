# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio
import dataclasses
import enum
import json
import logging
import os
import subprocess
import tempfile
import traceback
from pathlib import Path
from typing import Union, Optional, AsyncIterator, Set, List, Sequence, Dict

from ... import (
    json_rpc,
    error,
    version,
    command_arguments,
    commands,
    configuration as configuration_module,
    statistics,
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

CONSECUTIVE_START_ATTEMPT_THRESHOLD: int = 6


class LSPEvent(enum.Enum):
    INITIALIZED = "initialized"
    CONNECTED = "connected"
    NOT_CONNECTED = "not connected"
    DISCONNECTED = "disconnected"
    SUSPENDED = "suspended"


def _log_lsp_event(
    remote_logging: Optional[start.RemoteLogging],
    event: LSPEvent,
    integers: Optional[Dict[str, int]] = None,
    normals: Optional[Dict[str, Optional[str]]] = None,
) -> None:
    if remote_logging is not None:
        logger = remote_logging.logger
        if logger is not None:
            log_identifier = remote_logging.identifier
            statistics.log(
                category=statistics.LoggerCategory.LSP_EVENTS,
                logger=logger,
                integers=integers,
                normals={
                    **(normals or {}),
                    "event": event.value,
                    "pyre client version": version.__version__,
                    **(
                        {"identifier": log_identifier}
                        if log_identifier is not None
                        else {}
                    ),
                },
            )


def process_initialize_request(
    parameters: lsp.InitializeParameters,
) -> lsp.InitializeResult:
    LOG.info(
        f"Received initialization request from {parameters.client_info} "
        f" (pid = {parameters.process_id})"
    )

    server_info = lsp.Info(name="pyre", version=version.__version__)
    server_capabilities = lsp.ServerCapabilities(
        text_document_sync=lsp.TextDocumentSyncOptions(
            open_close=True,
            change=lsp.TextDocumentSyncKind.NONE,
            save=lsp.SaveOptions(include_text=False),
        )
    )
    return lsp.InitializeResult(
        capabilities=server_capabilities, server_info=server_info
    )


@dataclasses.dataclass(frozen=True)
class InitializationSuccess:
    client_capabilities: lsp.ClientCapabilities
    client_info: Optional[lsp.Info] = None
    initialization_options: Optional[lsp.InitializationOptions] = None


@dataclasses.dataclass(frozen=True)
class InitializationFailure:
    exception: Optional[json_rpc.JSONRPCException] = None


@dataclasses.dataclass(frozen=True)
class InitializationExit:
    pass


async def try_initialize(
    input_channel: connection.TextReader,
    output_channel: connection.TextWriter,
) -> Union[InitializationSuccess, InitializationFailure, InitializationExit]:
    """
    Read an LSP message from the input channel and try to initialize an LSP
    server. Also write to the output channel with proper response if the input
    message is a request instead of a notification.

    The function can return one of three possibilities:
    - If the initialization succeeds, return `InitializationSuccess`.
    - If the initialization fails, return `InitializationFailure`. There could
      be many reasons for the failure: The incoming LSP message may not be an
      initiailization request. The incoming LSP request may be malformed. Or the
      client may not complete the handshake by sending back an `initialized` request.
    - If an exit notification is received, return `InitializationExit`. The LSP
      spec allows exiting a server without a preceding initialize request.
    """
    try:
        request = await lsp.read_json_rpc(input_channel)
        LOG.debug(f"Received pre-initialization LSP request: {request}")

        request_id = request.id
        if request_id is None:
            return (
                InitializationExit()
                if request.method == "exit"
                else InitializationFailure()
            )
        if request.method != "initialize":
            raise lsp.ServerNotInitializedError("An initialize request is needed.")
        request_parameters = request.parameters
        if request_parameters is None:
            raise lsp.ServerNotInitializedError(
                "Missing parameters for initialize request."
            )

        initialize_parameters = lsp.InitializeParameters.from_json_rpc_parameters(
            request_parameters
        )
        result = process_initialize_request(initialize_parameters)
        await lsp.write_json_rpc(
            output_channel,
            # pyre-fixme[16]: Pyre doesn't understand `dataclasses_json`
            json_rpc.SuccessResponse(id=request_id, result=result.to_dict()),
        )

        initialized_notification = await lsp.read_json_rpc(input_channel)
        if initialized_notification.method != "initialized":
            raise lsp.ServerNotInitializedError(
                "Failed to receive an `initialized` request from client"
            )

        return InitializationSuccess(
            client_capabilities=initialize_parameters.capabilities,
            client_info=initialize_parameters.client_info,
            initialization_options=initialize_parameters.initialization_options,
        )
    except json_rpc.JSONRPCException as json_rpc_error:
        await lsp.write_json_rpc(
            output_channel,
            json_rpc.ErrorResponse(
                id=request.id,
                code=json_rpc_error.error_code(),
                message=str(json_rpc_error),
                data={"retry": False},
            ),
        )
        return InitializationFailure(exception=json_rpc_error)


@connection.asynccontextmanager
async def _read_lsp_request(
    input_channel: connection.TextReader, output_channel: connection.TextWriter
) -> AsyncIterator[json_rpc.Request]:
    try:
        message = await lsp.read_json_rpc(input_channel)
        yield message
    except json_rpc.JSONRPCException as json_rpc_error:
        await lsp.write_json_rpc(
            output_channel,
            json_rpc.ErrorResponse(
                id=message.id,
                code=json_rpc_error.error_code(),
                message=str(json_rpc_error),
            ),
        )


async def _publish_diagnostics(
    output_channel: connection.TextWriter,
    path: Path,
    diagnostics: Sequence[lsp.Diagnostic],
) -> None:
    LOG.debug(f"Publish diagnostics for {path}: {diagnostics}")
    await lsp.write_json_rpc(
        output_channel,
        json_rpc.Request(
            method="textDocument/publishDiagnostics",
            parameters=json_rpc.ByNameParameters(
                {
                    "uri": lsp.DocumentUri.from_file_path(path).unparse(),
                    "diagnostics": [
                        # pyre-fixme[16]: Pyre doesn't understand `dataclasses_json`
                        diagnostic.to_dict()
                        for diagnostic in diagnostics
                    ],
                }
            ),
        ),
    )


@dataclasses.dataclass
class ServerState:
    consecutive_start_failure: int = 0
    opened_documents: Set[Path] = dataclasses.field(default_factory=set)
    diagnostics: Dict[Path, List[lsp.Diagnostic]] = dataclasses.field(
        default_factory=dict
    )


class PyreServer:
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
                "Not restarting Pyre since failed consecutive start attempt limit"
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


@dataclasses.dataclass(frozen=True)
class StartSuccess:
    pass


@dataclasses.dataclass(frozen=True)
class StartFailure:
    detail: str


async def _start_pyre_server(
    binary_location: str, pyre_arguments: start.Arguments
) -> Union[StartSuccess, StartFailure]:
    try:
        with start.server_argument_file(pyre_arguments) as argument_file_path:
            server_environment = {
                **os.environ,
                # This is to make sure that backend server shares the socket root
                # directory with the client.
                # TODO(T77556312): It might be cleaner to turn this into a
                # configuration option instead.
                "TMPDIR": tempfile.gettempdir(),
            }

            with start.background_server_log_file(
                Path(pyre_arguments.log_path)
            ) as server_stderr:
                server_process = await asyncio.create_subprocess_exec(
                    binary_location,
                    "newserver",
                    str(argument_file_path),
                    stdout=subprocess.PIPE,
                    stderr=server_stderr,
                    env=server_environment,
                    start_new_session=True,
                )

            server_stdout = server_process.stdout
            if server_stdout is None:
                raise RuntimeError(
                    "asyncio.create_subprocess_exec failed to set up a pipe for "
                    "server stdout"
                )

            await server_event.Waiter(wait_on_initialization=True).async_wait_on(
                connection.TextReader(connection.StreamBytesReader(server_stdout))
            )

        return StartSuccess()
    except Exception as error:
        detail = traceback.format_exc()
        LOG.error(f"Exception occured during server start. {detail}")
        return StartFailure(detail)


@dataclasses.dataclass(frozen=True)
class SubscriptionResponse:
    name: str
    body: List[error.Error] = dataclasses.field(default_factory=list)


def parse_subscription_response(response: str) -> SubscriptionResponse:
    try:
        response_json = json.loads(response)
        # The response JSON is expected to have the following form:
        # `{"name": "foo", "body": ["TypeErrors", [error_json, ...]]}`
        if isinstance(response_json, dict):
            name = response_json.get("name", None)
            body = response_json.get("body", None)
            if name is not None and body is not None:
                return SubscriptionResponse(
                    name=name, body=incremental.parse_type_error_response_json(body)
                )
        raise incremental.InvalidServerResponse(
            f"Unexpected JSON subscription from server: {response_json}"
        )
    except json.JSONDecodeError as decode_error:
        message = f"Cannot parse subscription as JSON: {decode_error}"
        raise incremental.InvalidServerResponse(message) from decode_error


def type_error_to_diagnostic(type_error: error.Error) -> lsp.Diagnostic:
    return lsp.Diagnostic(
        range=lsp.Range(
            start=lsp.Position(line=type_error.line - 1, character=type_error.column),
            end=lsp.Position(
                line=type_error.stop_line - 1, character=type_error.stop_column
            ),
        ),
        message=type_error.description,
        severity=lsp.DiagnosticSeverity.ERROR,
        code=None,
        source="Pyre",
    )


def type_errors_to_diagnostics(
    type_errors: Sequence[error.Error],
) -> Dict[Path, List[lsp.Diagnostic]]:
    result: Dict[Path, List[lsp.Diagnostic]] = {}
    for type_error in type_errors:
        result.setdefault(type_error.path, []).append(
            type_error_to_diagnostic(type_error)
        )
    return result


class PyreServerHandler(connection.BackgroundTask):
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
            "Refereshing type errors received from Pyre server. "
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

    @connection.asynccontextmanager
    async def _read_server_response(
        self, server_input_channel: connection.TextReader
    ) -> AsyncIterator[str]:
        try:
            raw_response = await server_input_channel.read_until(separator="\n")
            yield raw_response
        except incremental.InvalidServerResponse as error:
            LOG.error(f"Pyre server returns invalid response: {error}")

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
                "Lost connection to background Pyre server.",
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
                f"Starting a new Pyre server at `{self.server_identifier}` in "
                "the background..."
            )

            start_status = await _start_pyre_server(
                self.binary_location, self.pyre_arguments
            )
            if isinstance(start_status, StartSuccess):
                await self.log_and_show_message_to_client(
                    f"Pyre server at `{self.server_identifier}` has been initialized."
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


async def run_persistent(
    binary_location: str, server_identifier: str, pyre_arguments: start.Arguments
) -> int:
    stdin, stdout = await connection.create_async_stdin_stdout()
    while True:
        initialize_result = await try_initialize(stdin, stdout)
        if isinstance(initialize_result, InitializationExit):
            LOG.info("Received exit request before initialization.")
            return 0
        elif isinstance(initialize_result, InitializationSuccess):
            LOG.info("Initialization successful.")
            client_info = initialize_result.client_info
            _log_lsp_event(
                remote_logging=pyre_arguments.remote_logging,
                event=LSPEvent.INITIALIZED,
                normals=(
                    {}
                    if client_info is None
                    else {
                        "lsp client name": client_info.name,
                        "lsp client version": client_info.version,
                    }
                ),
            )

            client_capabilities = initialize_result.client_capabilities
            LOG.debug(f"Client capabilities: {client_capabilities}")
            initial_server_state = ServerState()
            server = PyreServer(
                input_channel=stdin,
                output_channel=stdout,
                client_capabilities=client_capabilities,
                state=initial_server_state,
                pyre_manager=connection.BackgroundTaskManager(
                    PyreServerHandler(
                        binary_location=binary_location,
                        server_identifier=server_identifier,
                        pyre_arguments=pyre_arguments,
                        client_output_channel=stdout,
                        server_state=initial_server_state,
                    )
                ),
            )
            return await server.run()
        elif isinstance(initialize_result, InitializationFailure):
            exception = initialize_result.exception
            message = (
                str(exception) if exception is not None else "ignoring notification"
            )
            LOG.info(f"Initialization failed: {message}")
            # Loop until we get either InitializeExit or InitializeSuccess
        else:
            raise RuntimeError("Cannot determine the type of initialize_result")


def run(
    configuration: configuration_module.Configuration,
    start_arguments: command_arguments.StartArguments,
) -> int:
    binary_location = configuration.get_binary_respecting_override()
    if binary_location is None:
        raise configuration_module.InvalidConfiguration(
            "Cannot locate a Pyre binary to run."
        )

    server_identifier = start.get_server_identifier(configuration)
    pyre_arguments = start.create_server_arguments(configuration, start_arguments)
    if pyre_arguments.watchman_root is None:
        raise commands.ClientException(
            "Cannot locate a `watchman` root. Pyre's server will not function "
            "properly."
        )

    return asyncio.get_event_loop().run_until_complete(
        run_persistent(binary_location, server_identifier, pyre_arguments)
    )
