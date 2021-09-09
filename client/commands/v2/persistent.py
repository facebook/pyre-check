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
from typing import Union, Optional, AsyncIterator, Set, List, Sequence, Dict, Callable

from ... import (
    log,
    json_rpc,
    error,
    version,
    command_arguments,
    commands,
    configuration as configuration_module,
    statistics_logger,
)
from . import (
    backend_arguments,
    language_server_protocol as lsp,
    server_connection,
    async_server_connection as connection,
    start,
    incremental,
    server_event,
)

LOG: logging.Logger = logging.getLogger(__name__)

CONSECUTIVE_START_ATTEMPT_THRESHOLD: int = 5


class LSPEvent(enum.Enum):
    INITIALIZED = "initialized"
    NOT_INITIALIZED = "not initialized"
    CONNECTED = "connected"
    NOT_CONNECTED = "not connected"
    NOT_CONFIGURED = "not configured"
    DISCONNECTED = "disconnected"
    SUSPENDED = "suspended"


def _log_lsp_event(
    remote_logging: Optional[backend_arguments.RemoteLogging],
    event: LSPEvent,
    integers: Optional[Dict[str, int]] = None,
    normals: Optional[Dict[str, Optional[str]]] = None,
) -> None:
    if remote_logging is not None:
        logger = remote_logging.logger
        if logger is not None:
            log_identifier = remote_logging.identifier
            statistics_logger.log(
                category=statistics_logger.LoggerCategory.LSP_EVENTS,
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
        if initialized_notification.method == "shutdown":
            await _wait_for_exit(input_channel, output_channel)
            return InitializationExit()
        elif initialized_notification.method != "initialized":
            actual_message = json.dumps(initialized_notification.json())
            raise lsp.ServerNotInitializedError(
                "Failed to receive an `initialized` request from client. "
                + f"Got {log.truncate(actual_message, 100)}"
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


async def _wait_for_exit(
    input_channel: connection.TextReader, output_channel: connection.TextWriter
) -> None:
    """
    Wait for an LSP "exit" request from the `input_channel`. This is mostly useful
    when the LSP server has received a "shutdown" request, in which case the LSP
    specification dictates that only "exit" can be sent from the client side.

    If a non-exit LSP request is received, drop it and keep waiting on another
    "exit" request.
    """
    while True:
        async with _read_lsp_request(input_channel, output_channel) as request:
            if request.method == "exit":
                return
            else:
                raise json_rpc.InvalidRequestError(
                    f"Only exit requests are accepted after shutdown. Got {request}."
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
    # Immutable States
    client_capabilities: lsp.ClientCapabilities = lsp.ClientCapabilities()

    # Mutable States
    consecutive_start_failure: int = 0
    opened_documents: Set[Path] = dataclasses.field(default_factory=set)
    diagnostics: Dict[Path, List[lsp.Diagnostic]] = dataclasses.field(
        default_factory=dict
    )


class PyreServer:
    # I/O Channels
    input_channel: connection.TextReader
    output_channel: connection.TextWriter

    # `pyre_manager` is responsible for handling all interactions with background
    # Pyre server.
    pyre_manager: connection.BackgroundTaskManager
    # NOTE: `state` is mutable and can be changed on `pyre_manager` side.
    state: ServerState

    def __init__(
        self,
        input_channel: connection.TextReader,
        output_channel: connection.TextWriter,
        state: ServerState,
        pyre_manager: connection.BackgroundTaskManager,
    ) -> None:
        self.input_channel = input_channel
        self.output_channel = output_channel
        self.state = state
        self.pyre_manager = pyre_manager

    async def wait_for_exit(self) -> int:
        await _wait_for_exit(self.input_channel, self.output_channel)
        return 0

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
                LOG.debug(f"Received LSP request: {log.truncate(str(request), 400)}")

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
    message: str
    detail: str


async def _start_pyre_server(
    binary_location: str, pyre_arguments: start.Arguments
) -> Union[StartSuccess, StartFailure]:
    try:
        with backend_arguments.temporary_argument_file(
            pyre_arguments
        ) as argument_file_path:
            server_environment = {
                **os.environ,
                # This is to make sure that backend server shares the socket root
                # directory with the client.
                # TODO(T77556312): It might be cleaner to turn this into a
                # configuration option instead.
                "TMPDIR": tempfile.gettempdir(),
            }

            with start.background_server_log_file(
                Path(pyre_arguments.base_arguments.log_path)
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
    except server_event.ServerStartException as error:
        # We know where the exception come from. Let's keep the error details
        # succinct.
        message = str(error)
        LOG.error(message)
        return StartFailure(message=message, detail=message)
    except Exception as error:
        # These exceptions are unexpected. Let's keep verbose stack traces to
        # help with post-mortem analyses.
        message = str(error)
        detail = traceback.format_exc()
        LOG.error(f"{detail}")
        return StartFailure(message=message, detail=detail)


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


@dataclasses.dataclass(frozen=True)
class PyreServerStartOptions:
    binary: str
    server_identifier: str
    start_arguments: start.Arguments

    @staticmethod
    def read_from(
        command_argument: command_arguments.CommandArguments, base_directory: Path
    ) -> "PyreServerStartOptions":
        configuration = configuration_module.create_configuration(
            command_argument, base_directory
        )
        binary_location = configuration.get_binary_respecting_override()
        if binary_location is None:
            raise configuration_module.InvalidConfiguration(
                "Cannot locate a Pyre binary to run."
            )

        start_arguments = start.create_server_arguments(
            configuration,
            command_arguments.StartArguments(
                changed_files_path=command_argument.changed_files_path,
                debug=command_argument.debug,
                enable_memory_profiling=command_argument.enable_memory_profiling,
                enable_profiling=command_argument.enable_profiling,
                load_initial_state_from=command_argument.load_initial_state_from,
                log_identifier=command_argument.log_identifier,
                logging_sections=command_argument.logging_sections,
                no_saved_state=command_argument.no_saved_state,
                no_watchman=False,
                noninteractive=command_argument.noninteractive,
                save_initial_state_to=command_argument.save_initial_state_to,
                saved_state_project=command_argument.saved_state_project,
                sequential=command_argument.sequential,
                show_error_traces=command_argument.show_error_traces,
                store_type_check_resolution=False,
                terminal=False,
                wait_on_initialization=True,
            ),
        )
        if start_arguments.watchman_root is None:
            raise commands.ClientException(
                "Cannot locate a `watchman` root. Pyre's server will not function "
                "properly."
            )

        return PyreServerStartOptions(
            binary=binary_location,
            server_identifier=start.get_server_identifier(configuration),
            start_arguments=start_arguments,
        )


PyreServerStartOptionsReader = Callable[[], PyreServerStartOptions]


class PyreServerHandler(connection.BackgroundTask):
    server_start_options_reader: PyreServerStartOptionsReader
    remote_logging: Optional[backend_arguments.RemoteLogging]
    client_output_channel: connection.TextWriter
    server_state: ServerState

    def __init__(
        self,
        server_start_options_reader: PyreServerStartOptionsReader,
        client_output_channel: connection.TextWriter,
        server_state: ServerState,
        remote_logging: Optional[backend_arguments.RemoteLogging] = None,
    ) -> None:
        self.server_start_options_reader = server_start_options_reader
        self.remote_logging = remote_logging
        self.client_output_channel = client_output_channel
        self.server_state = server_state

    async def show_status_message_to_client(
        self,
        message: str,
        short_message: Optional[str] = None,
        level: lsp.MessageType = lsp.MessageType.INFO,
    ) -> None:
        def clientSupportsStatusBar(
            client_capabilities: lsp.ClientCapabilities,
        ) -> bool:
            window_capabilities = client_capabilities.window
            if window_capabilities is not None:
                return window_capabilities.status is not None
            else:
                return False

        if clientSupportsStatusBar(self.server_state.client_capabilities):
            await lsp.write_json_rpc(
                self.client_output_channel,
                json_rpc.Request(
                    id=0,  # the value doesn't matter but the existence does
                    method="window/showStatus",
                    parameters=json_rpc.ByNameParameters(
                        {
                            "type": int(level),
                            "message": message,
                            **(
                                {}
                                if short_message is None
                                else {"shortMessage": short_message}
                            ),
                        }
                    ),
                ),
            )
        else:
            status_message = (
                message if short_message is None else f"{short_message}: {message}"
            )
            await lsp.write_json_rpc(
                self.client_output_channel,
                json_rpc.Request(
                    method="window/showMessage",
                    parameters=json_rpc.ByNameParameters(
                        {"type": int(level), "message": status_message}
                    ),
                ),
            )

    async def log_and_show_status_message_to_client(
        self,
        message: str,
        short_message: Optional[str] = None,
        level: lsp.MessageType = lsp.MessageType.INFO,
    ) -> None:
        log_message = (
            message if short_message is None else f"[{short_message}] {message}"
        )
        if level == lsp.MessageType.ERROR:
            LOG.error(log_message)
        elif level == lsp.MessageType.WARNING:
            LOG.warning(log_message)
        elif level == lsp.MessageType.INFO:
            LOG.info(log_message)
        else:
            LOG.debug(log_message)
        await self.show_status_message_to_client(message, short_message, level)

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
            await self.show_status_message_to_client(
                "Lost connection to the background Pyre Server. "
                "This usually happens when Pyre detect changes in project which "
                "it was not able to handle incrementally. "
                "A new Pyre server will be started next time you open or save "
                "a .py file",
                short_message="Pyre Stopped",
                level=lsp.MessageType.ERROR,
            )
            self.server_state.diagnostics = {}
            await self.show_type_errors_to_client()

    @staticmethod
    def _auxiliary_logging_info(
        server_start_options: PyreServerStartOptions,
    ) -> Dict[str, Optional[str]]:
        relative_local_root = (
            server_start_options.start_arguments.base_arguments.relative_local_root
        )
        return {
            "binary": server_start_options.binary,
            "log_path": server_start_options.start_arguments.base_arguments.log_path,
            "global_root": server_start_options.start_arguments.base_arguments.global_root,
            **(
                {}
                if relative_local_root is None
                else {"local_root": relative_local_root}
            ),
        }

    async def _run(self, server_start_options: PyreServerStartOptions) -> None:
        server_identifier = server_start_options.server_identifier
        start_arguments = server_start_options.start_arguments
        socket_path = server_connection.get_default_socket_path(
            log_directory=Path(start_arguments.base_arguments.log_path)
        )
        try:
            async with connection.connect_in_text_mode(socket_path) as (
                input_channel,
                output_channel,
            ):
                await self.log_and_show_status_message_to_client(
                    "Established connection with existing Pyre server at "
                    f"`{server_identifier}`.",
                    short_message="Pyre Ready",
                    level=lsp.MessageType.INFO,
                )
                self.server_state.consecutive_start_failure = 0
                _log_lsp_event(
                    remote_logging=self.remote_logging,
                    event=LSPEvent.CONNECTED,
                    normals={
                        "connected_to": "already_running_server",
                        **self._auxiliary_logging_info(server_start_options),
                    },
                )
                await self.subscribe_to_type_error(input_channel, output_channel)
        except connection.ConnectionFailure:
            await self.log_and_show_status_message_to_client(
                f"Starting a new Pyre server at `{server_identifier}` in "
                "the background.",
                short_message="Starting Pyre...",
                level=lsp.MessageType.WARNING,
            )

            start_status = await _start_pyre_server(
                server_start_options.binary, start_arguments
            )
            if isinstance(start_status, StartSuccess):
                await self.log_and_show_status_message_to_client(
                    f"Pyre server at `{server_identifier}` has been initialized.",
                    short_message="Pyre Ready",
                    level=lsp.MessageType.INFO,
                )

                async with connection.connect_in_text_mode(socket_path) as (
                    input_channel,
                    output_channel,
                ):
                    self.server_state.consecutive_start_failure = 0
                    _log_lsp_event(
                        remote_logging=self.remote_logging,
                        event=LSPEvent.CONNECTED,
                        normals={
                            "connected_to": "newly_started_server",
                            **self._auxiliary_logging_info(server_start_options),
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
                        remote_logging=self.remote_logging,
                        event=LSPEvent.NOT_CONNECTED,
                        normals={
                            **self._auxiliary_logging_info(server_start_options),
                            "exception": str(start_status.detail),
                        },
                    )
                    await self.show_status_message_to_client(
                        f"Cannot start a new Pyre server at `{server_identifier}`. "
                        f"{start_status.message}",
                        short_message="Pyre Stopped",
                        level=lsp.MessageType.WARNING,
                    )
                else:
                    await self.show_status_message_to_client(
                        f"Pyre server restart at `{server_identifier}` has been "
                        "failing repeatedly. Disabling The Pyre plugin for now.",
                        short_message="Pyre Disabled",
                        level=lsp.MessageType.ERROR,
                    )
                    _log_lsp_event(
                        remote_logging=self.remote_logging,
                        event=LSPEvent.SUSPENDED,
                        normals=self._auxiliary_logging_info(server_start_options),
                    )

            else:
                raise RuntimeError("Impossible type for `start_status`")

    def read_server_start_options(self) -> PyreServerStartOptions:
        try:
            LOG.info("Reading Pyre server configurations...")
            return self.server_start_options_reader()
        except Exception:
            _log_lsp_event(
                remote_logging=self.remote_logging,
                event=LSPEvent.NOT_CONFIGURED,
                normals={
                    "exception": traceback.format_exc(),
                },
            )
            raise

    async def run(self) -> None:
        # Re-read server start options on every run, to make sure the server
        # start options are always up-to-date.
        server_start_options = self.read_server_start_options()
        try:
            LOG.info(f"Starting Pyre server from configuration: {server_start_options}")
            await self._run(server_start_options)
        except Exception:
            _log_lsp_event(
                remote_logging=self.remote_logging,
                event=LSPEvent.DISCONNECTED,
                normals={
                    **self._auxiliary_logging_info(server_start_options),
                    "exception": traceback.format_exc(),
                },
            )
            raise


async def run_persistent(
    server_start_options_reader: PyreServerStartOptionsReader,
    remote_logging: Optional[backend_arguments.RemoteLogging],
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
                remote_logging=remote_logging,
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
            initial_server_state = ServerState(client_capabilities=client_capabilities)
            server = PyreServer(
                input_channel=stdin,
                output_channel=stdout,
                state=initial_server_state,
                pyre_manager=connection.BackgroundTaskManager(
                    PyreServerHandler(
                        server_start_options_reader=server_start_options_reader,
                        remote_logging=remote_logging,
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
            _log_lsp_event(
                remote_logging=remote_logging,
                event=LSPEvent.NOT_INITIALIZED,
                normals=(
                    {
                        "exception": message,
                    }
                ),
            )
            # Loop until we get either InitializeExit or InitializeSuccess
        else:
            raise RuntimeError("Cannot determine the type of initialize_result")


def run(
    command_argument: command_arguments.CommandArguments,
    base_directory: Path,
    remote_logging: Optional[backend_arguments.RemoteLogging],
) -> int:
    def read_server_start_options() -> PyreServerStartOptions:
        return PyreServerStartOptions.read_from(command_argument, base_directory)

    return asyncio.get_event_loop().run_until_complete(
        run_persistent(
            read_server_start_options,
            remote_logging,
        )
    )
