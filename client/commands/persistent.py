# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import asyncio
import dataclasses
import json
import logging
import os
import subprocess
import tempfile
import traceback
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Union

from .. import (
    configuration as configuration_module,
    error,
    identifiers,
    json_rpc,
    log,
    timer,
    version,
)
from . import (
    backend_arguments,
    background,
    commands,
    connections,
    find_symbols,
    incremental,
    language_server_protocol as lsp,
    server_event,
    start,
    subscription,
)

from .language_server_features import LanguageServerFeatures

from .log_lsp_event import _log_lsp_event, LSPEvent

from .pyre_server_options import PyreServerOptions, PyreServerOptionsReader

from .request_handler import AbstractRequestHandler, RequestHandler
from .server_state import ServerState

LOG: logging.Logger = logging.getLogger(__name__)

COMMAND_NAME = "persistent"

CONSECUTIVE_START_ATTEMPT_THRESHOLD: int = 5


def read_server_options(
    server_options_reader: PyreServerOptionsReader,
    remote_logging: Optional[backend_arguments.RemoteLogging],
) -> "PyreServerOptions":
    try:
        LOG.info("Reading Pyre server configurations...")
        return server_options_reader()
    except Exception:
        _log_lsp_event(
            remote_logging=remote_logging,
            event=LSPEvent.NOT_CONFIGURED,
            normals={
                "exception": traceback.format_exc(),
            },
        )
        raise


def process_initialize_request(
    parameters: lsp.InitializeParameters,
    language_server_features: Optional[LanguageServerFeatures] = None,
) -> lsp.InitializeResult:
    LOG.info(
        f"Received initialization request from {parameters.client_info} "
        f" (pid = {parameters.process_id})"
    )
    if language_server_features is None:
        language_server_features = LanguageServerFeatures()
    server_info = lsp.Info(name="pyre", version=version.__version__)
    did_change_result = (
        lsp.TextDocumentSyncKind.FULL
        if language_server_features.unsaved_changes.is_enabled()
        else lsp.TextDocumentSyncKind.NONE
    )
    server_capabilities = lsp.ServerCapabilities(
        text_document_sync=lsp.TextDocumentSyncOptions(
            open_close=True,
            change=did_change_result,
            save=lsp.SaveOptions(include_text=False),
        ),
        **language_server_features.capabilities(),
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
    input_channel: connections.AsyncTextReader,
    output_channel: connections.AsyncTextWriter,
    server_options: PyreServerOptions,
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
    request = None
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

        result = process_initialize_request(
            initialize_parameters, server_options.language_server_features
        )
        await lsp.write_json_rpc_ignore_connection_error(
            output_channel,
            json_rpc.SuccessResponse(
                id=request_id,
                activity_key=request.activity_key,
                result=result.to_dict(),
            ),
        )

        initialized_notification = await lsp.read_json_rpc(input_channel)
        if initialized_notification.method == "shutdown":
            try:
                await _wait_for_exit(input_channel, output_channel)
            except lsp.ReadChannelClosedError:
                # This error can happen when the connection gets closed unilaterally
                # from the language client, which causes issue when we try to access
                # the input channel. This usually signals that the language client
                # has exited, which implies that the language server should do that
                # as well.
                LOG.info("Initialization connection closed by LSP client")
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
        await lsp.write_json_rpc_ignore_connection_error(
            output_channel,
            json_rpc.ErrorResponse(
                id=request.id if request is not None else None,
                activity_key=request.activity_key if request is not None else None,
                code=json_rpc_error.error_code(),
                message=str(json_rpc_error),
                data={"retry": False},
            ),
        )
        return InitializationFailure(exception=json_rpc_error)
    except lsp.ReadChannelClosedError:
        return InitializationExit()


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


async def _publish_diagnostics(
    output_channel: connections.AsyncTextWriter,
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
                    "diagnostics": [diagnostic.to_dict() for diagnostic in diagnostics],
                }
            ),
        ),
    )


async def _read_server_response(
    server_input_channel: connections.AsyncTextReader,
) -> str:
    return await server_input_channel.read_until(separator="\n")


@dataclasses.dataclass(frozen=True)
class StartSuccess:
    pass


@dataclasses.dataclass(frozen=True)
class BuckStartFailure:
    message: str


@dataclasses.dataclass(frozen=True)
class OtherStartFailure:
    message: str
    detail: str


async def _start_pyre_server(
    binary_location: str,
    pyre_arguments: start.Arguments,
    flavor: identifiers.PyreFlavor,
) -> Union[StartSuccess, BuckStartFailure, OtherStartFailure]:
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
                Path(pyre_arguments.base_arguments.log_path),
                flavor=flavor,
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
                connections.AsyncTextReader(
                    connections.StreamBytesReader(server_stdout)
                )
            )

        return StartSuccess()
    except server_event.ServerStartException as error:
        message = str(error)
        LOG.error(message)
        if error.kind == server_event.ErrorKind.BUCK_USER:
            return BuckStartFailure(message)
        else:
            # We know where the exception come from. Let's keep the error details
            # succinct.
            return OtherStartFailure(message=message, detail=message)
    except Exception as error:
        # These exceptions are unexpected. Let's keep verbose stack traces to
        # help with post-mortem analyses.
        message = str(error)
        detail = traceback.format_exc()
        LOG.error(f"{detail}")
        return OtherStartFailure(message=message, detail=detail)


def type_error_to_diagnostic(type_error: error.Error) -> lsp.Diagnostic:
    return lsp.Diagnostic(
        range=lsp.LspRange(
            start=lsp.LspPosition(
                line=type_error.line - 1, character=type_error.column
            ),
            end=lsp.LspPosition(
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


def _client_has_status_bar_support(
    client_capabilities: lsp.ClientCapabilities,
) -> bool:
    window_capabilities = client_capabilities.window
    if window_capabilities is not None:
        return window_capabilities.status is not None
    else:
        return False


async def _write_status(
    output_channel: connections.AsyncTextWriter,
    message: str,
    short_message: Optional[str] = None,
    level: lsp.MessageType = lsp.MessageType.INFO,
) -> None:
    await lsp.write_json_rpc(
        output_channel,
        json_rpc.Request(
            id=0,  # the value doesn't matter but the existence does
            method="window/showStatus",
            parameters=json_rpc.ByNameParameters(
                {
                    "type": int(level),
                    "message": message,
                    **(
                        {} if short_message is None else {"shortMessage": short_message}
                    ),
                }
            ),
        ),
    )


async def _write_notification(
    output_channel: connections.AsyncTextWriter,
    message: str,
    short_message: Optional[str] = None,
    level: lsp.MessageType = lsp.MessageType.INFO,
) -> None:
    await lsp.write_json_rpc(
        output_channel,
        json_rpc.Request(
            method="window/showMessage",
            parameters=json_rpc.ByNameParameters(
                {
                    "type": int(level),
                    "message": (
                        message
                        if short_message is None
                        else f"{short_message}: {message}"
                    ),
                }
            ),
        ),
    )


class PyreDaemonShutdown(Exception):
    pass


class PyreDaemonLaunchAndSubscribeHandler(background.Task):
    server_options_reader: PyreServerOptionsReader
    remote_logging: Optional[backend_arguments.RemoteLogging]
    client_output_channel: connections.AsyncTextWriter
    server_state: ServerState

    def __init__(
        self,
        server_options_reader: PyreServerOptionsReader,
        client_output_channel: connections.AsyncTextWriter,
        server_state: ServerState,
        remote_logging: Optional[backend_arguments.RemoteLogging] = None,
    ) -> None:
        self.server_options_reader = server_options_reader
        self.remote_logging = remote_logging
        self.client_output_channel = client_output_channel
        self.server_state = server_state

    async def show_notification_message_to_client(
        self,
        message: str,
        level: lsp.MessageType = lsp.MessageType.INFO,
    ) -> None:
        await _write_notification(self.client_output_channel, message, level=level)

    async def show_status_message_to_client(
        self,
        message: str,
        short_message: Optional[str] = None,
        level: lsp.MessageType = lsp.MessageType.INFO,
        fallback_to_notification: bool = False,
    ) -> None:
        if _client_has_status_bar_support(self.server_state.client_capabilities):
            await _write_status(
                self.client_output_channel, message, short_message, level
            )
        elif fallback_to_notification:
            await _write_notification(
                self.client_output_channel, message, short_message, level
            )

    async def log_and_show_status_message_to_client(
        self,
        message: str,
        short_message: Optional[str] = None,
        level: lsp.MessageType = lsp.MessageType.INFO,
        fallback_to_notification: bool = False,
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
        await self.show_status_message_to_client(
            message, short_message, level, fallback_to_notification
        )

    def update_type_errors(self, type_errors: Sequence[error.Error]) -> None:
        LOG.info(
            "Refreshing type errors received from Pyre server. "
            f"Total number of type errors is {len(type_errors)}."
        )
        incremental.log_error_statistics(
            remote_logging=self.remote_logging,
            type_errors=type_errors,
            command_name=COMMAND_NAME,
        )
        self.server_state.diagnostics = type_errors_to_diagnostics(type_errors)

    async def clear_type_errors_for_client(self) -> None:
        for path in self.server_state.diagnostics:
            await _publish_diagnostics(self.client_output_channel, path, [])
        last_update_timer = self.server_state.last_diagnostic_update_timer
        _log_lsp_event(
            self.remote_logging,
            LSPEvent.COVERED,
            integers={"duration": int(last_update_timer.stop_in_millisecond())},
        )
        # Reset the timestamp to avoid duplicate counting
        last_update_timer.reset()

    async def show_type_errors_to_client(self) -> None:
        for path, diagnostics in self.server_state.diagnostics.items():
            await _publish_diagnostics(self.client_output_channel, path, diagnostics)
        self.server_state.last_diagnostic_update_timer.reset()

    async def handle_type_error_subscription(
        self, type_error_subscription: subscription.TypeErrors
    ) -> None:
        availability = (
            self.server_state.server_options.language_server_features.type_errors
        )
        if availability.is_enabled():
            await self.clear_type_errors_for_client()
            self.update_type_errors(type_error_subscription.errors)
            await self.show_type_errors_to_client()
            await self.log_and_show_status_message_to_client(
                "Pyre has completed an incremental check and is currently "
                "watching on further source changes.",
                short_message="Pyre Ready",
                level=lsp.MessageType.INFO,
            )

    async def handle_status_update_subscription(
        self, status_update_subscription: subscription.StatusUpdate
    ) -> None:
        await self.clear_type_errors_for_client()
        if status_update_subscription.kind == "Rebuilding":
            await self.log_and_show_status_message_to_client(
                "Pyre is busy rebuilding the project for type checking...",
                short_message="Pyre (waiting for Buck)",
                level=lsp.MessageType.WARNING,
            )
        elif status_update_subscription.kind == "Rechecking":
            await self.log_and_show_status_message_to_client(
                "Pyre is busy re-type-checking the project...",
                short_message="Pyre (checking)",
                level=lsp.MessageType.WARNING,
            )

    async def handle_error_subscription(
        self, error_subscription: subscription.Error
    ) -> None:
        message = error_subscription.message
        LOG.info(f"Received error from subscription channel: {message}")
        raise PyreDaemonShutdown(message)

    async def _handle_subscription_body(
        self, subscription_body: subscription.Body
    ) -> None:
        if isinstance(subscription_body, subscription.TypeErrors):
            await self.handle_type_error_subscription(subscription_body)
        elif isinstance(subscription_body, subscription.StatusUpdate):
            await self.handle_status_update_subscription(subscription_body)
        elif isinstance(subscription_body, subscription.Error):
            await self.handle_error_subscription(subscription_body)

    async def _subscribe_to_type_error(
        self,
        server_input_channel: connections.AsyncTextReader,
        server_output_channel: connections.AsyncTextWriter,
    ) -> None:
        subscription_name = f"persistent_{os.getpid()}"
        await server_output_channel.write(
            f'["SubscribeToTypeErrors", "{subscription_name}"]\n'
        )

        first_response = await _read_server_response(server_input_channel)
        initial_type_errors = incremental.parse_type_error_response(first_response)
        self.update_type_errors(initial_type_errors)
        await self.show_type_errors_to_client()

        while True:
            raw_subscription_response = await _read_server_response(
                server_input_channel
            )
            subscription_response = subscription.Response.parse(
                raw_subscription_response
            )
            if subscription_name == subscription_response.name:
                await self._handle_subscription_body(subscription_response.body)

    async def subscribe_to_type_error(
        self,
        server_input_channel: connections.AsyncTextReader,
        server_output_channel: connections.AsyncTextWriter,
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
                fallback_to_notification=True,
            )
            await self.clear_type_errors_for_client()
            self.server_state.diagnostics = {}

    @staticmethod
    def _auxiliary_logging_info(
        server_options: PyreServerOptions,
    ) -> Dict[str, Optional[str]]:
        relative_local_root = (
            server_options.start_arguments.base_arguments.relative_local_root
        )
        return {
            "binary": server_options.binary,
            "log_path": server_options.start_arguments.base_arguments.log_path,
            "global_root": (server_options.start_arguments.base_arguments.global_root),
            **(
                {}
                if relative_local_root is None
                else {"local_root": relative_local_root}
            ),
        }

    async def _try_connect_and_subscribe(
        self,
        server_options: PyreServerOptions,
        socket_path: Path,
        connection_timer: timer.Timer,
        is_preexisting: bool,
    ) -> None:
        project_identifier = server_options.project_identifier
        async with connections.connect_async(socket_path) as (
            input_channel,
            output_channel,
        ):
            if is_preexisting:
                await self.log_and_show_status_message_to_client(
                    "Established connection with existing Pyre server at "
                    f"`{project_identifier}`.",
                    short_message="Pyre Ready",
                    level=lsp.MessageType.INFO,
                    fallback_to_notification=True,
                )
            else:
                await self.log_and_show_status_message_to_client(
                    f"Pyre server at `{project_identifier}` has been initialized.",
                    short_message="Pyre Ready",
                    level=lsp.MessageType.INFO,
                    fallback_to_notification=True,
                )
            self.server_state.consecutive_start_failure = 0
            self.server_state.is_user_notified_on_buck_failure = False
            _log_lsp_event(
                remote_logging=self.remote_logging,
                event=LSPEvent.CONNECTED,
                integers={"duration": int(connection_timer.stop_in_millisecond())},
                normals={
                    "connected_to": (
                        "already_running_server"
                        if is_preexisting
                        else "newly_started_server"
                    ),
                    **self._auxiliary_logging_info(server_options),
                },
            )
            await self.subscribe_to_type_error(input_channel, output_channel)

    async def launch_and_subscribe(self, server_options: PyreServerOptions) -> None:
        project_identifier = server_options.project_identifier
        start_arguments = server_options.start_arguments
        socket_path = server_options.get_socket_path()
        flavor = server_options.flavor

        connection_timer = timer.Timer()
        try:
            return await self._try_connect_and_subscribe(
                server_options,
                socket_path,
                connection_timer,
                is_preexisting=True,
            )
        except connections.ConnectionFailure:
            pass

        await self.log_and_show_status_message_to_client(
            f"Starting a new Pyre server at `{project_identifier}` in "
            "the background.",
            short_message="Starting Pyre...",
            level=lsp.MessageType.WARNING,
            fallback_to_notification=True,
        )
        start_status = await _start_pyre_server(
            server_options.binary,
            start_arguments,
            flavor,
        )
        if isinstance(start_status, StartSuccess):
            await self._try_connect_and_subscribe(
                server_options,
                socket_path,
                connection_timer,
                is_preexisting=False,
            )
        elif isinstance(start_status, BuckStartFailure):
            # Buck start failures are intentionally not counted towards
            # `consecutive_start_failure` -- they happen far too often in practice
            # so we do not want them to trigger suspensions.
            _log_lsp_event(
                remote_logging=self.remote_logging,
                event=LSPEvent.NOT_CONNECTED,
                integers={"duration": int(connection_timer.stop_in_millisecond())},
                normals={
                    **self._auxiliary_logging_info(server_options),
                    "exception": str(start_status.message),
                },
            )
            if not self.server_state.is_user_notified_on_buck_failure:
                await self.show_notification_message_to_client(
                    f"Cannot start a new Pyre server at `{project_identifier}` "
                    "due to Buck failure. If you added or changed a target, "
                    "make sure the target file is parsable and the owning "
                    "targets are buildable by Buck. If you removed a target, "
                    "make sure that target is not explicitly referenced from the "
                    "Pyre configuration file of the containing project.",
                    level=lsp.MessageType.ERROR,
                )
                self.server_state.is_user_notified_on_buck_failure = True
            await self.show_status_message_to_client(
                f"Cannot start a new Pyre server at `{project_identifier}`. "
                f"{start_status.message}",
                short_message="Pyre Stopped",
                level=lsp.MessageType.INFO,
                fallback_to_notification=False,
            )
        elif isinstance(start_status, OtherStartFailure):
            self.server_state.consecutive_start_failure += 1
            if (
                self.server_state.consecutive_start_failure
                < CONSECUTIVE_START_ATTEMPT_THRESHOLD
            ):
                _log_lsp_event(
                    remote_logging=self.remote_logging,
                    event=LSPEvent.NOT_CONNECTED,
                    integers={"duration": int(connection_timer.stop_in_millisecond())},
                    normals={
                        **self._auxiliary_logging_info(server_options),
                        "exception": str(start_status.detail),
                    },
                )
                await self.show_status_message_to_client(
                    f"Cannot start a new Pyre server at `{project_identifier}`. "
                    f"{start_status.message}",
                    short_message="Pyre Stopped",
                    level=lsp.MessageType.INFO,
                    fallback_to_notification=True,
                )
            else:
                _log_lsp_event(
                    remote_logging=self.remote_logging,
                    event=LSPEvent.SUSPENDED,
                    integers={"duration": int(connection_timer.stop_in_millisecond())},
                    normals={
                        **self._auxiliary_logging_info(server_options),
                        "exception": str(start_status.detail),
                    },
                )
                await self.show_status_message_to_client(
                    f"Pyre server restart at `{project_identifier}` has been "
                    "failing repeatedly. Disabling The Pyre plugin for now.",
                    short_message="Pyre Disabled",
                    level=lsp.MessageType.ERROR,
                    fallback_to_notification=True,
                )
        else:
            raise RuntimeError("Impossible type for `start_status`")

    async def run(self) -> None:
        """
        Reread the server start options, which can change due to configuration
        reloading, and run with error logging.
        """
        server_options = read_server_options(
            self.server_options_reader, self.remote_logging
        )
        # Update the server options, which can change if the config is modified
        self.server_state.server_options = server_options
        session_timer = timer.Timer()
        error_message: Optional[str] = None
        try:
            LOG.info(f"Starting Pyre server from configuration: {server_options}")
            await self.launch_and_subscribe(server_options)
        except asyncio.CancelledError:
            error_message = "Explicit termination request"
            raise
        except PyreDaemonShutdown as error:
            error_message = f"Pyre server shutdown: {error}"
        except BaseException:
            error_message = traceback.format_exc()
            raise
        finally:
            _log_lsp_event(
                remote_logging=self.remote_logging,
                event=LSPEvent.DISCONNECTED,
                integers={"duration": int(session_timer.stop_in_millisecond())},
                normals={
                    **self._auxiliary_logging_info(server_options),
                    **(
                        {"exception": error_message}
                        if error_message is not None
                        else {}
                    ),
                },
            )


@dataclasses.dataclass(frozen=True)
class PyreServer:
    # I/O Channels
    input_channel: connections.AsyncTextReader
    output_channel: connections.AsyncTextWriter

    # inside this task manager is a PyreDaemonLaunchAndSubscribeHandler
    pyre_manager: background.TaskManager
    # NOTE: The fields inside `server_state` are mutable and can be changed by `pyre_manager`
    server_state: ServerState

    handler: AbstractRequestHandler

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

    def get_language_server_features(self) -> LanguageServerFeatures:
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
            await self.write_telemetry(
                {
                    "type": "LSP",
                    "operation": "hover",
                    "filePath": str(document_path),
                    "nonEmpty": len(result.contents) > 0,
                    "response": raw_result,
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
            await self.write_telemetry(
                {
                    "type": "LSP",
                    "operation": "definition",
                    "filePath": str(document_path),
                    "count": len(raw_result),
                    "response": raw_result,
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


async def run_persistent(
    server_options_reader: PyreServerOptionsReader,
    remote_logging: Optional[backend_arguments.RemoteLogging],
) -> int:
    try:
        initial_server_options = read_server_options(
            server_options_reader, remote_logging=None
        )
    except configuration_module.InvalidConfiguration as e:
        raise lsp.ServerNotInitializedError(str(e)) from None
    stdin, stdout = await connections.create_async_stdin_stdout()
    while True:
        initialize_result = await try_initialize(stdin, stdout, initial_server_options)
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
            server_state = ServerState(
                client_capabilities=client_capabilities,
                server_options=initial_server_options,
            )
            server = PyreServer(
                input_channel=stdin,
                output_channel=stdout,
                server_state=server_state,
                pyre_manager=background.TaskManager(
                    PyreDaemonLaunchAndSubscribeHandler(
                        server_options_reader=server_options_reader,
                        remote_logging=remote_logging,
                        client_output_channel=stdout,
                        server_state=server_state,
                    )
                ),
                handler=RequestHandler(
                    server_state=server_state,
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
    read_server_options: PyreServerOptionsReader,
    remote_logging: Optional[backend_arguments.RemoteLogging],
) -> int:
    command_timer = timer.Timer()
    error_message: Optional[str] = None
    try:
        return asyncio.get_event_loop().run_until_complete(
            run_persistent(
                read_server_options,
                remote_logging,
            )
        )
    except Exception:
        error_message = traceback.format_exc()
        return 1
    finally:
        _log_lsp_event(
            remote_logging,
            LSPEvent.STOPPED,
            integers={"duration": int(command_timer.stop_in_millisecond())},
            normals={
                **({"exception": error_message} if error_message is not None else {})
            },
        )
