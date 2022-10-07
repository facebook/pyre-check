# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


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

from .. import error, identifiers, json_rpc, log, timer, version
from . import (
    backend_arguments,
    background,
    connections,
    incremental,
    language_server_features as features,
    language_server_protocol as lsp,
    log_lsp_event,
    pyre_language_server,
    pyre_server_options,
    request_handler,
    server_event,
    server_state as state,
    start,
    subscription,
)

from .pyre_server_options import PyreServerOptionsReader
from .server_state import ServerState


LOG: logging.Logger = logging.getLogger(__name__)

COMMAND_NAME = "persistent"

CONSECUTIVE_START_ATTEMPT_THRESHOLD: int = 5


def read_server_options(
    server_options_reader: pyre_server_options.PyreServerOptionsReader,
    remote_logging: Optional[backend_arguments.RemoteLogging],
) -> "pyre_server_options.PyreServerOptions":
    try:
        LOG.info("Reading Pyre server configurations...")
        return server_options_reader()
    except Exception:
        log_lsp_event._log_lsp_event(
            remote_logging=remote_logging,
            event=log_lsp_event.LSPEvent.NOT_CONFIGURED,
            normals={
                "exception": traceback.format_exc(),
            },
        )
        raise


def process_initialize_request(
    parameters: lsp.InitializeParameters,
    language_server_features: Optional[features.LanguageServerFeatures] = None,
) -> lsp.InitializeResult:
    LOG.info(
        f"Received initialization request from {parameters.client_info} "
        f" (pid = {parameters.process_id})"
    )
    if language_server_features is None:
        language_server_features = features.LanguageServerFeatures()
    server_info = lsp.Info(name="pyre", version=version.__version__)
    server_capabilities = lsp.ServerCapabilities(
        text_document_sync=lsp.TextDocumentSyncOptions(
            open_close=True,
            change=lsp.TextDocumentSyncKind.FULL,
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
    server_options: pyre_server_options.PyreServerOptions,
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
                await pyre_language_server._wait_for_exit(input_channel, output_channel)
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


class ClientStatusMessageHandler:
    def __init__(
        self,
        client_output_channel: connections.AsyncTextWriter,
        server_state: ServerState,
    ) -> None:
        self.client_output_channel = client_output_channel
        self.server_state = server_state

    def get_status_updates_availability(self) -> features.StatusUpdatesAvailability:
        return self.server_state.server_options.language_server_features.status_updates

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
        if self.get_status_updates_availability().is_disabled():
            return
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


class ClientTypeErrorHandler:
    def __init__(
        self,
        client_output_channel: connections.AsyncTextWriter,
        server_state: ServerState,
        remote_logging: Optional[backend_arguments.RemoteLogging] = None,
    ) -> None:
        self.client_output_channel = client_output_channel
        self.remote_logging = remote_logging
        self.server_state = server_state

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
        log_lsp_event._log_lsp_event(
            self.remote_logging,
            log_lsp_event.LSPEvent.COVERED,
            integers={"duration": int(last_update_timer.stop_in_millisecond())},
        )
        # Reset the timestamp to avoid duplicate counting
        last_update_timer.reset()

    async def show_type_errors_to_client(self) -> None:
        for path, diagnostics in self.server_state.diagnostics.items():
            await _publish_diagnostics(self.client_output_channel, path, diagnostics)
        self.server_state.last_diagnostic_update_timer.reset()


READY_MESSAGE: str = "Pyre has completed an incremental check and is currently watching on further source changes."
READY_SHORT: str = "Pyre Ready"


class PyreDaemonLaunchAndSubscribeHandler(background.Task):
    server_options_reader: PyreServerOptionsReader
    remote_logging: Optional[backend_arguments.RemoteLogging]
    server_state: ServerState
    client_status_message_handler: ClientStatusMessageHandler
    client_type_error_handler: ClientTypeErrorHandler

    def __init__(
        self,
        server_options_reader: PyreServerOptionsReader,
        server_state: ServerState,
        client_status_message_handler: ClientStatusMessageHandler,
        client_type_error_handler: ClientTypeErrorHandler,
        remote_logging: Optional[backend_arguments.RemoteLogging] = None,
    ) -> None:
        self.server_options_reader = server_options_reader
        self.remote_logging = remote_logging
        self.server_state = server_state
        self.client_status_message_handler = client_status_message_handler
        self.client_type_error_handler = client_type_error_handler

    def get_type_errors_availability(self) -> features.TypeErrorsAvailability:
        return self.server_state.server_options.language_server_features.type_errors

    async def handle_type_error_subscription(
        self, type_error_subscription: subscription.TypeErrors
    ) -> None:
        await self.client_type_error_handler.clear_type_errors_for_client()
        self.client_type_error_handler.update_type_errors(
            type_error_subscription.errors
        )
        self.server_state.server_last_status = state.ServerStatus.READY
        await self.client_type_error_handler.show_type_errors_to_client()
        await self.client_status_message_handler.log_and_show_status_message_to_client(
            READY_MESSAGE,
            short_message=READY_SHORT,
            level=lsp.MessageType.INFO,
        )

    async def handle_status_update_subscription(
        self, status_update_subscription: subscription.StatusUpdate
    ) -> None:
        if not self.get_type_errors_availability().is_disabled():
            await self.client_type_error_handler.clear_type_errors_for_client()
        if status_update_subscription.kind == "Rebuilding":
            self.server_state.server_last_status = state.ServerStatus.BUCK_BUILDING
            await self.client_status_message_handler.log_and_show_status_message_to_client(
                "Pyre is busy rebuilding the project for type checking...",
                short_message="Pyre (waiting for Buck)",
                level=lsp.MessageType.WARNING,
            )
        elif status_update_subscription.kind == "Rechecking":
            self.server_state.server_last_status = state.ServerStatus.INCREMENTAL_CHECK
            await self.client_status_message_handler.log_and_show_status_message_to_client(
                "Pyre is busy re-type-checking the project...",
                short_message="Pyre (checking)",
                level=lsp.MessageType.WARNING,
            )
        elif status_update_subscription.kind == "Ready":
            self.server_state.server_last_status = state.ServerStatus.READY
            await self.client_status_message_handler.log_and_show_status_message_to_client(
                READY_MESSAGE,
                short_message=READY_SHORT,
                level=lsp.MessageType.INFO,
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

    async def _run_subscription_loop(
        self,
        subscription_name: str,
        server_input_channel: connections.AsyncTextReader,
        server_output_channel: connections.AsyncTextWriter,
    ) -> None:
        while True:
            raw_subscription_response = await _read_server_response(
                server_input_channel
            )
            subscription_response = subscription.Response.parse(
                raw_subscription_response
            )
            if subscription_name == subscription_response.name:
                await self._handle_subscription_body(subscription_response.body)

    async def _subscribe_to_type_errors(
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
        self.client_type_error_handler.update_type_errors(initial_type_errors)
        await self.client_type_error_handler.show_type_errors_to_client()
        await self._run_subscription_loop(
            subscription_name,
            server_input_channel,
            server_output_channel,
        )

    async def _subscribe_to_state_changes(
        self,
        server_input_channel: connections.AsyncTextReader,
        server_output_channel: connections.AsyncTextWriter,
    ) -> None:
        subscription_name = f"persistent_{os.getpid()}"
        await server_output_channel.write(
            f'["SubscribeToStateChanges", "{subscription_name}"]\n'
        )
        first_response = await _read_server_response(server_input_channel)
        if json.loads(first_response) != ["Ok"]:
            raise ValueError(
                f"Unexpected server response to SubscribeToStateChanges: {first_response!r}"
            )
        await self._run_subscription_loop(
            subscription_name,
            server_input_channel,
            server_output_channel,
        )

    async def _subscribe(
        self,
        server_input_channel: connections.AsyncTextReader,
        server_output_channel: connections.AsyncTextWriter,
    ) -> None:
        if self.get_type_errors_availability().is_enabled():
            await self._subscribe_to_type_errors(
                server_input_channel,
                server_output_channel,
            )
        else:
            await self._subscribe_to_state_changes(
                server_input_channel,
                server_output_channel,
            )

    async def subscribe(
        self,
        server_input_channel: connections.AsyncTextReader,
        server_output_channel: connections.AsyncTextWriter,
    ) -> None:
        try:
            await self._subscribe(
                server_input_channel,
                server_output_channel,
            )
        finally:
            await self.client_status_message_handler.show_status_message_to_client(
                "Lost connection to the background Pyre Server. "
                "This usually happens when Pyre detect changes in project which "
                "it was not able to handle incrementally. "
                "A new Pyre server will be started next time you open or save "
                "a .py file",
                short_message="Pyre Stopped",
                level=lsp.MessageType.ERROR,
                fallback_to_notification=True,
            )
            await self.client_type_error_handler.clear_type_errors_for_client()
            self.server_state.diagnostics = {}

    @staticmethod
    def _auxiliary_logging_info(
        server_options: pyre_server_options.PyreServerOptions,
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
        server_options: pyre_server_options.PyreServerOptions,
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
                await self.client_status_message_handler.log_and_show_status_message_to_client(
                    "Established connection with existing Pyre server at "
                    f"`{project_identifier}`.",
                    short_message="Pyre Ready",
                    level=lsp.MessageType.INFO,
                    fallback_to_notification=True,
                )
            else:
                await self.client_status_message_handler.log_and_show_status_message_to_client(
                    f"Pyre server at `{project_identifier}` has been initialized.",
                    short_message="Pyre Ready",
                    level=lsp.MessageType.INFO,
                    fallback_to_notification=True,
                )
            self.server_state.consecutive_start_failure = 0
            self.server_state.is_user_notified_on_buck_failure = False
            log_lsp_event._log_lsp_event(
                remote_logging=self.remote_logging,
                event=log_lsp_event.LSPEvent.CONNECTED,
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
            self.server_state.server_last_status = state.ServerStatus.READY
            await self.subscribe(input_channel, output_channel)

    async def launch_and_subscribe(
        self,
        server_options: pyre_server_options.PyreServerOptions,
    ) -> state.ServerStatus:
        project_identifier = server_options.project_identifier
        start_arguments = server_options.start_arguments
        socket_path = server_options.get_socket_path()
        flavor = server_options.flavor

        connection_timer = timer.Timer()
        try:
            await self._try_connect_and_subscribe(
                server_options,
                socket_path,
                connection_timer,
                is_preexisting=True,
            )
            # Unreachable code because _try_connect_and_subscribe may never terminate.
            return state.ServerStatus.READY
        except connections.ConnectionFailure:
            pass

        await self.client_status_message_handler.log_and_show_status_message_to_client(
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
            return state.ServerStatus.READY
        elif isinstance(start_status, BuckStartFailure):
            # Buck start failures are intentionally not counted towards
            # `consecutive_start_failure` -- they happen far too often in practice
            # so we do not want them to trigger suspensions.
            log_lsp_event._log_lsp_event(
                remote_logging=self.remote_logging,
                event=log_lsp_event.LSPEvent.NOT_CONNECTED,
                integers={"duration": int(connection_timer.stop_in_millisecond())},
                normals={
                    **self._auxiliary_logging_info(server_options),
                    "exception": str(start_status.message),
                },
            )
            if not self.server_state.is_user_notified_on_buck_failure:
                await self.client_status_message_handler.show_notification_message_to_client(
                    f"Cannot start a new Pyre server at `{project_identifier}` "
                    "due to Buck failure. If you added or changed a target, "
                    "make sure the target file is parsable and the owning "
                    "targets are buildable by Buck. If you removed a target, "
                    "make sure that target is not explicitly referenced from the "
                    "Pyre configuration file of the containing project.",
                    level=lsp.MessageType.ERROR,
                )
                self.server_state.is_user_notified_on_buck_failure = True
            await self.client_status_message_handler.show_status_message_to_client(
                f"Cannot start a new Pyre server at `{project_identifier}`. "
                f"{start_status.message}",
                short_message="Pyre Stopped",
                level=lsp.MessageType.INFO,
                fallback_to_notification=False,
            )
            return state.ServerStatus.NOT_CONNECTED
        elif isinstance(start_status, OtherStartFailure):
            self.server_state.consecutive_start_failure += 1
            if (
                self.server_state.consecutive_start_failure
                < CONSECUTIVE_START_ATTEMPT_THRESHOLD
            ):
                log_lsp_event._log_lsp_event(
                    remote_logging=self.remote_logging,
                    event=log_lsp_event.LSPEvent.NOT_CONNECTED,
                    integers={"duration": int(connection_timer.stop_in_millisecond())},
                    normals={
                        **self._auxiliary_logging_info(server_options),
                        "exception": str(start_status.detail),
                    },
                )
                await self.client_status_message_handler.show_status_message_to_client(
                    f"Cannot start a new Pyre server at `{project_identifier}`. "
                    f"{start_status.message}",
                    short_message="Pyre Stopped",
                    level=lsp.MessageType.INFO,
                    fallback_to_notification=True,
                )
                return state.ServerStatus.NOT_CONNECTED
            else:
                log_lsp_event._log_lsp_event(
                    remote_logging=self.remote_logging,
                    event=log_lsp_event.LSPEvent.SUSPENDED,
                    integers={"duration": int(connection_timer.stop_in_millisecond())},
                    normals={
                        **self._auxiliary_logging_info(server_options),
                        "exception": str(start_status.detail),
                    },
                )
                await self.client_status_message_handler.show_status_message_to_client(
                    f"Pyre server restart at `{project_identifier}` has been "
                    "failing repeatedly. Disabling The Pyre plugin for now.",
                    short_message="Pyre Disabled",
                    level=lsp.MessageType.ERROR,
                    fallback_to_notification=True,
                )
                return state.ServerStatus.SUSPENDED
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
            self.server_state.server_last_status = state.ServerStatus.DISCONNECTED
            raise
        except PyreDaemonShutdown as error:
            error_message = f"Pyre server shutdown: {error}"
            self.server_state.server_last_status = state.ServerStatus.DISCONNECTED
        except BaseException:
            error_message = traceback.format_exc()
            self.server_state.server_last_status = state.ServerStatus.DISCONNECTED
            raise
        finally:
            log_lsp_event._log_lsp_event(
                remote_logging=self.remote_logging,
                event=log_lsp_event.LSPEvent.DISCONNECTED,
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


async def try_initialize_loop(
    server_options: pyre_server_options.PyreServerOptions,
    input_channel: connections.AsyncTextReader,
    output_channel: connections.AsyncTextWriter,
    remote_logging: Optional[backend_arguments.RemoteLogging],
) -> InitializationSuccess | InitializationExit:
    while True:
        initialize_result = await try_initialize(
            input_channel, output_channel, server_options
        )
        if isinstance(initialize_result, InitializationExit):
            LOG.info("Received exit request before initialization.")
            return initialize_result
        elif isinstance(initialize_result, InitializationSuccess):
            LOG.info("Initialization successful.")
            return initialize_result
        elif isinstance(initialize_result, InitializationFailure):
            exception = initialize_result.exception
            message = (
                str(exception) if exception is not None else "ignoring notification"
            )
            LOG.info(f"Initialization failed: {message}")
            log_lsp_event._log_lsp_event(
                remote_logging=remote_logging,
                event=log_lsp_event.LSPEvent.NOT_INITIALIZED,
                normals=(
                    {
                        "exception": message,
                    }
                ),
            )
            # Loop until we get either InitializeExit or InitializeSuccess
        else:
            raise RuntimeError("Cannot determine the type of initialize_result")


async def run_persistent(
    server_options_reader: pyre_server_options.PyreServerOptionsReader,
    remote_logging: Optional[backend_arguments.RemoteLogging],
) -> int:
    initial_server_options = read_server_options(
        server_options_reader, remote_logging=None
    )
    stdin, stdout = await connections.create_async_stdin_stdout()

    initialize_result = await try_initialize_loop(
        initial_server_options, stdin, stdout, remote_logging
    )
    if isinstance(initialize_result, InitializationExit):
        return 0

    client_info = initialize_result.client_info
    log_lsp_event._log_lsp_event(
        remote_logging=remote_logging,
        event=log_lsp_event.LSPEvent.INITIALIZED,
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
    server_state = state.ServerState(
        client_capabilities=client_capabilities,
        server_options=initial_server_options,
    )
    server = pyre_language_server.PyreLanguageServer(
        input_channel=stdin,
        output_channel=stdout,
        server_state=server_state,
        pyre_manager=background.TaskManager(
            PyreDaemonLaunchAndSubscribeHandler(
                server_options_reader=server_options_reader,
                remote_logging=remote_logging,
                server_state=server_state,
                client_status_message_handler=ClientStatusMessageHandler(
                    stdout, server_state
                ),
                client_type_error_handler=ClientTypeErrorHandler(
                    stdout, server_state, remote_logging
                ),
            )
        ),
        handler=request_handler.RequestHandler(
            server_state=server_state,
        ),
    )
    return await server.run()


def run(
    read_server_options: pyre_server_options.PyreServerOptionsReader,
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
        LOG.exception("Uncaught error in persistent.run")
        return 1
    finally:
        log_lsp_event._log_lsp_event(
            remote_logging,
            log_lsp_event.LSPEvent.STOPPED,
            integers={"duration": int(command_timer.stop_in_millisecond())},
            normals={
                **({"exception": error_message} if error_message is not None else {})
            },
        )
