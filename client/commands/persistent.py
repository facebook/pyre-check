# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module represents the top level class and entry point for the LSP client
to interact with the Pyre Server. The LSP client will invoke the top level
method here, which will instantiate a running instance of a persistent Pyre
client that will interact with the pyre server.

The meaning of "persistent" in this context is to illustrate that the
connection to the pyre server is non-transient and is designed to stay alive
during the entire duration of a user's interaction with Vscode.

The main responsibilities of this class are:

1. Instantiating an instance of the persistent Pyre client and ensuring
re-launch of the Pyre server in the scenario that it goes down.

2. Handling subscriptions to the Pyre server - listening to events and status
updates from the server, and sending updates back to the LSP to display
appropriate notifications to the user. These events include type errors,
status updates, and error messages.

"""


from __future__ import annotations

import asyncio
import dataclasses
import logging
import os
import traceback
from pathlib import Path
from typing import Dict, List, Optional, Sequence

from .. import error, json_rpc, timer, version
from ..language_server import connections, features, protocol as lsp
from . import (
    backend_arguments,
    background,
    daemon_querier,
    incremental,
    launch_and_subscribe_handler,
    log_lsp_event,
    pyre_language_server,
    pyre_server_options,
    server_state as state,
    subscription,
)
from .initialization import async_try_initialize_loop, InitializationExit

from .server_state import ServerState


LOG: logging.Logger = logging.getLogger(__name__)

COMMAND_NAME = "persistent"


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

    def log(
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

    async def log_and_show_status_message_to_client(
        self,
        message: str,
        short_message: Optional[str] = None,
        level: lsp.MessageType = lsp.MessageType.INFO,
        fallback_to_notification: bool = False,
    ) -> None:
        self.log(message, short_message, level)
        await self.show_status_message_to_client(
            message, short_message, level, fallback_to_notification
        )


@dataclasses.dataclass(frozen=True)
class ClientTypeErrorHandler:
    client_output_channel: connections.AsyncTextWriter
    server_state: ServerState
    remote_logging: Optional[backend_arguments.RemoteLogging] = None

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

    async def show_type_errors_to_client(self) -> None:
        for path, diagnostics in self.server_state.diagnostics.items():
            await _publish_diagnostics(self.client_output_channel, path, diagnostics)

    async def show_overlay_type_errors(
        self,
        path: Path,
        type_errors: Sequence[error.Error],
    ) -> None:
        LOG.info(
            f"Refreshing type errors at path {path}. "
            f"Total number of type errors is {len(type_errors)}."
        )
        diagnostics_by_path = type_errors_to_diagnostics(type_errors)
        diagnostics = diagnostics_by_path.get(path, [])
        await _publish_diagnostics(self.client_output_channel, path, diagnostics)


READY_MESSAGE: str = "Pyre has completed an incremental check and is currently watching on further source changes."
READY_SHORT: str = "Pyre Ready"


class PyrePersistentSubscriptionResponseParser(
    launch_and_subscribe_handler.PyreSubscriptionResponseParser
):
    def parse_response(self, response: str) -> subscription.Response:
        return subscription.Response.parse(response)


class PyrePersistentDaemonLaunchAndSubscribeHandler(
    launch_and_subscribe_handler.PyreDaemonLaunchAndSubscribeHandler
):
    def __init__(
        self,
        server_options_reader: pyre_server_options.PyreServerOptionsReader,
        server_state: ServerState,
        client_status_message_handler: ClientStatusMessageHandler,
        client_type_error_handler: ClientTypeErrorHandler,
        querier: daemon_querier.AbstractDaemonQuerier,
        remote_logging: Optional[backend_arguments.RemoteLogging] = None,
    ) -> None:
        super().__init__(
            server_options_reader,
            server_state,
            client_status_message_handler,
            client_type_error_handler,
            PyrePersistentSubscriptionResponseParser(),
            querier,
            remote_logging,
        )

    async def handle_type_error_event(
        self,
        type_error_subscription: subscription.TypeErrors,
    ) -> None:
        await self.client_type_error_handler.clear_type_errors_for_client()
        self.client_type_error_handler.update_type_errors(
            type_error_subscription.errors
        )
        self.server_state.status_tracker.set_status(state.ConnectionStatus.READY)
        await self.client_type_error_handler.show_type_errors_to_client()
        await self.client_status_message_handler.log_and_show_status_message_to_client(
            READY_MESSAGE,
            short_message=READY_SHORT,
            level=lsp.MessageType.INFO,
        )

    async def handle_status_update_event(
        self,
        status_update_subscription: subscription.StatusUpdate,
    ) -> None:
        if not self.get_type_errors_availability().is_disabled():
            await self.client_type_error_handler.clear_type_errors_for_client()
        if status_update_subscription.kind == "Rebuilding":
            self.server_state.status_tracker.set_status(
                state.ConnectionStatus.BUCK_BUILDING
            )
            await self.client_status_message_handler.log_and_show_status_message_to_client(
                "Pyre is busy rebuilding the project for type checking...",
                short_message="Pyre (waiting for Buck)",
                level=lsp.MessageType.WARNING,
            )
        elif status_update_subscription.kind == "Rechecking":
            self.server_state.status_tracker.set_status(
                state.ConnectionStatus.INCREMENTAL_CHECK
            )
            await self.client_status_message_handler.log_and_show_status_message_to_client(
                "Pyre is busy re-type-checking the project...",
                short_message="Pyre (checking)",
                level=lsp.MessageType.WARNING,
            )
        elif status_update_subscription.kind == "Ready":
            self.server_state.status_tracker.set_status(state.ConnectionStatus.READY)
            await self.client_status_message_handler.log_and_show_status_message_to_client(
                READY_MESSAGE,
                short_message=READY_SHORT,
                level=lsp.MessageType.INFO,
            )

    async def _subscribe_to_type_errors(
        self,
        server_input_channel: connections.AsyncTextReader,
        server_output_channel: connections.AsyncTextWriter,
    ) -> None:
        subscription_name = f"persistent_{os.getpid()}"
        await server_output_channel.write(
            f'["SubscribeToTypeErrors", "{subscription_name}"]\n'
        )
        first_response = await launch_and_subscribe_handler.PyreDaemonLaunchAndSubscribeHandler._read_server_response(
            server_input_channel
        )
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
        await self._read_server_response(server_input_channel)
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

    async def send_open_state(self) -> None:
        """Unnecessary to send open state on server instantiation for persistent server."""
        pass


async def run_persistent(
    server_options_reader: pyre_server_options.PyreServerOptionsReader,
    remote_logging: Optional[backend_arguments.RemoteLogging],
) -> int:
    initial_server_options = launch_and_subscribe_handler.PyreDaemonLaunchAndSubscribeHandler.read_server_options(
        server_options_reader, remote_logging
    )
    stdin, stdout = await connections.create_async_stdin_stdout()

    initialize_result = await async_try_initialize_loop(
        stdin,
        stdout,
        remote_logging,
        compute_initialize_result=lambda parameters: process_initialize_request(
            parameters, initial_server_options.language_server_features
        ),
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
    querier = daemon_querier.PersistentDaemonQuerier(
        server_state=server_state,
    )
    client_type_error_handler = ClientTypeErrorHandler(
        stdout, server_state, remote_logging
    )

    server = pyre_language_server.PyreLanguageServerDispatcher(
        input_channel=stdin,
        output_channel=stdout,
        server_state=server_state,
        daemon_manager=background.TaskManager(
            PyrePersistentDaemonLaunchAndSubscribeHandler(
                server_options_reader=server_options_reader,
                remote_logging=remote_logging,
                server_state=server_state,
                client_status_message_handler=ClientStatusMessageHandler(
                    stdout, server_state
                ),
                querier=querier,
                client_type_error_handler=client_type_error_handler,
            )
        ),
        api=pyre_language_server.PyreLanguageServerApi(
            output_channel=stdout,
            server_state=server_state,
            querier=querier,
            client_type_error_handler=client_type_error_handler,
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
