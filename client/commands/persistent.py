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

The main responsibilities of this class is to:

-   Instantiate an instance of the persistent Pyre client and ensure
    re-launch of the Pyre server in the scenario that it goes down.

"""


from __future__ import annotations

import asyncio
import logging
import os
import traceback

from typing import Optional

from .. import backend_arguments, background_tasks, log_lsp_event, timer, version
from ..language_server import connections, features, protocol as lsp
from . import (
    daemon_querier,
    incremental,
    launch_and_subscribe_handler,
    pyre_language_server,
    pyre_server_options,
    server_state as state,
    status_message_handler,
    subscription,
    type_error_handler,
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


READY_MESSAGE: str = "Pyre has completed an incremental check and is currently watching on further source changes."


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
        client_status_message_handler: status_message_handler.ClientStatusMessageHandler,
        client_type_error_handler: type_error_handler.ClientTypeErrorHandler,
        remote_logging: Optional[backend_arguments.RemoteLogging] = None,
    ) -> None:
        super().__init__(
            server_options_reader,
            server_state,
            client_status_message_handler,
            client_type_error_handler,
            PyrePersistentSubscriptionResponseParser(),
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
            short_message=f"{self.server_state.server_options.flavor.simple_name()} Ready",
            level=lsp.MessageType.INFO,
        )

    async def handle_status_update_event(
        self,
        status_update_subscription: subscription.StatusUpdate,
    ) -> None:
        flavor_simple_name = self.server_state.server_options.flavor.simple_name()
        if not self.get_type_errors_availability().is_disabled():
            await self.client_type_error_handler.clear_type_errors_for_client()
        if status_update_subscription.kind == "Rebuilding":
            self.server_state.status_tracker.set_status(
                state.ConnectionStatus.BUCK_BUILDING
            )
            await self.client_status_message_handler.log_and_show_status_message_to_client(
                "Pyre is busy rebuilding the project for type checking...",
                short_message=f"{flavor_simple_name} (waiting for Buck)",
                level=lsp.MessageType.WARNING,
            )
        elif status_update_subscription.kind == "Rechecking":
            self.server_state.status_tracker.set_status(
                state.ConnectionStatus.INCREMENTAL_CHECK
            )
            await self.client_status_message_handler.log_and_show_status_message_to_client(
                "Pyre is busy re-type-checking the project...",
                short_message=f"{flavor_simple_name} (checking)",
                level=lsp.MessageType.WARNING,
            )
        elif status_update_subscription.kind == "Ready":
            self.server_state.status_tracker.set_status(state.ConnectionStatus.READY)
            await self.client_status_message_handler.log_and_show_status_message_to_client(
                READY_MESSAGE,
                short_message=f"{flavor_simple_name} Ready",
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
        initial_type_errors = incremental.parse_type_error_response(
            first_response
        ).errors
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

    async def client_setup(self) -> None:
        """Persistent server does not require any client-side setup"""
        pass

    async def client_teardown(self) -> None:
        """Persistent server does not require any client-side teardown"""
        pass


async def run_persistent(
    server_options_reader: pyre_server_options.PyreServerOptionsReader,
    remote_logging: Optional[backend_arguments.RemoteLogging],
) -> int:
    initial_server_options = pyre_server_options.read_server_options(
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
    log_lsp_event.log(
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
    client_type_error_handler = type_error_handler.ClientTypeErrorHandler(
        stdout, server_state, remote_logging
    )

    server = pyre_language_server.PyreLanguageServerDispatcher(
        input_channel=stdin,
        output_channel=stdout,
        server_state=server_state,
        daemon_manager=background_tasks.TaskManager(
            PyrePersistentDaemonLaunchAndSubscribeHandler(
                server_options_reader=server_options_reader,
                remote_logging=remote_logging,
                server_state=server_state,
                client_status_message_handler=status_message_handler.ClientStatusMessageHandler(
                    stdout, server_state
                ),
                client_type_error_handler=client_type_error_handler,
            )
        ),
        api=pyre_language_server.PyreLanguageServer(
            output_channel=stdout,
            server_state=server_state,
            querier=querier,
            index_querier=daemon_querier.EmptyQuerier(server_state=server_state),
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
        log_lsp_event.log(
            remote_logging,
            log_lsp_event.LSPEvent.STOPPED,
            integers={"duration": int(command_timer.stop_in_millisecond())},
            normals={
                **({"exception": error_message} if error_message is not None else {})
            },
        )
