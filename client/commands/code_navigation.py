# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This command contains the main logic for the client connecting language servers
and Pyre's code navigation server. It mainly ferries LSP requests back and forth between
editors and the backend, and handles the initialization protocol and searches for appropriate
configurations.
"""

from __future__ import annotations

import asyncio
import json
import logging
import traceback
from typing import Dict, Optional

from .. import (
    backend_arguments,
    background_tasks,
    json_rpc,
    log_lsp_event,
    timer,
    version,
)
from ..language_server import (
    connections,
    daemon_connection,
    features,
    protocol as lsp,
    remote_index,
)
from . import (
    daemon_querier,
    document_formatter,
    initialization,
    launch_and_subscribe_handler,
    pyre_language_server,
    pyre_server_options,
    server_state as state,
    status_message_handler,
    subscription,
    type_error_handler,
)

LOG: logging.Logger = logging.getLogger(__name__)


async def _read_server_response(
    server_input_channel: connections.AsyncTextReader,
) -> str:
    return await server_input_channel.read_until(separator="\n")


class PyreCodeNavigationSubscriptionResponseParser(
    launch_and_subscribe_handler.PyreSubscriptionResponseParser
):
    def parse_response(self, response: str) -> subscription.Response:
        return subscription.Response.parse_code_navigation_response(response)


class PyreCodeNavigationDaemonLaunchAndSubscribeHandler(
    launch_and_subscribe_handler.PyreDaemonLaunchAndSubscribeHandler
):
    querier: daemon_querier.AbstractDaemonQuerier
    output_channel: connections.AsyncTextWriter

    def __init__(
        self,
        server_options_reader: pyre_server_options.PyreServerOptionsReader,
        server_state: state.ServerState,
        client_status_message_handler: status_message_handler.ClientStatusMessageHandler,
        client_type_error_handler: type_error_handler.ClientTypeErrorHandler,
        querier: daemon_querier.AbstractDaemonQuerier,
        output_channel: connections.AsyncTextWriter,
        remote_logging: Optional[backend_arguments.RemoteLogging] = None,
    ) -> None:
        super().__init__(
            server_options_reader,
            server_state,
            client_status_message_handler,
            client_type_error_handler,
            PyreCodeNavigationSubscriptionResponseParser(),
            remote_logging,
        )
        self.querier = querier
        self.output_channel = output_channel

    async def write_telemetry(
        self,
        parameters: Dict[str, object],
    ) -> None:
        should_write_telemetry = self.server_state.server_options.language_server_features.telemetry.is_enabled()
        if should_write_telemetry:
            parameters = dict(parameters)
            parameters["project_identifier"] = (
                self.server_state.server_options.project_identifier
            )
            parameters["enabled_features"] = {
                "type_errors_enabled": self.server_state.server_options.language_server_features.type_errors.is_enabled()
            }
            await lsp.write_json_rpc_ignore_connection_error(
                self.output_channel,
                json_rpc.Request(
                    activity_key=None,
                    method="telemetry/event",
                    parameters=json_rpc.ByNameParameters(parameters),
                ),
            )

    def get_type_errors_availability(self) -> features.TypeErrorsAvailability:
        return self.server_state.server_options.language_server_features.type_errors

    async def handle_type_error_event(
        self, type_error_subscription: subscription.TypeErrors
    ) -> None:
        # We currently do not broadcast any type errors on the CodeNav server - the intent is to be
        # as lazy as possible and only provide actionable information to users. The error is intended
        # to demonstrate that contract.
        raise RuntimeError(
            "The Pyre code navigation server is not expected to broadcast type errors at the moment."
        )

    async def handle_status_update_event(
        self, status_update_subscription: subscription.StatusUpdate
    ) -> None:
        flavor_simple_name = self.server_state.server_options.flavor.simple_name()
        if not self.get_type_errors_availability().is_disabled():
            await self.client_type_error_handler.clear_type_errors_for_client()
        if status_update_subscription.kind == "Stop":
            self.server_state.status_tracker.set_status(
                state.ConnectionStatus.DISCONNECTED
            )
            await self.client_status_message_handler.log_and_show_status_message_to_client(
                "The Pyre code-navigation server has stopped.",
                short_message=f"{flavor_simple_name} (stopped)",
                level=lsp.MessageType.WARNING,
            )
            raise launch_and_subscribe_handler.PyreDaemonShutdown(
                status_update_subscription.message or ""
            )
        elif status_update_subscription.kind == "BusyBuilding":
            self.server_state.status_tracker.set_status(
                state.ConnectionStatus.BUCK_BUILDING
            )
            await self.client_status_message_handler.log_and_show_status_message_to_client(
                "The Pyre code-navigation server is busy re-building the project...",
                short_message=f"{flavor_simple_name} (building)",
                level=lsp.MessageType.WARNING,
            )
        elif status_update_subscription.kind == "BusyChecking":
            self.server_state.status_tracker.set_status(
                state.ConnectionStatus.INCREMENTAL_CHECK
            )
            await self.client_status_message_handler.log_and_show_status_message_to_client(
                "The Pyre code-navigation server is busy re-type-checking the project...",
                short_message=f"{flavor_simple_name} (checking)",
                level=lsp.MessageType.WARNING,
            )
        elif status_update_subscription.kind == "Idle":
            self.server_state.status_tracker.set_status(state.ConnectionStatus.READY)
            await self.client_status_message_handler.log_and_show_status_message_to_client(
                "Pyre's code navigation server has completed an incremental check and is currently watching on further source changes.",
                short_message=f"{flavor_simple_name} Ready",
                level=lsp.MessageType.INFO,
            )

    async def _subscribe(
        self,
        server_input_channel: connections.AsyncTextReader,
        server_output_channel: connections.AsyncTextWriter,
    ) -> None:
        subscription_name = "code_navigation"
        await server_output_channel.write('["Subscription", ["Subscribe"]]\n')
        first_response = await _read_server_response(server_input_channel)
        if json.loads(first_response) != ["Ok"]:
            raise ValueError(
                f"Unexpected server response to Subscription: {first_response!r}"
            )
        await self._run_subscription_loop(
            subscription_name,
            server_input_channel,
            server_output_channel,
        )

    async def client_setup(self) -> None:
        register_client_request = await self.querier.handle_register_client()
        if isinstance(
            register_client_request, daemon_connection.DaemonConnectionFailure
        ):
            error_message = (
                f"error occurred in client_setup request: {register_client_request}"
            )
            LOG.warning(f"Failed to register daemon querier: {register_client_request}")
        else:
            error_message = None
            LOG.info("Registered daemon querier.")
        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "clientSetup",
                "error_message": error_message,
                **self.server_state.status_tracker.get_status().as_telemetry_dict(),
            }
        )

        results = await asyncio.gather(
            *[
                self.querier.handle_file_opened(path, document.code)
                for path, document in self.server_state.opened_documents.items()
            ]
        )
        if len(results) > 0:
            LOG.info(f"Sent {len(results)} open messages to daemon for existing state.")

    async def client_teardown(self) -> None:
        dispose_client_request = await self.querier.handle_dispose_client()
        if isinstance(
            dispose_client_request, daemon_connection.DaemonConnectionFailure
        ):
            error_message = (
                f"error occurred in client_teardown request: {dispose_client_request}"
            )
            LOG.warning(
                f"Failed to dispose of daemon querier: {dispose_client_request}"
            )
        else:
            error_message = None
            LOG.info("Disposed daemon querier.")
        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "clientSetup",
                "error_message": error_message,
                **self.server_state.status_tracker.get_status().as_telemetry_dict(),
            }
        )


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
    server_info = lsp.Info(name="pyre-codenav", version=version.__version__)
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


async def async_run_code_navigation_client(
    server_options_reader: pyre_server_options.PyreServerOptionsReader,
    remote_logging: Optional[backend_arguments.RemoteLogging],
    index: remote_index.AbstractRemoteIndex,
    document_formatter: Optional[document_formatter.AbstractDocumentFormatter],
) -> int:
    initial_server_options = pyre_server_options.read_server_options(
        server_options_reader, remote_logging
    )
    stdin, stdout = await connections.create_async_stdin_stdout()
    LOG.info("Waiting on LSP initialization request")
    initialize_result = await initialization.async_try_initialize_loop(
        stdin,
        stdout,
        remote_logging,
        compute_initialize_result=lambda parameters: process_initialize_request(
            parameters, initial_server_options.language_server_features
        ),
    )
    if isinstance(initialize_result, initialization.InitializationExit):
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
        client_register_event=asyncio.Event(),
    )

    codenav_querier = daemon_querier.CodeNavigationDaemonQuerier(
        server_state=server_state
    )

    index_querier = daemon_querier.RemoteIndexBackedQuerier(
        server_state.status_tracker, codenav_querier, index
    )
    client_type_error_handler = type_error_handler.ClientTypeErrorHandler(
        stdout, server_state, remote_logging
    )
    server = pyre_language_server.PyreLanguageServerDispatcher(
        input_channel=stdin,
        output_channel=stdout,
        server_state=server_state,
        daemon_manager=background_tasks.TaskManager(
            PyreCodeNavigationDaemonLaunchAndSubscribeHandler(
                server_options_reader=server_options_reader,
                remote_logging=remote_logging,
                server_state=server_state,
                client_status_message_handler=status_message_handler.ClientStatusMessageHandler(
                    stdout, server_state
                ),
                querier=codenav_querier,
                client_type_error_handler=client_type_error_handler,
                output_channel=stdout,
            )
        ),
        api=pyre_language_server.PyreLanguageServer(
            output_channel=stdout,
            server_state=server_state,
            querier=codenav_querier,
            index_querier=index_querier,
            document_formatter=document_formatter,
            client_type_error_handler=client_type_error_handler,
        ),
    )
    return await server.run()


def run(
    server_options_reader: pyre_server_options.PyreServerOptionsReader,
    remote_logging: Optional[backend_arguments.RemoteLogging],
    index: remote_index.AbstractRemoteIndex,
    document_formatter: Optional[document_formatter.AbstractDocumentFormatter],
) -> int:
    command_timer = timer.Timer()
    error_message: Optional[str] = None
    try:
        return asyncio.run(
            async_run_code_navigation_client(
                server_options_reader,
                remote_logging,
                index,
                document_formatter,
            )
        )
    except Exception:
        error_message = traceback.format_exc()
        LOG.exception("Uncaught error in code_navigation.run")
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
