# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module contains an abstract daemon class that manages a Pyre process.

The abstract methods serve as the interfaces that users of the daemon handler must implement.
"""

from __future__ import annotations

import abc

import asyncio
import logging
import traceback
from pathlib import Path
from typing import Dict, Optional, TYPE_CHECKING

from .. import timer
from ..language_server import connections, features, protocol as lsp
from . import (
    backend_arguments,
    background,
    daemon_querier,
    log_lsp_event,
    pyre_server_options,
    server_state as state,
    subscription,
)
from .initialization import (
    async_start_pyre_server,
    BuckStartFailure,
    OtherStartFailure,
    StartSuccess,
)
from .pyre_server_options import PyreServerOptionsReader
from .server_state import ServerState

if TYPE_CHECKING:
    from .persistent import ClientStatusMessageHandler, ClientTypeErrorHandler

LOG: logging.Logger = logging.getLogger(__name__)

CONSECUTIVE_START_ATTEMPT_THRESHOLD: int = 5


class PyreDaemonShutdown(Exception):
    pass


class PyreSubscriptionResponseParser(abc.ABC):
    @abc.abstractmethod
    def parse_response(self, response: str) -> subscription.Response:
        pass


class PyreDaemonLaunchAndSubscribeHandler(background.Task):
    server_options_reader: PyreServerOptionsReader
    remote_logging: Optional[backend_arguments.RemoteLogging]
    server_state: ServerState
    client_status_message_handler: ClientStatusMessageHandler
    client_type_error_handler: ClientTypeErrorHandler
    querier: daemon_querier.AbstractDaemonQuerier
    subscription_response_parser: PyreSubscriptionResponseParser

    def __init__(
        self,
        server_options_reader: PyreServerOptionsReader,
        server_state: ServerState,
        client_status_message_handler: ClientStatusMessageHandler,
        client_type_error_handler: ClientTypeErrorHandler,
        subscription_response_parser: PyreSubscriptionResponseParser,
        querier: daemon_querier.AbstractDaemonQuerier,
        remote_logging: Optional[backend_arguments.RemoteLogging] = None,
    ) -> None:
        self.server_options_reader = server_options_reader
        self.remote_logging = remote_logging
        self.server_state = server_state
        self.client_status_message_handler = client_status_message_handler
        self.client_type_error_handler = client_type_error_handler
        self.querier = querier
        self.subscription_response_parser = subscription_response_parser

    @abc.abstractmethod
    async def handle_type_error_event(
        self,
        type_error_subscription: subscription.TypeErrors,
    ) -> None:
        pass

    @abc.abstractmethod
    async def handle_status_update_event(
        self,
        status_update_subscription: subscription.StatusUpdate,
    ) -> None:
        pass

    async def handle_error_event(self, error_subscription: subscription.Error) -> None:
        message = error_subscription.message
        LOG.info(f"Received error from subscription channel: {message}")
        raise PyreDaemonShutdown(message)

    @abc.abstractmethod
    async def _subscribe(
        self,
        server_input_channel: connections.AsyncTextReader,
        server_output_channel: connections.AsyncTextWriter,
    ) -> None:
        pass

    @abc.abstractmethod
    async def send_open_state(self) -> None:
        pass

    def get_type_errors_availability(self) -> features.TypeErrorsAvailability:
        return self.server_state.server_options.language_server_features.type_errors

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

    @staticmethod
    def read_server_options(
        server_options_reader: pyre_server_options.PyreServerOptionsReader,
        remote_logging: Optional[backend_arguments.RemoteLogging],
    ) -> pyre_server_options.PyreServerOptions:
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

    @staticmethod
    async def _read_server_response(
        server_input_channel: connections.AsyncTextReader,
    ) -> str:
        return await server_input_channel.read_until(separator="\n")

    async def _handle_subscription_body(
        self, subscription_body: subscription.Body
    ) -> None:
        if isinstance(subscription_body, subscription.TypeErrors):
            await self.handle_type_error_event(subscription_body)
        elif isinstance(subscription_body, subscription.StatusUpdate):
            await self.handle_status_update_event(subscription_body)
        elif isinstance(subscription_body, subscription.Error):
            await self.handle_error_event(subscription_body)
        elif isinstance(subscription_body, subscription.IncrementalTelemetry):
            pass

    async def _run_subscription_loop(
        self,
        subscription_name: str,
        server_input_channel: connections.AsyncTextReader,
        server_output_channel: connections.AsyncTextWriter,
    ) -> None:
        while True:
            raw_subscription_response = await self._read_server_response(
                server_input_channel
            )
            subscription_response = self.subscription_response_parser.parse_response(
                raw_subscription_response
            )
            await self._handle_subscription_body(subscription_response.body)

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

    async def connect_and_subscribe(
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
                asyncio.gather(
                    self.send_open_state(),
                    self.client_status_message_handler.log_and_show_status_message_to_client(
                        f"Pyre server at `{project_identifier}` has been initialized.",
                        short_message="Pyre Ready",
                        level=lsp.MessageType.INFO,
                        fallback_to_notification=True,
                    ),
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
            self.server_state.daemon_status.set(state.ServerStatus.READY)
            await self.subscribe(input_channel, output_channel)

    async def launch_and_subscribe(
        self,
        server_options: pyre_server_options.PyreServerOptions,
    ) -> None:
        project_identifier = server_options.project_identifier
        start_arguments = server_options.start_arguments
        socket_path = server_options.get_socket_path()
        flavor = server_options.flavor

        connection_timer = timer.Timer()
        try:
            await self.connect_and_subscribe(
                server_options,
                socket_path,
                connection_timer,
                is_preexisting=True,
            )
        except connections.ConnectionFailure:
            pass

        await self.client_status_message_handler.log_and_show_status_message_to_client(
            f"Starting a new Pyre server at `{project_identifier}` in "
            "the background.",
            short_message="Starting Pyre...",
            level=lsp.MessageType.WARNING,
            fallback_to_notification=True,
        )
        self.server_state.daemon_status.set(state.ServerStatus.STARTING)
        start_status = await async_start_pyre_server(
            server_options.binary,
            start_arguments,
            flavor,
        )
        if isinstance(start_status, StartSuccess):
            await self.connect_and_subscribe(
                server_options,
                socket_path,
                connection_timer,
                is_preexisting=False,
            )
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
        else:
            raise RuntimeError("Impossible type for `start_status`")

    async def run(self) -> None:
        """
        Reread the server start options, which can change due to configuration
        reloading, and run with error logging.
        """
        server_options = self.read_server_options(
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
            self.server_state.daemon_status.set(state.ServerStatus.DISCONNECTED)
            raise
        except PyreDaemonShutdown as error:
            error_message = f"Pyre server shutdown: {error}"
            self.server_state.daemon_status.set(state.ServerStatus.DISCONNECTED)
        except BaseException:
            error_message = traceback.format_exc()
            self.server_state.daemon_status.set(state.ServerStatus.DISCONNECTED)
            raise
        finally:
            if error_message is not None:
                log_lsp_event._log_lsp_event(
                    remote_logging=self.remote_logging,
                    event=log_lsp_event.LSPEvent.DISCONNECTED,
                    integers={"duration": int(session_timer.stop_in_millisecond())},
                    normals={
                        **self._auxiliary_logging_info(server_options),
                        "exception": error_message,
                    },
                )
