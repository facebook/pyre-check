# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

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
from typing import Dict, Optional

from .. import backend_arguments, background_tasks, log_lsp_event, timer
from ..language_server import connections, features, protocol as lsp
from . import (
    pyre_server_options,
    server_state as state,
    status_message_handler,
    subscription,
    type_error_handler,
)
from .initialization import (
    async_start_pyre_server,
    BuckStartFailure,
    OtherStartFailure,
    StartSuccess,
)
from .pyre_server_options import PyreServerOptionsReader
from .server_state import ServerState

LOG: logging.Logger = logging.getLogger(__name__)

CONSECUTIVE_START_ATTEMPT_THRESHOLD: int = 5


class PyreDaemonShutdown(Exception):
    pass


class PyreSubscriptionResponseParser(abc.ABC):
    @abc.abstractmethod
    def parse_response(self, response: str) -> subscription.Response:
        pass


class PyreDaemonLaunchAndSubscribeHandler(background_tasks.Task):
    server_options_reader: PyreServerOptionsReader
    remote_logging: Optional[backend_arguments.RemoteLogging]
    server_state: ServerState
    client_status_message_handler: status_message_handler.ClientStatusMessageHandler
    client_type_error_handler: type_error_handler.ClientTypeErrorHandler
    subscription_response_parser: PyreSubscriptionResponseParser

    def __init__(
        self,
        server_options_reader: PyreServerOptionsReader,
        server_state: ServerState,
        client_status_message_handler: status_message_handler.ClientStatusMessageHandler,
        client_type_error_handler: type_error_handler.ClientTypeErrorHandler,
        subscription_response_parser: PyreSubscriptionResponseParser,
        remote_logging: Optional[backend_arguments.RemoteLogging] = None,
    ) -> None:
        self.server_options_reader = server_options_reader
        self.remote_logging = remote_logging
        self.server_state = server_state
        self.client_status_message_handler = client_status_message_handler
        self.client_type_error_handler = client_type_error_handler
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
    async def client_setup(self) -> None:
        pass

    @abc.abstractmethod
    async def client_teardown(self) -> None:
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
            "binary": str(
                server_options.server_start_command.get_pyre_binary_location()
            ),
            "log_path": server_options.start_arguments.base_arguments.log_path,
            "global_root": (server_options.start_arguments.base_arguments.global_root),
            **(
                {}
                if relative_local_root is None
                else {"local_root": relative_local_root}
            ),
        }

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
                short_message=f"{self.server_state.server_options.flavor.simple_name()} Stopped",
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
                    short_message=f"{server_options.flavor.simple_name()} Ready",
                    level=lsp.MessageType.INFO,
                    fallback_to_notification=True,
                )
            else:
                await self.client_status_message_handler.log_and_show_status_message_to_client(
                    f"Pyre server at `{project_identifier}` has been initialized.",
                    short_message=f"{server_options.flavor.simple_name()} Ready",
                    level=lsp.MessageType.INFO,
                    fallback_to_notification=True,
                )
            self.server_state.consecutive_start_failure = 0
            self.server_state.is_user_notified_on_buck_failure = False
            log_lsp_event.log(
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
            self.server_state.status_tracker.set_status(state.ConnectionStatus.READY)
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
            await self.client_setup()
            if self.server_state.client_register_event is not None:
                self.server_state.client_register_event.set()
            await self.connect_and_subscribe(
                server_options,
                socket_path,
                connection_timer,
                is_preexisting=True,
            )
        except connections.ConnectionFailure:
            if self.server_state.client_register_event is not None:
                self.server_state.client_register_event.clear()

        await self.client_status_message_handler.log_and_show_status_message_to_client(
            f"Starting a new Pyre server at `{project_identifier}` in "
            "the background.",
            short_message=f"Starting {flavor.simple_name()}...",
            level=lsp.MessageType.WARNING,
            fallback_to_notification=True,
        )
        self.server_state.status_tracker.set_status(state.ConnectionStatus.STARTING)
        start_status = await async_start_pyre_server(
            server_options.server_start_command,
            start_arguments,
            flavor,
        )
        if isinstance(start_status, StartSuccess):
            await self.client_setup()
            if self.server_state.client_register_event is not None:
                self.server_state.client_register_event.set()
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
            log_lsp_event.log(
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
                short_message=f"{server_options.flavor.simple_name()} Stopped",
                level=lsp.MessageType.INFO,
                fallback_to_notification=False,
            )
        elif isinstance(start_status, OtherStartFailure):
            self.server_state.consecutive_start_failure += 1
            if (
                self.server_state.consecutive_start_failure
                < CONSECUTIVE_START_ATTEMPT_THRESHOLD
            ):
                log_lsp_event.log(
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
                    short_message=f"{server_options.flavor.simple_name()} Stopped",
                    level=lsp.MessageType.INFO,
                    fallback_to_notification=True,
                )
            else:
                log_lsp_event.log(
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
                    short_message=f"{server_options.flavor.simple_name()} Disabled",
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
        server_options = pyre_server_options.read_server_options(
            self.server_options_reader, self.remote_logging
        )
        # Update the server options, which can change if the config is modified
        self.server_state.server_options = server_options
        session_timer = timer.Timer()
        error_message: Optional[str] = None
        try:
            LOG.info(f"Starting Pyre server from configuration: {server_options}")
            await self.launch_and_subscribe(server_options)
        except asyncio.CancelledError as error:
            error_message = f"Explicit termination request: {error}"
            self.server_state.status_tracker.set_status(
                state.ConnectionStatus.DISCONNECTED
            )
            raise
        except PyreDaemonShutdown as error:
            error_message = f"Pyre server shutdown: {error}"
            self.server_state.status_tracker.set_status(
                state.ConnectionStatus.DISCONNECTED
            )
        except BaseException as error:
            error_message = traceback.format_exc()
            if isinstance(error, asyncio.IncompleteReadError):
                error_message += f"\nIncompleteReadError partial message: `{error.partial.decode('utf-8')}`"
            self.server_state.status_tracker.set_status(
                state.ConnectionStatus.DISCONNECTED
            )
            # we have this here and down below since we need to stop allowing
            # requests to be sent before client_teardown
            if self.server_state.client_register_event is not None:
                self.server_state.client_register_event.clear()
            await self.client_teardown()
            raise
        finally:
            if self.server_state.client_register_event is not None:
                self.server_state.client_register_event.clear()
            if error_message is not None:
                log_lsp_event.log(
                    remote_logging=self.remote_logging,
                    event=log_lsp_event.LSPEvent.DISCONNECTED,
                    integers={"duration": int(session_timer.stop_in_millisecond())},
                    normals={
                        **self._auxiliary_logging_info(server_options),
                        "exception": error_message,
                    },
                )
