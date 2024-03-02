# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module provides shared logic (Persistent, CodeNav and Bento) for sending
status messages to the LSP client.
"""

import logging
from typing import Optional

from .. import json_rpc
from ..language_server import connections, features, protocol as lsp
from . import server_state

LOG: logging.Logger = logging.getLogger(__name__)


def _client_has_status_bar_support(
    client_capabilities: lsp.ClientCapabilities,
) -> bool:
    window_capabilities = client_capabilities.window
    if window_capabilities is not None:
        return window_capabilities.status is not None
    else:
        return False


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


class ClientStatusMessageHandler:
    def __init__(
        self,
        client_output_channel: connections.AsyncTextWriter,
        server_state: server_state.ServerState,
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
            LOG.error("Status updates are disabled, skipping status message.")
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
