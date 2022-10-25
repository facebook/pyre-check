# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import logging

from dataclasses import dataclass
from typing import Awaitable, Callable, Optional

from .. import json_rpc
from ..language_server import connections, protocol as lsp
from . import backend_arguments, log_lsp_event
from .pyre_server_options import PyreServerOptions

LOG: logging.Logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class InitializationSuccess:
    client_capabilities: lsp.ClientCapabilities
    client_info: Optional[lsp.Info] = None
    initialization_options: Optional[lsp.InitializationOptions] = None


@dataclass(frozen=True)
class InitializationFailure:
    exception: Optional[json_rpc.JSONRPCException] = None


@dataclass(frozen=True)
class InitializationExit:
    pass


async def async_try_initialize_loop(
    server_options: PyreServerOptions,
    input_channel: connections.AsyncTextReader,
    output_channel: connections.AsyncTextWriter,
    remote_logging: Optional[backend_arguments.RemoteLogging],
    async_try_initialize: Callable[
        [connections.AsyncTextReader, connections.AsyncTextWriter, PyreServerOptions],
        Awaitable[InitializationExit | InitializationSuccess | InitializationFailure],
    ],
) -> InitializationSuccess | InitializationExit:
    while True:
        initialize_result = await async_try_initialize(
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
