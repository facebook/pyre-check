# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import json

import logging

from dataclasses import dataclass
from typing import Callable, Optional, Union

from typing_extensions import TypeAlias

from .. import json_rpc, log
from ..language_server import connections, protocol as lsp
from ..language_server.features import LanguageServerFeatures
from ..language_server.protocol import InitializeParameters, InitializeResult

from . import (
    backend_arguments,
    log_lsp_event,
    pyre_language_server,
    pyre_server_options,
)
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


ProcessInitializeRequestCallable: TypeAlias = Callable[
    [InitializeParameters, LanguageServerFeatures], InitializeResult
]


async def async_try_initialize(
    input_channel: connections.AsyncTextReader,
    output_channel: connections.AsyncTextWriter,
    server_options: pyre_server_options.PyreServerOptions,
    process_initialize_request: ProcessInitializeRequestCallable,
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


async def async_try_initialize_loop(
    server_options: PyreServerOptions,
    input_channel: connections.AsyncTextReader,
    output_channel: connections.AsyncTextWriter,
    remote_logging: Optional[backend_arguments.RemoteLogging],
    process_initialize_request: ProcessInitializeRequestCallable,
) -> InitializationSuccess | InitializationExit:
    while True:
        initialize_result = await async_try_initialize(
            input_channel, output_channel, server_options, process_initialize_request
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
