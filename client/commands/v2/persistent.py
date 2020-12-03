# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import logging
from typing import Union

from ... import (
    json_rpc,
    version,
    command_arguments,
    commands,
    configuration as configuration_module,
)
from . import language_server_protocol as lsp, async_server_connection as connection

LOG: logging.Logger = logging.getLogger(__name__)


def process_initialize_request(
    parameters: lsp.InitializeParameters,
) -> lsp.InitializeResult:
    LOG.info(
        f"Received initialization request from {parameters.client_info} "
        f" (pid = {parameters.process_id})"
    )

    server_info = lsp.Info(name="pyre", version=version.__version__)
    server_capabilities = lsp.ServerCapabilities(
        text_document_sync=lsp.TextDocumentSyncOptions(
            open_close=True,
            change=lsp.TextDocumentSyncKind.NONE,
            save=lsp.SaveOptions(include_text=False),
        )
    )
    return lsp.InitializeResult(
        capabilities=server_capabilities, server_info=server_info
    )


@dataclasses.dataclass(frozen=True)
class InitializationSuccess:
    client_capabilities: lsp.ClientCapabilities


@dataclasses.dataclass(frozen=True)
class InitializationFailure:
    pass


@dataclasses.dataclass(frozen=True)
class InitializationExit:
    pass


async def try_initialize(
    input_channel: connection.TextReader,
    output_channel: connection.TextWriter,
) -> Union[InitializationSuccess, InitializationFailure, InitializationExit]:
    """
    Read a LSP message from the input channel and try to initialize an LSP
    server. Also write to the output channel with proper response if the input
    message is a request instead of a notification.

    The function can return one of three possibilities:
    - If the initialization succeeds, return `InitializationSuccess`.
    - If the initialization fails, return `InitializationFailure`. There could
      be many reasons for the failure: The incoming LSP message may not be an
      initiailization request. The incoming LSP request may be malformed. Or the
      client may not support the right capabilities that we want them to support.
    - If an exit notification is received, return `InitializationExit`. The LSP
      spec allows exiting a server without a preceding initialize request.
    """
    try:
        request = await lsp.read_json_rpc(input_channel)
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
        result = process_initialize_request(initialize_parameters)
        await lsp.write_json_rpc(
            output_channel,
            # pyre-fixme[16]: Pyre doesn't understand `dataclasses_json`
            json_rpc.SuccessResponse(id=request_id, result=result.to_dict()),
        )

        return InitializationSuccess(
            client_capabilities=initialize_parameters.capabilities
        )
    except json_rpc.JSONRPCException as json_rpc_error:
        await lsp.write_json_rpc(
            output_channel,
            json_rpc.ErrorResponse(
                id=request.id,
                code=json_rpc_error.error_code(),
                message=str(json_rpc_error),
                data={"retry": False},
            ),
        )
        return InitializationFailure()


def run(
    configuration: configuration_module.Configuration,
    start_arguments: command_arguments.StartArguments,
) -> commands.ExitCode:
    LOG.warning("The best is yet to come...")
    return commands.ExitCode.SUCCESS
