# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


"""
This is an implementation of Pysa's language server. It is a refactored
version of persistent.py.
"""

import asyncio
import json
import logging
from os.path import relpath, exists
from pathlib import Path
from typing import Optional

from . import query as query_v2
from . import types as types_query

from typing import Union

from .. import (
    json_rpc,
    log,
    command_arguments,
    configuration as configuration_module,
)
from . import (
    commands,
    language_server_protocol as lsp,
    async_server_connection as connection,
    start,
)
from .persistent import (
    LSPEvent,
    _read_lsp_request,
    _log_lsp_event,
    InitializationSuccess,
    InitializationFailure,
    InitializationExit,
    _wait_for_exit,
    process_initialize_request,
)

LOG: logging.Logger = logging.getLogger(__name__)


async def try_initialize(
    input_channel: connection.TextReader,
    output_channel: connection.TextWriter,
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
        result = process_initialize_request(initialize_parameters)
        await lsp.write_json_rpc(
            output_channel,
            # pyre-fixme[16]: Pyre doesn't understand `dataclasses_json`
            json_rpc.SuccessResponse(id=request_id, result=result.to_dict()),
        )

        initialized_notification = await lsp.read_json_rpc(input_channel)
        if initialized_notification.method == "shutdown":
            await _wait_for_exit(input_channel, output_channel)
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
        await lsp.write_json_rpc(
            output_channel,
            json_rpc.ErrorResponse(
                id=request.id if request is not None else None,
                code=json_rpc_error.error_code(),
                message=str(json_rpc_error),
                data={"retry": False},
            ),
        )
        return InitializationFailure(exception=json_rpc_error)


class PysaServer:
    # I/O Channels
    input_channel: connection.TextReader
    output_channel: connection.TextWriter

    # Immutable States
    client_capabilities: lsp.ClientCapabilities

    def __init__(
        self,
        input_channel: connection.TextReader,
        output_channel: connection.TextWriter,
        client_capabilities: lsp.ClientCapabilities,
        pyre_arguments: start.Arguments,
        binary_location: str,
        server_identifier: str,
    ) -> None:
        self.input_channel = input_channel
        self.output_channel = output_channel
        self.client_capabilities = client_capabilities
        self.pyre_arguments = pyre_arguments
        self.binary_location = binary_location
        self.server_identifier = server_identifier

    async def copy_model(
        self, document_path: str, position: lsp.Position
    ) -> Optional[str]:
        rel_path = relpath(document_path, self.pyre_arguments.global_root)
        try:

            types = types_query.types(
                Path(self.pyre_arguments.base_arguments.log_path), [rel_path]
            )

            for t in types[0].types:
                start = types_query.Position(
                    line=t.location["start"].line,
                    column=t.location["start"].column,
                )
                stop = types_query.Position(
                    line=t.location["stop"].line,
                    column=t.location["stop"].column,
                )
                selected = types_query.Position(
                    line=position.line,
                    column=position.character,
                )
                if selected > start and selected < stop:
                    function_model = t.extract_function_model()
                    return function_model

        except query_v2.InvalidQueryResponse as e:
            await self.log_and_show_message_to_client(e, lsp.MessageType.ERROR)

    async def show_message_to_client(
        self, message: str, level: lsp.MessageType = lsp.MessageType.INFO
    ) -> None:
        await lsp.write_json_rpc(
            self.output_channel,
            json_rpc.Request(
                method="window/showMessage",
                parameters=json_rpc.ByNameParameters(
                    {"type": int(level), "message": message}
                ),
            ),
        )

    async def log_and_show_message_to_client(
        self, message: str, level: lsp.MessageType = lsp.MessageType.INFO
    ) -> None:
        if level == lsp.MessageType.ERROR:
            LOG.error(message)
        elif level == lsp.MessageType.WARNING:
            LOG.warning(message)
        elif level == lsp.MessageType.INFO:
            LOG.info(message)
        else:
            LOG.debug(message)
        await self.show_message_to_client(message, level)

    async def wait_for_exit(self) -> int:
        while True:
            async with _read_lsp_request(
                self.input_channel, self.output_channel
            ) as request:
                LOG.debug(f"Received post-shutdown request: {request}")

                if request.method == "exit":
                    return 0
                else:
                    raise json_rpc.InvalidRequestError("LSP server has been shut down")

    async def process_open_request(
        self, parameters: lsp.DidOpenTextDocumentParameters
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )

    async def process_close_request(
        self, parameters: lsp.DidCloseTextDocumentParameters
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        try:
            LOG.info(f"File closed: {document_path}")

        except KeyError:
            LOG.warning(f"Trying to close an un-opened file: {document_path}")

    async def process_did_save_request(
        self, parameters: lsp.DidSaveTextDocumentParameters
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )

    async def process_copy_model_request(
        self, parameters: lsp.DidCopyModelParameters
    ) -> str:
        document_path = parameters.path
        if not exists(document_path):
            raise json_rpc.InvalidRequestError(
                f"Document path is not a file: {parameters.path}"
            )
        return await self.copy_model(document_path, parameters.position)

    async def run(self) -> int:
        while True:
            async with _read_lsp_request(
                self.input_channel, self.output_channel
            ) as request:
                try:
                    if request.method == "exit":
                        return commands.ExitCode.FAILURE
                    elif request.method == "shutdown":
                        lsp.write_json_rpc(
                            self.output_channel,
                            json_rpc.SuccessResponse(id=request.id, result=None),
                        )
                        return await self.wait_for_exit()
                    elif request.method == "textDocument/didOpen":
                        parameters = request.parameters
                        if parameters is None:
                            raise json_rpc.InvalidRequestError(
                                "Missing parameters for didOpen method"
                            )
                        await self.process_open_request(
                            lsp.DidOpenTextDocumentParameters.from_json_rpc_parameters(
                                parameters
                            )
                        )
                    elif request.method == "textDocument/didClose":
                        parameters = request.parameters
                        if parameters is None:
                            raise json_rpc.InvalidRequestError(
                                "Missing parameters for didClose method"
                            )
                        await self.process_close_request(
                            lsp.DidCloseTextDocumentParameters.from_json_rpc_parameters(
                                parameters
                            )
                        )
                    elif request.method == "textDocument/didSave":
                        parameters = request.parameters
                        if parameters is None:
                            raise json_rpc.InvalidRequestError(
                                "Missing parameters for didSave method"
                            )
                        await self.process_did_save_request(
                            lsp.DidSaveTextDocumentParameters.from_json_rpc_parameters(
                                parameters
                            )
                        )
                    elif request.method == "pysa/copyModel":
                        parameters = request.parameters
                        if parameters is None:
                            raise json_rpc.InvalidRequestError(
                                "Missing parameters for copyModel method"
                            )
                        # processing parameter data sent from VSCode
                        request.parameters.values["path"] = request.parameters.values[
                            "path"
                        ]["path"]
                        request.parameters.values["position"]["line"] = (
                            request.parameters.values["position"]["line"] + 1
                        )
                        copied_model = await self.process_copy_model_request(
                            lsp.DidCopyModelParameters.from_json_rpc_parameters(
                                parameters
                            )
                        )
                        await lsp.write_json_rpc(
                            self.output_channel,
                            json_rpc.SuccessResponse(
                                id=request.id, result=copied_model
                            ),
                        )
                    elif request.id is not None:
                        raise lsp.RequestCancelledError("Request not supported yet")
                except Exception as e:
                    await self.log_and_show_message_to_client(
                        str(e), lsp.MessageType.ERROR
                    )


async def run_persistent(
    binary_location: str,
    server_identifier: str,
    pysa_arguments: start.Arguments,
) -> int:
    stdin, stdout = await connection.create_async_stdin_stdout()
    while True:
        initialize_result = await try_initialize(stdin, stdout)
        if isinstance(initialize_result, InitializationExit):
            LOG.info("Received exit request before initialization.")
            return 0
        elif isinstance(initialize_result, InitializationSuccess):
            LOG.info("Initialization successful.")
            client_info = initialize_result.client_info
            _log_lsp_event(
                remote_logging=pysa_arguments.base_arguments.remote_logging,
                event=LSPEvent.INITIALIZED,
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
            server = PysaServer(
                input_channel=stdin,
                output_channel=stdout,
                client_capabilities=client_capabilities,
                binary_location=binary_location,
                server_identifier=server_identifier,
                pyre_arguments=pysa_arguments,
            )
            return await server.run()
        elif isinstance(initialize_result, InitializationFailure):
            exception = initialize_result.exception
            message = (
                str(exception) if exception is not None else "ignoring notification"
            )
            LOG.info(f"Initialization failed: {message}")
            # Loop until we get either InitializeExit or InitializeSuccess
        else:
            raise RuntimeError("Cannot determine the type of initialize_result")


def run(
    configuration: configuration_module.Configuration,
    start_arguments: command_arguments.StartArguments,
) -> int:
    binary_location = configuration.get_binary_respecting_override()
    if binary_location is None:
        raise configuration_module.InvalidConfiguration(
            "Cannot locate a Pyre binary to run."
        )

    server_identifier = start.get_server_identifier(configuration)
    pyre_arguments = start.create_server_arguments(configuration, start_arguments)
    if pyre_arguments.watchman_root is None:
        raise commands.ClientException(
            (
                "Cannot locate a `watchman` root. Pyre's server will not function "
                + "properly."
            )
        )

    return asyncio.get_event_loop().run_until_complete(
        run_persistent(binary_location, server_identifier, pyre_arguments)
    )
