# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio
import json
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence, Tuple, Union

from ... import (
    backend_arguments,
    background_tasks,
    error,
    identifiers,
    json_rpc,
    type_error_handler,
)
from ...language_server import connections, daemon_connection, features, protocol as lsp
from .. import (
    daemon_querier as querier,
    daemon_query,
    pyre_language_server as ls,
    pyre_server_options as options,
    server_state as state,
    start,
)

DEFAULT_BINARY = "/bin/pyre"
DEFAULT_SERVER_IDENTIFIER = "server_identifier"
DEFAULT_START_ARGUMENTS: start.Arguments = start.Arguments(
    base_arguments=backend_arguments.BaseArguments(
        source_paths=backend_arguments.SimpleSourcePath(),
        log_path="/log/path",
        global_root="/global/root",
    ),
    socket_path=Path("irrelevant_socket_path.sock"),
)
DEFAULT_FEATURES: features.LanguageServerFeatures = features.LanguageServerFeatures(
    type_coverage=features.TypeCoverageAvailability.FUNCTION_LEVEL
)
DEFAULT_IS_STRICT = False
DEFAULT_EXCLUDES: Optional[Sequence[str]] = None
DEFAULT_FLAVOR: identifiers.PyreFlavor = identifiers.PyreFlavor.CLASSIC
DEFAULT_FILE_CONTENTS: str = "```\nfoo.Foo\n```"
DEFAULT_USE_ERRPY_PARSER: bool = False
DEFAULT_REQUEST_ID: int = 42
DEFAULT_CONNECTION_STATUS: state.ConnectionStatus = state.ConnectionStatus.READY


def create_server_options(
    binary: str = DEFAULT_BINARY,
    server_identifier: str = DEFAULT_SERVER_IDENTIFIER,
    start_arguments: start.Arguments = DEFAULT_START_ARGUMENTS,
    language_server_features: features.LanguageServerFeatures = DEFAULT_FEATURES,
    strict_default: bool = DEFAULT_IS_STRICT,
    excludes: Optional[Sequence[str]] = DEFAULT_EXCLUDES,
    flavor: identifiers.PyreFlavor = DEFAULT_FLAVOR,
    use_errpy_parser: bool = DEFAULT_USE_ERRPY_PARSER,
) -> options.PyreServerOptions:
    return options.PyreServerOptions(
        binary,
        server_identifier,
        start_arguments,
        language_server_features,
        strict_default,
        excludes if excludes else [],
        flavor,
        use_errpy_parser,
    )


def _create_server_options(
    binary: str = DEFAULT_BINARY,
    server_identifier: str = DEFAULT_SERVER_IDENTIFIER,
    start_arguments: start.Arguments = DEFAULT_START_ARGUMENTS,
    language_server_features: features.LanguageServerFeatures = DEFAULT_FEATURES,
    strict_default: bool = DEFAULT_IS_STRICT,
    excludes: Optional[Sequence[str]] = DEFAULT_EXCLUDES,
    flavor: identifiers.PyreFlavor = DEFAULT_FLAVOR,
) -> options.PyreServerOptionsReader:
    return lambda: create_server_options(
        binary,
        server_identifier,
        start_arguments,
        language_server_features,
        strict_default,
        excludes,
        flavor,
    )


def create_server_state_with_options(
    binary: str = DEFAULT_BINARY,
    server_identifier: str = DEFAULT_SERVER_IDENTIFIER,
    start_arguments: start.Arguments = DEFAULT_START_ARGUMENTS,
    language_server_features: features.LanguageServerFeatures = DEFAULT_FEATURES,
    strict_default: bool = DEFAULT_IS_STRICT,
    excludes: Optional[Sequence[str]] = DEFAULT_EXCLUDES,
    flavor: identifiers.PyreFlavor = DEFAULT_FLAVOR,
) -> state.ServerState:
    return state.ServerState(
        create_server_options(
            binary,
            server_identifier,
            start_arguments,
            language_server_features,
            strict_default,
            excludes,
            flavor,
        )
    )


class ExceptionRaisingBytesWriter(connections.AsyncBytesWriter):
    """
    An AsyncBytesWriter that always raises a given except when write is invoked.
    """

    def __init__(self, exception: Exception) -> None:
        self.exception = exception

    async def write(self, data: bytes) -> None:
        raise self.exception

    async def close(self) -> None:
        pass


class MockDaemonQuerier(querier.AbstractDaemonQuerier):
    def __init__(
        self,
        mock_type_errors: Optional[List[error.Error]] = None,
        mock_type_coverage: Optional[lsp.TypeCoverageResponse] = None,
        mock_hover_response: Optional[querier.GetHoverResponse] = None,
        mock_definition_response: Optional[
            querier.GetDefinitionLocationsResponse
        ] = None,
        mock_completion_response: Optional[List[lsp.CompletionItem]] = None,
        mock_call_hierarchy_response: Optional[List[lsp.CallHierarchyItem]] = None,
        mock_references_response: Optional[List[lsp.LspLocation]] = None,
    ) -> None:
        self.requests: List[object] = []
        self.mock_type_errors = mock_type_errors
        self.mock_type_coverage = mock_type_coverage
        self.mock_hover_response = mock_hover_response
        self.mock_definition_response = mock_definition_response
        self.mock_completion_response = mock_completion_response
        self.mock_call_hierarchy_response = mock_call_hierarchy_response
        self.mock_references_response = mock_references_response

    async def get_type_errors(
        self,
        path: Path,
    ) -> Union[daemon_query.DaemonQueryFailure, List[error.Error]]:
        return self.mock_type_errors or []

    async def get_type_coverage(
        self,
        path: Path,
    ) -> Union[daemon_query.DaemonQueryFailure, Optional[lsp.TypeCoverageResponse]]:
        self.requests.append({"path": path})
        return self.mock_type_coverage

    async def get_hover(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[daemon_query.DaemonQueryFailure, querier.GetHoverResponse]:
        self.requests.append({"path": path, "position": position})
        if self.mock_hover_response is None:
            raise ValueError("You need to set the hover response in the mock querier")
        else:
            return self.mock_hover_response

    async def get_definition_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[daemon_query.DaemonQueryFailure, querier.GetDefinitionLocationsResponse]:
        self.requests.append({"path": path, "position": position})
        if self.mock_definition_response is None:
            raise ValueError(
                "You need to set the get definition locations response in the mock querier"
            )
        else:
            return self.mock_definition_response

    async def get_completions(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[daemon_query.DaemonQueryFailure, List[lsp.CompletionItem]]:
        self.requests.append({"path": path, "position": position})
        if self.mock_completion_response is None:
            raise ValueError(
                "You need to set the get completions response in the mock querier"
            )
        else:
            return self.mock_completion_response

    async def get_reference_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[daemon_query.DaemonQueryFailure, List[lsp.LspLocation]]:
        self.requests.append({"path": path, "position": position})
        if self.mock_references_response is None:
            raise ValueError(
                "You need to set the get reference locations response in the mock querier"
            )
        else:
            return self.mock_references_response

    async def get_init_call_hierarchy(
        self,
        path: Path,
        position: lsp.PyrePosition,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> Union[daemon_query.DaemonQueryFailure, List[lsp.CallHierarchyItem]]:
        if self.mock_call_hierarchy_response is None:
            raise ValueError(
                "You need to set the get call hierarchy response in the mock querier"
            )
        else:
            return self.mock_call_hierarchy_response

    async def get_call_hierarchy_from_item(
        self,
        path: Path,
        call_hierarchy_item: lsp.CallHierarchyItem,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> Union[daemon_query.DaemonQueryFailure, List[lsp.CallHierarchyItem]]:
        if self.mock_call_hierarchy_response is None:
            raise ValueError(
                "You need to set the get call hierarchy response in the mock querier"
            )
        else:
            return self.mock_call_hierarchy_response

    async def update_overlay(
        self,
        path: Path,
        code: str,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        self.requests.append({"path": path, "code": code})
        # dummy result here- response not processed.
        return code

    async def handle_file_opened(
        self,
        path: Path,
        code: str,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return "Ok"

    async def handle_file_closed(
        self,
        path: Path,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return "Ok"

    async def handle_register_client(
        self,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return "Ok"

    async def handle_dispose_client(
        self,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return "Ok"


mock_server_options_reader: options.PyreServerOptionsReader = create_server_options
mock_initial_server_options: options.PyreServerOptions = mock_server_options_reader()
mock_server_state: state.ServerState = state.ServerState(
    server_options=mock_initial_server_options
)


def create_pyre_language_server_api(
    output_channel: connections.AsyncTextWriter,
    server_state: state.ServerState,
    querier: querier.AbstractDaemonQuerier,
) -> ls.PyreLanguageServerApi:
    return ls.PyreLanguageServer(
        output_channel=output_channel,
        server_state=server_state,
        querier=querier,
        client_type_error_handler=type_error_handler.ClientTypeErrorHandler(
            client_output_channel=output_channel,
            server_state=server_state,
        ),
    )


def create_pyre_language_server_api_and_output(
    opened_documents: Dict[Path, state.OpenedDocumentState],
    querier: MockDaemonQuerier,
    server_options: options.PyreServerOptions = mock_initial_server_options,
    connection_status: state.ConnectionStatus = DEFAULT_CONNECTION_STATUS,
) -> Tuple[ls.PyreLanguageServerApi, connections.MemoryBytesWriter]:
    output_writer = connections.MemoryBytesWriter()
    output_channel = connections.AsyncTextWriter(output_writer)
    server_state = state.ServerState(
        server_options=server_options,
        opened_documents=opened_documents,
    )
    server_state.status_tracker.set_status(connection_status)
    api = create_pyre_language_server_api(
        output_channel=output_channel,
        server_state=server_state,
        querier=querier,
    )
    return api, output_writer


def create_pyre_language_server_dispatcher(
    input_channel: connections.AsyncTextReader,
    server_state: state.ServerState,
    daemon_manager: background_tasks.TaskManager,
    querier: MockDaemonQuerier,
) -> Tuple[ls.PyreLanguageServerDispatcher, connections.MemoryBytesWriter]:
    output_writer = connections.MemoryBytesWriter()
    output_channel = connections.AsyncTextWriter(output_writer)
    api = create_pyre_language_server_api(
        output_channel=output_channel,
        server_state=server_state,
        querier=querier,
    )
    dispatcher = ls.PyreLanguageServerDispatcher(
        input_channel=input_channel,
        output_channel=output_channel,
        server_state=server_state,
        daemon_manager=daemon_manager,
        api=api,
    )
    return dispatcher, output_writer


def extract_json_from_json_rpc_message(
    raw_message: bytes,
) -> str:
    """
    Return the content-length of a json rpc message
    """
    content_length_portion, json_portion = raw_message.split(b"\r\n\r\n")
    CONTENT_LENGTH_PREFIX = b"Content-Length: "
    if not content_length_portion.startswith(CONTENT_LENGTH_PREFIX):
        raise ValueError(
            f"Did not get expected content length header, but {content_length_portion!r}"
        )
    content_length = int(content_length_portion[len(CONTENT_LENGTH_PREFIX) :])
    if not len(json_portion) == content_length:
        raise ValueError(
            f"Expected content length {content_length} to match length "
            f"{len(json_portion)} of json mssage {json_portion!r}"
        )
    return json_portion.decode()


async def create_input_channel_with_requests(
    requests: Iterable[json_rpc.Request],
) -> connections.AsyncTextReader:
    bytes_writer = connections.MemoryBytesWriter()
    for request in requests:
        await lsp.write_json_rpc(connections.AsyncTextWriter(bytes_writer), request)
    return connections.AsyncTextReader(
        connections.MemoryBytesReader(b"\n".join(bytes_writer.items()))
    )


def success_response_json(
    result: object,
    request_id: int = DEFAULT_REQUEST_ID,
) -> str:
    return json.dumps(json_rpc.SuccessResponse(id=request_id, result=result).json())


class NoOpBackgroundTask(background_tasks.Task):
    async def run(self) -> None:
        pass


class WaitForeverBackgroundTask(background_tasks.Task):
    async def run(self) -> None:
        await asyncio.Event().wait()
