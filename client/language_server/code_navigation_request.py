# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module contains the definition of code navigation requests and an API to convert a given LSP request
to a corresponding code navigation request. Also contains an API that sends a given request to the code navigation
server and gets a response.
"""

import dataclasses
import enum
import json
from pathlib import Path
from typing import Any, Dict, List, Optional, Type, TypeVar, Union

from .. import dataclasses_json_extensions as json_mixins, error

from . import daemon_connection, protocol as lsp


@dataclasses.dataclass(frozen=True)
class HoverRequest:
    path: str
    client_id: str
    position: lsp.PyrePosition

    def to_json(self) -> List[object]:
        return [
            "Hover",
            {
                "path": self.path,
                "client_id": self.client_id,
                "position": {
                    "line": self.position.line,
                    "column": self.position.character,
                },
            },
        ]


@dataclasses.dataclass(frozen=True)
class LocationOfDefinitionRequest:
    path: str
    client_id: str
    position: lsp.PyrePosition

    def to_json(self) -> List[object]:
        return [
            "LocationOfDefinition",
            {
                "path": self.path,
                "client_id": self.client_id,
                "position": {
                    "line": self.position.line,
                    "column": self.position.character,
                },
            },
        ]


@dataclasses.dataclass(frozen=True)
class TypeErrorsRequest:
    paths: List[str]
    client_id: str

    def to_json(self) -> List[object]:
        return [
            "GetTypeErrors",
            {
                "paths": self.paths,
                "client_id": self.client_id,
            },
        ]


@dataclasses.dataclass(frozen=True)
class ErrorResponse:
    message: str
    error_source: Optional[Exception] = None


@dataclasses.dataclass(frozen=True)
class HoverResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    contents: List[lsp.PyreHoverResponse]


@dataclasses.dataclass(frozen=True)
class CodeNavigationPosition(json_mixins.CamlCaseAndExcludeJsonMixin):
    """LSP uses 0-indexing for lines whereas Pyre uses 1-indexing."""

    line: int
    column: int

    def to_lsp_position(self) -> lsp.LspPosition:
        return lsp.LspPosition(self.line - 1, self.column)


@dataclasses.dataclass(frozen=True)
class CodeNavigationRange(json_mixins.CamlCaseAndExcludeJsonMixin):
    start: CodeNavigationPosition
    stop: CodeNavigationPosition

    def to_lsp_range(self) -> lsp.LspRange:
        return lsp.LspRange(
            start=self.start.to_lsp_position(),
            end=self.stop.to_lsp_position(),
        )


@dataclasses.dataclass(frozen=True)
class DefinitionResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    path: str
    range: CodeNavigationRange

    def to_lsp_definition_response(self) -> lsp.LspLocation:
        return lsp.LspLocation(uri=self.path, range=self.range.to_lsp_range())


@dataclasses.dataclass(frozen=True)
class LocationOfDefinitionResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    definitions: List[DefinitionResponse]


@dataclasses.dataclass(frozen=True)
class TypeErrorsResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    errors: List[Dict[str, Any]]

    def to_errors_response(self) -> List[error.Error]:
        return [error.Error.from_json(error_response) for error_response in self.errors]


class PyreCompletionItemKind(str, enum.Enum):
    # TODO(T159253974): extend to support remaining CompletionItemKinds when local/global variable autocomplete are supported
    SIMPLE = "SIMPLE"
    METHOD = "METHOD"
    PROPERTY = "PROPERTY"
    VARIABLE = "VARIABLE"

    def to_lsp_completion_item_kind(self) -> lsp.CompletionItemKind:
        if self == self.METHOD:
            return lsp.CompletionItemKind.METHOD
        elif self == self.PROPERTY:
            return lsp.CompletionItemKind.PROPERTY
        elif self == self.VARIABLE:
            return lsp.CompletionItemKind.VARIABLE
        else:
            return lsp.CompletionItemKind.TEXT


@dataclasses.dataclass(frozen=True)
class PyreCompletionItem(json_mixins.CamlCaseAndExcludeJsonMixin):
    label: str
    kind: PyreCompletionItemKind
    detail: Optional[str]

    def to_lsp_completion_item(self) -> lsp.CompletionItem:
        return lsp.CompletionItem(
            label=self.label,
            kind=self.kind.to_lsp_completion_item_kind(),
            filterText=self.label,
            sortText=self.label,
            detail=self.detail,
        )


@dataclasses.dataclass(frozen=True)
class PyreCompletionsResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    completions: List[PyreCompletionItem]


@dataclasses.dataclass(frozen=True)
class RegisterClient:
    client_id: str

    def to_json(self) -> List[object]:
        return [
            "RegisterClient",
            {
                "client_id": self.client_id,
            },
        ]


@dataclasses.dataclass(frozen=True)
class DisposeClient:
    client_id: str

    def to_json(self) -> List[object]:
        return [
            "DisposeClient",
            {
                "client_id": self.client_id,
            },
        ]


@dataclasses.dataclass(frozen=True)
class LocalUpdate:
    path: str
    content: str
    client_id: str

    def to_json(self) -> List[object]:
        return [
            "LocalUpdate",
            {
                "path": self.path,
                "content": self.content,
                "client_id": self.client_id,
            },
        ]


@dataclasses.dataclass(frozen=True)
class FileOpened:
    path: Path
    content: Optional[str]
    client_id: str

    def to_json(self) -> List[object]:
        return [
            "FileOpened",
            {
                "path": f"{self.path}",
                "content": self.content,
                "client_id": self.client_id,
            },
        ]


@dataclasses.dataclass(frozen=True)
class FileClosed:
    path: Path
    client_id: str

    def to_json(self) -> List[object]:
        return [
            "FileClosed",
            {
                "path": f"{self.path}",
                "client_id": self.client_id,
            },
        ]


@dataclasses.dataclass(frozen=True)
class ClassExpression(json_mixins.SnakeCaseAndExcludeJsonMixin):
    module: str
    qualified_name: str


@dataclasses.dataclass(frozen=True)
class SuperclassesRequest:
    class_: ClassExpression

    def to_json(self) -> List[object]:
        return [
            "Superclasses",
            {"class": self.class_.to_dict()},
        ]


@dataclasses.dataclass(frozen=True)
class SuperclassesResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    superclasses: List[ClassExpression]


def invalid_response(response: str) -> ErrorResponse:
    return ErrorResponse(
        message=f"Invalid response {response} to pyre code_navigation request."
    )


ResponseKind = TypeVar("ResponseKind", bound=json_mixins.CamlCaseAndExcludeJsonMixin)


def parse_response(
    response: Dict[str, Any], response_type: Type[ResponseKind]
) -> Union[ResponseKind, ErrorResponse]:
    try:
        return response_type.cached_schema().load(response)
    except AssertionError as error:
        return ErrorResponse(
            message=f"Assertion error when parsing JSON into the response schema: {error}"
        )


def parse_raw_response(
    raw_response: str, expected_response_kind: str, response_type: Type[ResponseKind]
) -> Union[ResponseKind, ErrorResponse]:
    try:
        response = json.loads(raw_response)
        if (
            not isinstance(response, list)
            or len(response) != 2
            or response[0] != expected_response_kind
        ):
            return invalid_response(raw_response)
    except Exception as error:
        return ErrorResponse(message=f"Exception while parsing response: {error}")
    return parse_response(response[1], response_type)


async def async_handle_hover_request(
    socket_path: Path,
    hover_request: HoverRequest,
) -> Union[HoverResponse, ErrorResponse]:
    raw_request = json.dumps(["Query", hover_request.to_json()])
    response = await daemon_connection.attempt_send_async_raw_request(
        socket_path, raw_request
    )
    if isinstance(response, daemon_connection.DaemonConnectionFailure):
        return ErrorResponse(
            message=response.error_message, error_source=response.error_source
        )
    response = parse_raw_response(
        response, expected_response_kind="Hover", response_type=HoverResponse
    )
    if isinstance(response, ErrorResponse):
        return response
    return HoverResponse(response.contents)


async def async_handle_definition_request(
    socket_path: Path,
    definition_request: LocationOfDefinitionRequest,
) -> Union[LocationOfDefinitionResponse, ErrorResponse]:
    raw_request = json.dumps(["Query", definition_request.to_json()])
    response = await daemon_connection.attempt_send_async_raw_request(
        socket_path, raw_request
    )
    if isinstance(response, daemon_connection.DaemonConnectionFailure):
        return ErrorResponse(
            message=response.error_message, error_source=response.error_source
        )
    return parse_raw_response(
        response,
        expected_response_kind="LocationOfDefinition",
        response_type=LocationOfDefinitionResponse,
    )


async def async_handle_type_errors_request(
    socket_path: Path,
    type_errors_request: TypeErrorsRequest,
) -> Union[TypeErrorsResponse, ErrorResponse]:
    raw_request = json.dumps(["Query", type_errors_request.to_json()])
    response = await daemon_connection.attempt_send_async_raw_request(
        socket_path, raw_request
    )
    if isinstance(response, daemon_connection.DaemonConnectionFailure):
        return ErrorResponse(
            message=response.error_message, error_source=response.error_source
        )
    return parse_raw_response(
        response, expected_response_kind="TypeErrors", response_type=TypeErrorsResponse
    )


async def async_handle_completion_request(
    socket_path: Path,
    completion_request: lsp.CompletionRequest,
) -> Union[PyreCompletionsResponse, ErrorResponse]:
    raw_request = json.dumps(["Query", completion_request.to_json()])
    response = await daemon_connection.attempt_send_async_raw_request(
        socket_path, raw_request
    )
    if isinstance(response, daemon_connection.DaemonConnectionFailure):
        return ErrorResponse(
            message=response.error_message, error_source=response.error_source
        )
    return parse_raw_response(
        response,
        expected_response_kind="Completion",
        response_type=PyreCompletionsResponse,
    )


async def async_handle_register_client(
    socket_path: Path, register_client: RegisterClient
) -> Union[str, daemon_connection.DaemonConnectionFailure]:
    raw_command = json.dumps(["Command", register_client.to_json()])
    response = await daemon_connection.attempt_send_async_raw_request(
        socket_path, raw_command
    )
    return response


async def async_handle_dispose_client(
    socket_path: Path, dispose_client: DisposeClient
) -> Union[str, daemon_connection.DaemonConnectionFailure]:
    raw_command = json.dumps(["Command", dispose_client.to_json()])
    response = await daemon_connection.attempt_send_async_raw_request(
        socket_path, raw_command
    )
    return response


async def async_handle_local_update(
    socket_path: Path, local_update: LocalUpdate
) -> Union[str, daemon_connection.DaemonConnectionFailure]:
    raw_command = json.dumps(["Command", local_update.to_json()])
    response = await daemon_connection.attempt_send_async_raw_request(
        socket_path, raw_command
    )
    return response


async def async_handle_file_opened(
    socket_path: Path, file_opened: FileOpened
) -> Union[str, daemon_connection.DaemonConnectionFailure]:
    raw_command = json.dumps(["Command", file_opened.to_json()])
    response = await daemon_connection.attempt_send_async_raw_request(
        socket_path, raw_command
    )
    return response


async def async_handle_file_closed(
    socket_path: Path, file_closed: FileClosed
) -> Union[str, daemon_connection.DaemonConnectionFailure]:
    raw_command = json.dumps(["Command", file_closed.to_json()])
    response = await daemon_connection.attempt_send_async_raw_request(
        socket_path, raw_command
    )
    return response


async def async_handle_superclasses(
    socket_path: Path,
    superclasses: SuperclassesRequest,
) -> Union[str, daemon_connection.DaemonConnectionFailure]:
    raw_command = json.dumps(["Query", superclasses.to_json()])
    response = await daemon_connection.attempt_send_async_raw_request(
        socket_path, raw_command
    )
    return response
