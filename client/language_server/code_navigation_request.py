# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

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
    originating_request: Optional[str] = None


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
        return lsp.LspLocation(
            uri=lsp.DocumentUri.from_file_path(Path(self.path)).unparse(),
            range=self.range.to_lsp_range(),
        )


@dataclasses.dataclass(frozen=True)
class LocationOfDefinitionResponse(json_mixins.SnakeCaseAndExcludeJsonMixin):
    definitions: List[DefinitionResponse]
    empty_reason: Optional[object] = None
    duration: float = 0


@dataclasses.dataclass(frozen=True)
class TypeErrorsResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    errors: List[Dict[str, Any]]

    def to_errors_response(self) -> List[error.Error]:
        return [error.Error.from_json(error_response) for error_response in self.errors]


class PyreDocumentSymbolKind(str, enum.Enum):
    FILE = "FILE"
    MODULE = "MODULE"
    NAMESPACE = "NAMESPACE"
    PACKAGE = "PACKAGE"
    CLASS = "CLASS"
    METHOD = "METHOD"
    PROPERTY = "PROPERTY"
    FIELD = "FIELD"
    CONSTRUCTOR = "CONSTRUCTOR"
    ENUM = "ENUM"
    INTERFACE = "INTERFACE"
    FUNCTION = "FUNCTION"
    VARIABLE = "VARIABLE"
    CONSTANT = "CONSTANT"
    STRING = "STRING"
    NUMBER = "NUMBER"
    BOOLEAN = "BOOLEAN"
    ARRAY = "ARRAY"
    OBJECT = "OBJECT"
    KEY = "KEY"
    NULL = "NULL"
    ENUM_MEMBER = "ENUM_MEMBER"
    STRUCT = "STRUCT"
    EVENT = "EVENT"
    OPERATOR = "OPERATOR"
    TYPEPARAMETER = "TYPEPARAMETER"

    def to_lsp_document_symbol_kind(self) -> lsp.SymbolKind:
        if self == self.FILE:
            return lsp.SymbolKind.FILE
        elif self == self.MODULE:
            return lsp.SymbolKind.MODULE
        elif self == self.NAMESPACE:
            return lsp.SymbolKind.NAMESPACE
        elif self == self.PACKAGE:
            return lsp.SymbolKind.PACKAGE
        elif self == self.CLASS:
            return lsp.SymbolKind.CLASS
        elif self == self.METHOD:
            return lsp.SymbolKind.METHOD
        elif self == self.PROPERTY:
            return lsp.SymbolKind.PROPERTY
        elif self == self.FIELD:
            return lsp.SymbolKind.FIELD
        elif self == self.CONSTRUCTOR:
            return lsp.SymbolKind.CONSTRUCTOR
        elif self == self.ENUM:
            return lsp.SymbolKind.ENUM
        elif self == self.INTERFACE:
            return lsp.SymbolKind.INTERFACE
        elif self == self.FUNCTION:
            return lsp.SymbolKind.FUNCTION
        elif self == self.VARIABLE:
            return lsp.SymbolKind.VARIABLE
        elif self == self.CONSTANT:
            return lsp.SymbolKind.CONSTANT
        elif self == self.STRING:
            return lsp.SymbolKind.STRING
        elif self == self.NUMBER:
            return lsp.SymbolKind.NUMBER
        elif self == self.BOOLEAN:
            return lsp.SymbolKind.BOOLEAN
        elif self == self.ARRAY:
            return lsp.SymbolKind.ARRAY
        elif self == self.OBJECT:
            return lsp.SymbolKind.OBJECT
        elif self == self.KEY:
            return lsp.SymbolKind.KEY
        elif self == self.NULL:
            return lsp.SymbolKind.NULL
        elif self == self.ENUM_MEMBER:
            return lsp.SymbolKind.ENUMMEMBER
        elif self == self.STRUCT:
            return lsp.SymbolKind.STRUCT
        elif self == self.EVENT:
            return lsp.SymbolKind.EVENT
        elif self == self.OPERATOR:
            return lsp.SymbolKind.OPERATOR
        elif self == self.TYPEPARAMETER:
            return lsp.SymbolKind.TYPEPARAMETER
        else:
            raise ValueError("Unknown symbol kind")


@dataclasses.dataclass(frozen=True)
class PyreDocumentSymbol(json_mixins.CamlCaseAndExcludeJsonMixin):
    name: str
    detail: Optional[str]
    kind: PyreDocumentSymbolKind
    range: CodeNavigationRange
    selection_range: CodeNavigationRange
    children: List["PyreDocumentSymbol"]

    def to_lsp_document_symbol(
        self,
    ) -> lsp.DocumentSymbol:
        children = []
        for child in self.children:
            res = child.to_lsp_document_symbol()
            assert child is not None
            children.append(res)

        response = lsp.DocumentSymbol(
            name=self.name,
            detail=self.detail,
            kind=self.kind.to_lsp_document_symbol_kind(),
            range=self.range.to_lsp_range(),
            selection_range=self.selection_range.to_lsp_range(),
            children=children,
        )

        return response


@dataclasses.dataclass(frozen=True)
class PyreDocumentSymbolResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    symbols: List[PyreDocumentSymbol]


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


def invalid_response(response: str, raw_request: str) -> ErrorResponse:
    return ErrorResponse(
        message=f"Invalid response {response} to pyre code_navigation request.",
        originating_request=raw_request,
    )


ResponseKind = TypeVar(
    "ResponseKind",
    bound=Union[
        json_mixins.CamlCaseAndExcludeJsonMixin,
        json_mixins.SnakeCaseAndExcludeJsonMixin,
    ],
)


def parse_response(
    response: Dict[str, Any], response_type: Type[ResponseKind], raw_request: str
) -> Union[ResponseKind, ErrorResponse]:
    try:
        return response_type.cached_schema().load(response)
    except AssertionError as error:
        return ErrorResponse(
            message=f"Assertion error when parsing JSON into the response schema: {error}",
            originating_request=raw_request,
        )


def parse_raw_response(
    raw_response: str,
    expected_response_kind: str,
    response_type: Type[ResponseKind],
    raw_request: str,
) -> Union[ResponseKind, ErrorResponse]:
    try:
        response = json.loads(raw_response)
        if (
            not isinstance(response, list)
            or len(response) != 2
            or response[0] != expected_response_kind
        ):
            return invalid_response(raw_response, raw_request)
    except Exception as error:
        return ErrorResponse(message=f"Exception while parsing response: {error}")
    return parse_response(response[1], response_type, raw_request)


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
        response,
        expected_response_kind="Hover",
        response_type=HoverResponse,
        raw_request=raw_request,
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
        raw_request=raw_request,
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
        response,
        expected_response_kind="TypeErrors",
        response_type=TypeErrorsResponse,
        raw_request=raw_request,
    )


async def async_handle_document_symbol_request(
    socket_path: Path,
    document_symbol_request: lsp.DocumentSymbolRequest,
) -> Union[PyreDocumentSymbolResponse, ErrorResponse]:
    raw_request = json.dumps(["Query", document_symbol_request.to_json()])
    response = await daemon_connection.attempt_send_async_raw_request(
        socket_path, raw_request
    )
    if isinstance(response, daemon_connection.DaemonConnectionFailure):
        return ErrorResponse(
            message=response.error_message, error_source=response.error_source
        )
    return parse_raw_response(
        response,
        expected_response_kind="DocumentSymbol",
        response_type=PyreDocumentSymbolResponse,
        raw_request=raw_request,
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
        raw_request=raw_request,
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
) -> Union[SuperclassesResponse, ErrorResponse]:
    raw_command = json.dumps(["Query", superclasses.to_json()])
    response = await daemon_connection.attempt_send_async_raw_request(
        socket_path, raw_command
    )
    if isinstance(response, daemon_connection.DaemonConnectionFailure):
        # For now, we don't try to initialize a server when unable to connect. In the future,
        # after we have a programmatic way of starting a Pyre server, we should be able to
        # change behavior here to initialize a server.
        return ErrorResponse(
            message="Failed to connect to Pyre server.",
            error_source=response.error_source,
        )
    return parse_raw_response(
        response,
        expected_response_kind="Superclasses",
        response_type=SuperclassesResponse,
        raw_request=raw_command,
    )
    return response
