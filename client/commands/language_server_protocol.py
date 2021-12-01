# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio
import dataclasses
import enum
import logging
import urllib
from pathlib import Path
from typing import (
    Iterable,
    Optional,
    List,
    Type,
    TypeVar,
)

import dataclasses_json

from .. import json_rpc
from . import async_server_connection

LOG: logging.Logger = logging.getLogger(__name__)

T = TypeVar("T")
Point = TypeVar("Point")
Value = TypeVar("Value")


class ServerNotInitializedError(json_rpc.JSONRPCException):
    def error_code(self) -> int:
        return -32002


class RequestCancelledError(json_rpc.JSONRPCException):
    def error_code(self) -> int:
        return -32800


async def _read_headers(input_channel: async_server_connection.TextReader) -> List[str]:
    headers = []
    header = await input_channel.read_until("\r\n")
    while header != "\r\n":
        headers.append(header)
        header = await input_channel.read_until("\r\n")
    return headers


def _get_content_length(headers: Iterable[str]) -> int:
    try:
        for header in headers:
            parts = [part.strip().lower() for part in header.split(":", maxsplit=1)]
            if len(parts) <= 1:
                continue

            if parts[0] == "content-length":
                return int(parts[1])

        # pyre-fixme[61]: `parts` may not be initialized here.
        raise json_rpc.ParseError(f"Failed to find content length header from {parts}")
    except ValueError as error:
        raise json_rpc.ParseError(f"Cannot parse content length into integer: {error}")


async def read_json_rpc(
    input_channel: async_server_connection.TextReader,
) -> json_rpc.Request:
    """
    Asynchronously read a JSON-RPC request from the given input channel.
    May raise `json_rpc.ParseError`, `json_rpc.InvalidRequestError` and
    `json_prc.InvalidParameterError`.
    """
    try:
        headers = await _read_headers(input_channel)
        content_length = _get_content_length(headers)

        payload = await input_channel.read_exactly(content_length)
        return json_rpc.Request.from_string(payload)
    except asyncio.IncompleteReadError as error:
        raise json_rpc.ParseError(str(error)) from error


async def write_json_rpc(
    output_channel: async_server_connection.TextWriter, response: json_rpc.JSONRPC
) -> None:
    """
    Asynchronously write a JSON-RPC response to the given output channel.
    """
    payload = response.serialize()
    await output_channel.write(f"Content-Length: {len(payload)}\r\n\r\n{payload}")


def _parse_parameters(parameters: json_rpc.Parameters, target: Type[T]) -> T:
    """
    Parse the given JSON-RPC parameters into specified LSP parameters.
    Raise `json_rpc.InvalidRequestError`on parsing failure.
    """
    if not isinstance(parameters, json_rpc.ByNameParameters):
        raise json_rpc.InvalidRequestError(
            "Parameters for LSP requests must be passed by name"
        )
    try:
        # pyre-fixme[16]: Pyre doesn't understand `dataclasses_json`
        return target.schema().load(parameters.values)
    except (KeyError, ValueError, dataclasses_json.mm.ValidationError) as error:
        raise json_rpc.InvalidRequestError(str(error)) from error


class SerializationSafeIntEnum(enum.IntEnum):
    def __repr(self) -> str:
        return str(self.value)


class DiagnosticTag(SerializationSafeIntEnum):
    UNNECESSARY = 1
    DEPRECATED = 2


class DiagnosticSeverity(SerializationSafeIntEnum):
    ERROR = 1
    WARNING = 2
    INFORMATION = 3
    HINT = 4


class TextDocumentSyncKind(SerializationSafeIntEnum):
    NONE = 0
    FULL = 1
    INCREMENTAL = 2


class MessageType(SerializationSafeIntEnum):
    ERROR = 1
    WARNING = 2
    INFO = 3
    LOG = 4


@dataclasses.dataclass(frozen=True)
class DocumentUri:
    scheme: str
    authority: str
    path: str
    query: str
    fragment: str

    def to_file_path(self) -> Optional[Path]:
        if self.scheme == "file":
            return Path(self.path)
        return None

    def unparse(self) -> str:
        return urllib.parse.urlunparse(
            (
                urllib.parse.quote(self.scheme),
                urllib.parse.quote(self.authority),
                urllib.parse.quote(self.path),
                "",
                urllib.parse.quote(self.query),
                urllib.parse.quote(self.fragment),
            )
        )

    @staticmethod
    def parse(uri: str) -> "DocumentUri":
        parsed_uri = urllib.parse.urlparse(uri)
        return DocumentUri(
            scheme=urllib.parse.unquote(parsed_uri.scheme),
            authority=urllib.parse.unquote(parsed_uri.netloc),
            path=urllib.parse.unquote(parsed_uri.path),
            query=urllib.parse.unquote(parsed_uri.query),
            fragment=urllib.parse.unquote(parsed_uri.fragment),
        )

    @staticmethod
    def from_file_path(file_path: Path) -> "DocumentUri":
        return DocumentUri(
            scheme="file", authority="", path=str(file_path), query="", fragment=""
        )


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True, order=True)
class Position:
    line: int
    character: int


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class LspPosition:
    """LSP uses 0-indexing for lines whereas Pyre uses 1-indexing."""

    line: int
    character: int

    def to_pyre_position(self) -> "Position":
        return Position(self.line + 1, self.character)


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class Range:
    start: Position
    end: Position


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class Diagnostic:
    range: Range
    message: str
    severity: Optional[DiagnosticSeverity] = None
    code: Optional[int] = None
    source: Optional[str] = None


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class Info:
    name: str
    version: Optional[str] = None


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class TextDocumentSyncClientCapabilities:
    did_save: bool = False


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class PublishDiagnosticsClientTagSupport:
    value_set: List[DiagnosticTag] = dataclasses.field(default_factory=list)


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class PublishDiagnosticsClientCapabilities:
    related_information: bool = False
    tag_support: Optional[PublishDiagnosticsClientTagSupport] = None
    version_support: bool = False


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class TextDocumentClientCapabilities:
    synchronization: Optional[TextDocumentSyncClientCapabilities] = None
    publish_diagnostics: Optional[PublishDiagnosticsClientCapabilities] = None


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class ShowStatusRequestClientCapabilities:
    pass


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class WindowClientCapabilities:
    work_done_progress: Optional[bool] = None
    # Custom VSCode extension for status bar
    status: Optional[ShowStatusRequestClientCapabilities] = None


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class ClientCapabilities:
    text_document: Optional[TextDocumentClientCapabilities] = None
    window: Optional[WindowClientCapabilities] = None


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class SaveOptions:
    include_text: Optional[bool] = None


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class TextDocumentSyncOptions:
    open_close: bool = False
    change: TextDocumentSyncKind = TextDocumentSyncKind.NONE
    save: Optional[SaveOptions] = None


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class ServerCapabilities:
    text_document_sync: Optional[TextDocumentSyncOptions] = None
    hover_provider: Optional[bool] = None


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class InitializationOptions:
    notebook_number: Optional[int] = None


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class InitializeParameters:
    capabilities: ClientCapabilities
    process_id: Optional[int] = None
    client_info: Optional[Info] = None
    initialization_options: Optional[InitializationOptions] = None

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "InitializeParameters":
        return _parse_parameters(parameters, target=InitializeParameters)


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class InitializeResult:
    capabilities: ServerCapabilities
    server_info: Optional[Info] = None


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class TextDocumentIdentifier:
    uri: str

    def document_uri(self) -> DocumentUri:
        return DocumentUri.parse(self.uri)


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class TextDocumentItem:
    uri: str
    language_id: str
    version: int
    text: str

    def document_uri(self) -> DocumentUri:
        return DocumentUri.parse(self.uri)


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class DidOpenTextDocumentParameters:
    text_document: TextDocumentItem

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "DidOpenTextDocumentParameters":
        return _parse_parameters(parameters, target=DidOpenTextDocumentParameters)


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class DidCloseTextDocumentParameters:
    text_document: TextDocumentIdentifier

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "DidCloseTextDocumentParameters":
        return _parse_parameters(parameters, target=DidCloseTextDocumentParameters)


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class DidSaveTextDocumentParameters:
    text_document: TextDocumentIdentifier
    text: Optional[str] = None

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "DidSaveTextDocumentParameters":
        return _parse_parameters(parameters, target=DidSaveTextDocumentParameters)


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class WorkspaceConfiguration:
    kernel_runtime_dir: List[str]


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class WorkspaceDidChangeConfigurationParameters:
    settings: WorkspaceConfiguration

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "WorkspaceDidChangeConfigurationParameters":
        return _parse_parameters(
            parameters, target=WorkspaceDidChangeConfigurationParameters
        )


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class HoverTextDocumentParameters:
    text_document: TextDocumentIdentifier
    position: LspPosition

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "HoverTextDocumentParameters":
        return _parse_parameters(parameters, target=HoverTextDocumentParameters)


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class HoverResponse:
    """Contains the Markdown response to be shown in the hover card."""

    contents: str

    @staticmethod
    def empty() -> "HoverResponse":
        return HoverResponse(contents="")


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class TypeCoverageTextDocumentParameters:
    text_document: TextDocumentIdentifier

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "TypeCoverageTextDocumentParameters":
        return _parse_parameters(parameters, target=TypeCoverageTextDocumentParameters)


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class TypeCoverageResult:
    """Result for nuclide-vscode-lsp coverage feature."""

    covered_percent: float
    uncovered_ranges: List[Diagnostic]
    default_message: str
