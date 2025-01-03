# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module contains two main responsibilities:
1. Provide all the different data models representing requests/responses for the LSP client
to/from the persistent Pyre client (such as hover, definition, etc.)

2. Provide protocols for reading requests, parsing, and writing responses to/from
the LSP client. This currently involves doing some kind of transformation to/from a JSON string
to the specific representation in class form.
"""

import asyncio
import dataclasses
import enum
import logging
import urllib
from dataclasses import field
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Type, TypeVar, Union

import dataclasses_json
from pyre_extensions import override

from .. import dataclasses_json_extensions as json_mixins, json_rpc
from . import connections

LOG: logging.Logger = logging.getLogger(__name__)

T = TypeVar("T", bound=json_mixins.DataclassJsonMixinWithCachedSchema)


class ServerNotInitializedError(json_rpc.JSONRPCException):
    @override
    def error_code(self) -> int:
        return -32002


class RequestFailedError(json_rpc.JSONRPCException):
    @override
    def error_code(self) -> int:
        return -32803


class RequestCancelledError(json_rpc.JSONRPCException):
    @override
    def error_code(self) -> int:
        return -32800


class ReadChannelClosedError(Exception):
    pass


async def _read_headers(
    input_channel: connections.AsyncTextReader,
) -> List[str]:
    headers = []
    header = await input_channel.read_until("\r\n")
    while header != "\r\n":
        headers.append(header)
        header = await input_channel.read_until("\r\n")
    return headers


def _get_content_length(headers: Iterable[str]) -> int:
    try:
        parts: List[str] = []
        for header in headers:
            parts = [part.strip().lower() for part in header.split(":", maxsplit=1)]
            if len(parts) <= 1:
                continue

            if parts[0] == "content-length":
                return int(parts[1])

        raise json_rpc.ParseError(f"Failed to find content length header from {parts}")
    except ValueError as error:
        raise json_rpc.ParseError(
            "Cannot parse content length into integer."
        ) from error


async def _try_read_json_rpc(
    input_channel: connections.AsyncTextReader,
) -> json_rpc.Request:
    """
    Asynchronously read a JSON-RPC request from the given input channel.

    May raise `json_rpc.ParseError`, `json_rpc.InvalidRequestError`,
    `json_prc.InvalidParameterError`, and `ReadChannelClosedError`.

    This is expected to throw errors if the editor sends empty requests,
    most callsites should use `read_nonempty_json_rpc_request`
    """
    try:
        headers = await _read_headers(input_channel)
        content_length = _get_content_length(headers)
        payload = await input_channel.read_exactly(content_length)
        return json_rpc.Request.from_string(payload)
    except asyncio.IncompleteReadError as error:
        if len(error.partial) == 0:
            raise ReadChannelClosedError(
                "Trying to read from a closed input channel"
            ) from None
        else:
            raise json_rpc.ParseError(str(error)) from None


async def read_json_rpc(
    input_channel: connections.AsyncTextReader,
) -> json_rpc.Request:
    """
    Read a JSON-RPC request from the given input channel. Ignores
    any requests that are missing the "method" field, which is needed
    because VSCode often sends empty requests.

    May raise `json_rpc.ParseError`, `json_rpc.InvalidRequestError`,
    `json_prc.InvalidParameterError`, and `ReadChannelClosedError`.
    """
    while True:
        try:
            return await _try_read_json_rpc(input_channel)
        except json_rpc.MissingMethodFieldInRequestError:
            continue


def json_rpc_payload(message: json_rpc.JSONRPC) -> str:
    payload = message.serialize()
    return f"Content-Length: {len(payload)}\r\n\r\n{payload}"


async def write_json_rpc(
    output_channel: connections.AsyncTextWriter,
    response: json_rpc.JSONRPC,
) -> None:
    """
    Asynchronously write a JSON-RPC response to the given output channel.
    """
    await output_channel.write(json_rpc_payload(response))


async def write_json_rpc_ignore_connection_error(
    output_channel: connections.AsyncTextWriter,
    response: json_rpc.JSONRPC,
) -> None:
    """
    Asynchronously write a JSON-RPC response to the given output channel, and ignore
    any `ConnectionError` that occurred.
    """
    try:
        await write_json_rpc(output_channel, response)
    except ConnectionError as error:
        LOG.info(f"Ignoring connection error while writing JSON RPC. Error: {error}")


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
        # pyre-ignore[6, 7]: Imprecise typing of `load()`
        return target.cached_schema().load(parameters.values)
    except (KeyError, ValueError, dataclasses_json.mm.ValidationError) as error:
        raise json_rpc.InvalidRequestError(str(error)) from error


class DiagnosticTag(enum.IntEnum):
    UNNECESSARY = 1
    DEPRECATED = 2


class DiagnosticSeverity(enum.IntEnum):
    ERROR = 1
    WARNING = 2
    INFORMATION = 3
    HINT = 4


class TextDocumentSyncKind(enum.IntEnum):
    NONE = 0
    FULL = 1
    INCREMENTAL = 2


class MessageType(enum.IntEnum):
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


@dataclasses.dataclass(frozen=True, order=True)
class PyrePosition(json_mixins.CamlCaseAndExcludeJsonMixin):
    line: int
    character: int

    def to_lsp_position(self) -> "LspPosition":
        return LspPosition(self.line - 1, self.character)


@dataclasses.dataclass(frozen=True)
class LspPosition(json_mixins.CamlCaseAndExcludeJsonMixin):
    """LSP uses 0-indexing for lines whereas Pyre uses 1-indexing."""

    line: int
    character: int

    def to_pyre_position(self) -> "PyrePosition":
        return PyrePosition(self.line + 1, self.character)


@dataclasses.dataclass(frozen=True)
class PyreRange(json_mixins.CamlCaseAndExcludeJsonMixin):
    start: PyrePosition
    end: PyrePosition

    def to_lsp_range(self) -> "LspRange":
        return LspRange(
            start=self.start.to_lsp_position(),
            end=self.end.to_lsp_position(),
        )


@dataclasses.dataclass(frozen=True)
class LspRange(json_mixins.CamlCaseAndExcludeJsonMixin):
    start: LspPosition
    end: LspPosition


@dataclasses.dataclass(frozen=True)
class CodeDescription(json_mixins.CamlCaseAndExcludeJsonMixin):
    href: str


@dataclasses.dataclass(frozen=True)
class Diagnostic(json_mixins.CamlCaseAndExcludeJsonMixin):
    range: LspRange
    message: str
    severity: Optional[DiagnosticSeverity] = None
    code: Optional[Union[int, str]] = None
    code_description: Optional[CodeDescription] = None
    source: Optional[str] = None


@dataclasses.dataclass(frozen=True)
class Info(json_mixins.CamlCaseAndExcludeJsonMixin):
    name: str
    version: Optional[str] = None


@dataclasses.dataclass(frozen=True)
class TextDocumentSyncClientCapabilities(json_mixins.CamlCaseAndExcludeJsonMixin):
    did_save: bool = False


@dataclasses.dataclass(frozen=True)
class PublishDiagnosticsClientTagSupport(json_mixins.CamlCaseAndExcludeJsonMixin):
    value_set: List[DiagnosticTag] = dataclasses.field(default_factory=list)


@dataclasses.dataclass(frozen=True)
class PublishDiagnosticsClientCapabilities(json_mixins.CamlCaseAndExcludeJsonMixin):
    related_information: bool = False
    tag_support: Optional[PublishDiagnosticsClientTagSupport] = None
    version_support: bool = False


@dataclasses.dataclass(frozen=True)
class TextDocumentClientCapabilities(json_mixins.CamlCaseAndExcludeJsonMixin):
    synchronization: Optional[TextDocumentSyncClientCapabilities] = None
    publish_diagnostics: Optional[PublishDiagnosticsClientCapabilities] = None


@dataclasses.dataclass(frozen=True)
class ShowStatusRequestClientCapabilities(json_mixins.CamlCaseAndExcludeJsonMixin):
    pass


@dataclasses.dataclass(frozen=True)
class WindowClientCapabilities(json_mixins.CamlCaseAndExcludeJsonMixin):
    work_done_progress: Optional[bool] = None
    # Custom VSCode extension for status bar
    status: Optional[ShowStatusRequestClientCapabilities] = None


@dataclasses.dataclass(frozen=True)
class ClientCapabilities(json_mixins.CamlCaseAndExcludeJsonMixin):
    text_document: Optional[TextDocumentClientCapabilities] = None
    window: Optional[WindowClientCapabilities] = None


@dataclasses.dataclass(frozen=True)
class SaveOptions(json_mixins.CamlCaseAndExcludeJsonMixin):
    include_text: Optional[bool] = None


@dataclasses.dataclass(frozen=True)
class TextDocumentSyncOptions(json_mixins.CamlCaseAndExcludeJsonMixin):
    open_close: bool = False
    change: TextDocumentSyncKind = TextDocumentSyncKind.NONE
    save: Optional[SaveOptions] = None


@dataclasses.dataclass(frozen=True)
class ServerCapabilities(json_mixins.CamlCaseAndExcludeJsonMixin):
    text_document_sync: Optional[TextDocumentSyncOptions] = None


@dataclasses.dataclass(frozen=True)
class InitializationOptions(json_mixins.CamlCaseAndExcludeJsonMixin):
    notebook_number: Optional[int] = None


@dataclasses.dataclass(frozen=True)
class InitializeParameters(json_mixins.CamlCaseAndExcludeJsonMixin):
    capabilities: ClientCapabilities
    process_id: Optional[int] = None
    client_info: Optional[Info] = None
    initialization_options: Optional[InitializationOptions] = None

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "InitializeParameters":
        return _parse_parameters(parameters, target=InitializeParameters)


@dataclasses.dataclass(frozen=True)
class InitializeResult(json_mixins.CamlCaseAndExcludeJsonMixin):
    capabilities: ServerCapabilities
    server_info: Optional[Info] = None


@dataclasses.dataclass(frozen=True)
class TextDocumentIdentifier(json_mixins.CamlCaseAndExcludeJsonMixin):
    uri: str

    def document_uri(self) -> DocumentUri:
        return DocumentUri.parse(self.uri)


@dataclasses.dataclass(frozen=True)
class TextDocumentItem(json_mixins.CamlCaseAndExcludeJsonMixin):
    uri: str
    language_id: str
    version: int
    text: str

    def document_uri(self) -> DocumentUri:
        return DocumentUri.parse(self.uri)


@dataclasses.dataclass(frozen=True)
class ContentChange(json_mixins.CamlCaseAndExcludeJsonMixin):
    text: str


@dataclasses.dataclass(frozen=True)
class DidOpenTextDocumentParameters(json_mixins.CamlCaseAndExcludeJsonMixin):
    text_document: TextDocumentItem

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "DidOpenTextDocumentParameters":
        return _parse_parameters(parameters, target=DidOpenTextDocumentParameters)


@dataclasses.dataclass(frozen=True)
class DidCloseTextDocumentParameters(json_mixins.CamlCaseAndExcludeJsonMixin):
    text_document: TextDocumentIdentifier

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "DidCloseTextDocumentParameters":
        return _parse_parameters(parameters, target=DidCloseTextDocumentParameters)


@dataclasses.dataclass(frozen=True)
class DidChangeTextDocumentParameters(json_mixins.CamlCaseAndExcludeJsonMixin):
    text_document: TextDocumentIdentifier
    content_changes: List[ContentChange]

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "DidChangeTextDocumentParameters":
        return _parse_parameters(parameters, target=DidChangeTextDocumentParameters)


@dataclasses.dataclass(frozen=True)
class DidSaveTextDocumentParameters(json_mixins.CamlCaseAndExcludeJsonMixin):
    text_document: TextDocumentIdentifier
    text: Optional[str] = None

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "DidSaveTextDocumentParameters":
        return _parse_parameters(parameters, target=DidSaveTextDocumentParameters)


@dataclasses.dataclass(frozen=True)
class WorkspaceConfiguration(json_mixins.CamlCaseAndExcludeJsonMixin):
    kernel_runtime_dir: List[str]


@dataclasses.dataclass(frozen=True)
class WorkspaceDidChangeConfigurationParameters(
    json_mixins.CamlCaseAndExcludeJsonMixin
):
    settings: WorkspaceConfiguration

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "WorkspaceDidChangeConfigurationParameters":
        return _parse_parameters(
            parameters, target=WorkspaceDidChangeConfigurationParameters
        )


@dataclasses.dataclass(frozen=True)
class TypeCoverageParameters(json_mixins.CamlCaseAndExcludeJsonMixin):
    text_document: TextDocumentIdentifier

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "TypeCoverageParameters":
        return _parse_parameters(parameters, target=TypeCoverageParameters)


@dataclasses.dataclass(frozen=True)
class TypeCoverageResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    """Result for nuclide-vscode-lsp coverage feature."""

    covered_percent: float
    uncovered_ranges: List[Diagnostic]
    default_message: str


@dataclasses.dataclass(frozen=True)
class LspLocation(json_mixins.CamlCaseAndExcludeJsonMixin):
    """Contains the start and end column and row for a symbol."""

    uri: str
    range: LspRange
