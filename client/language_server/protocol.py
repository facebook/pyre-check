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
from typing import Dict, Iterable, List, Optional, Type, TypeVar

import dataclasses_json
from pyre_extensions import override

from .. import dataclasses_json_extensions as json_mixins, json_rpc
from . import connections

LOG: logging.Logger = logging.getLogger(__name__)

T = TypeVar("T", bound=json_mixins.DataclassJsonMixinWithCachedSchema)
Point = TypeVar("Point")
Value = TypeVar("Value")


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


class SymbolKind(enum.IntEnum):
    FILE = 1
    MODULE = 2
    NAMESPACE = 3
    PACKAGE = 4
    CLASS = 5
    METHOD = 6
    PROPERTY = 7
    FIELD = 8
    CONSTRUCTOR = 9
    ENUM = 10
    INTERFACE = 11
    FUNCTION = 12
    VARIABLE = 13
    CONSTANT = 14
    STRING = 15
    NUMBER = 16
    BOOLEAN = 17
    ARRAY = 18
    OBJECT = 19
    KEY = 20
    NULL = 21
    ENUMMEMBER = 22
    STRUCT = 23
    EVENT = 24
    OPERATOR = 25
    TYPEPARAMETER = 26


class PyreCallHierarchyRelationDirection(str, enum.Enum):
    PARENT: str = "PARENT"
    CHILD: str = "CHILD"

    def is_parent(self) -> bool:
        return self.value == self.PARENT

    def is_child(self) -> bool:
        return self.value == self.CHILD


class CompletionItemKind(enum.IntEnum):
    TEXT = 1
    METHOD = 2
    FUNCTION = 3
    CONSTRUCTOR = 4
    FIELD = 5
    VARIABLE = 6
    CLASS = 7
    INTERFACE = 8
    MODULE = 9
    PROPERTY = 10
    UNIT = 11
    VALUE = 12
    ENUM = 13
    KEYWORD = 14
    SNIPPET = 15
    COLOR = 16
    FILE = 17
    REFERENCE = 18
    FOLDER = 19
    ENUMMEMBER = 20
    CONSTANT = 21
    STRUCT = 22
    EVENT = 23
    OPERATOR = 24
    TYPEPARAMETER = 25


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
    code: Optional[int | str] = None
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
    hover_provider: Optional[bool] = None
    definition_provider: Optional[bool] = None
    document_symbol_provider: Optional[bool] = None
    references_provider: Optional[bool] = None
    completion_provider: Optional[bool] = None
    call_hierarchy_provider: Optional[bool] = None
    rename_provider: Optional[bool] = None
    workspace_symbol_provider: Optional[bool] = None
    inlay_hint_provider: Optional[bool] = None
    document_formatting_provider: Optional[bool] = None


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
class HoverParameters(json_mixins.CamlCaseAndExcludeJsonMixin):
    text_document: TextDocumentIdentifier
    position: LspPosition

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "HoverParameters":
        return _parse_parameters(parameters, target=HoverParameters)


@dataclasses.dataclass(frozen=True)
class LspHoverResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    """Contains the Markdown response to be shown in the hover card."""

    contents: str

    @staticmethod
    def from_pyre_hover_responses(
        responses: List["PyreHoverResponse"],
    ) -> Optional["LspHoverResponse"]:
        lsp_hover_responses = [
            lsp_response
            for lsp_response in (hover.to_lsp_hover_response() for hover in responses)
            if lsp_response is not None
        ]
        return (
            None
            if len(lsp_hover_responses) == 0
            else LspHoverResponse(
                "\n".join(response.contents for response in lsp_hover_responses)
            )
        )


@dataclasses.dataclass(frozen=True)
class PyreHoverResponse(json_mixins.CamlCaseAndExcludeJsonMixin):

    value: Optional[str] = None
    docstring: Optional[str] = None

    def to_lsp_hover_response(self) -> Optional[LspHoverResponse]:
        lines = []
        if self.value:
            lines.append(f"```\n{self.value}\n```" if self.value else "")
        if self.docstring:
            lines.append(self.docstring)
        if len(lines) == 0:
            return None
        return LspHoverResponse("\n".join(lines))


@dataclasses.dataclass(frozen=True)
class ReferencesParameters(json_mixins.CamlCaseAndExcludeJsonMixin):
    text_document: TextDocumentIdentifier
    position: LspPosition

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "ReferencesParameters":
        return _parse_parameters(parameters, target=ReferencesParameters)


@dataclasses.dataclass(frozen=True)
class ReferencesResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    """Contains code location of one reference."""

    path: str
    range: PyreRange

    def to_lsp_references_response(
        self,
    ) -> "LspLocation":
        return LspLocation(uri=self.path, range=self.range.to_lsp_range())


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
class DefinitionParameters(json_mixins.CamlCaseAndExcludeJsonMixin):
    text_document: TextDocumentIdentifier
    position: LspPosition

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "DefinitionParameters":
        return _parse_parameters(parameters, target=DefinitionParameters)


@dataclasses.dataclass(frozen=True)
class DefinitionResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    """Contains one possible definition for a symbol."""

    path: str
    range: PyreRange

    def to_lsp_definition_response(
        self,
    ) -> "LspLocation":
        return LspLocation(uri=self.path, range=self.range.to_lsp_range())


@dataclasses.dataclass(frozen=True)
class CallHierarchyParameters(json_mixins.CamlCaseAndExcludeJsonMixin):
    text_document: TextDocumentIdentifier
    position: LspPosition

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "CallHierarchyParameters":
        return _parse_parameters(parameters, target=CallHierarchyParameters)


@dataclasses.dataclass(frozen=True)
class CallHierarchyItem(json_mixins.CamlCaseAndExcludeJsonMixin):
    name: str
    kind: SymbolKind
    uri: str

    """
    The range enclosing this symbol not including leading/trailing whitespace
    but everything else, e.g. comments and code.
    """
    range: LspRange

    """
    The range that should be selected and revealed when this symbol is being
    picked, e.g. the name of a function. Must be contained by the
    CallHierarchyItem.range).
    """
    selection_range: LspRange

    def document_uri(self) -> DocumentUri:
        return DocumentUri.parse(self.uri)


@dataclasses.dataclass(frozen=True)
class CallHierarchyIncomingCallParameters(json_mixins.CamlCaseAndExcludeJsonMixin):
    item: CallHierarchyItem

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "CallHierarchyIncomingCallParameters":
        return _parse_parameters(parameters, target=CallHierarchyIncomingCallParameters)


@dataclasses.dataclass(frozen=True)
class CallHierarchyIncomingCall(json_mixins.CamlCaseAndExcludeJsonMixin):
    from_: CallHierarchyItem = field(
        metadata=dataclasses_json.config(field_name="from")
    )
    from_ranges: List[LspRange]


@dataclasses.dataclass(frozen=True)
class CallHierarchyOutgoingCallParameters(json_mixins.CamlCaseAndExcludeJsonMixin):
    item: CallHierarchyItem

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "CallHierarchyOutgoingCallParameters":
        return _parse_parameters(parameters, target=CallHierarchyOutgoingCallParameters)


@dataclasses.dataclass(frozen=True)
class CallHierarchyOutgoingCall(json_mixins.CamlCaseAndExcludeJsonMixin):
    to: CallHierarchyItem
    from_ranges: List[LspRange]


@dataclasses.dataclass(frozen=True)
class CompletionParameters(json_mixins.CamlCaseAndExcludeJsonMixin):
    text_document: TextDocumentIdentifier
    position: LspPosition

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "CompletionParameters":
        return _parse_parameters(parameters, target=CompletionParameters)


@dataclasses.dataclass(frozen=True)
class LspLocation(json_mixins.CamlCaseAndExcludeJsonMixin):
    """Contains the start and end column and row for a symbol."""

    uri: str
    range: LspRange


@dataclasses.dataclass(frozen=True)
class DocumentSymbolsParameters(json_mixins.CamlCaseAndExcludeJsonMixin):
    text_document: TextDocumentIdentifier

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "DocumentSymbolsParameters":
        return _parse_parameters(parameters, target=DocumentSymbolsParameters)


@dataclasses.dataclass(frozen=True)
class DocumentSymbol(json_mixins.CamlCaseAndExcludeJsonMixin):
    """Contains detailed information about a specified symbol."""

    name: str
    detail: Optional[str]
    kind: SymbolKind
    range: LspRange
    selection_range: LspRange
    children: List["DocumentSymbol"]


@dataclasses.dataclass(frozen=True)
class DocumentSymbolRequest:
    path: str
    client_id: str

    def to_json(self) -> List[object]:
        return [
            "DocumentSymbol",
            {
                "path": self.path,
                "client_id": self.client_id,
            },
        ]


@dataclasses.dataclass(frozen=True)
class CompletionRequest:
    path: str
    client_id: str
    position: PyrePosition

    def to_json(self) -> List[object]:
        return [
            "Completion",
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
class CompletionItem(json_mixins.CamlCaseAndExcludeJsonMixin):
    label: str
    kind: Optional[CompletionItemKind]
    sortText: str
    filterText: str
    detail: Optional[str]


@dataclasses.dataclass(frozen=True)
class CompletionResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    completions: List[CompletionItem]


@dataclasses.dataclass(frozen=True)
class TextEdit(json_mixins.CamlCaseAndExcludeJsonMixin):
    range: LspRange
    new_text: str


@dataclasses.dataclass(frozen=True)
class WorkspaceEdit(json_mixins.CamlCaseAndExcludeJsonMixin):
    changes: Optional[Dict[str, List[TextEdit]]]


@dataclasses.dataclass(frozen=True)
class RenameParameters(json_mixins.CamlCaseAndExcludeJsonMixin):
    text_document: TextDocumentIdentifier
    position: LspPosition
    new_name: str

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "RenameParameters":
        return _parse_parameters(parameters, target=RenameParameters)


@dataclasses.dataclass(frozen=True)
class WorkspaceSymbolParameters(json_mixins.CamlCaseAndExcludeJsonMixin):
    query: str

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "WorkspaceSymbolParameters":
        return _parse_parameters(parameters, target=WorkspaceSymbolParameters)


@dataclasses.dataclass(frozen=True)
class DocumentFormattingParameters(json_mixins.CamlCaseAndExcludeJsonMixin):
    text_document: TextDocumentIdentifier

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "DocumentFormattingParameters":
        return _parse_parameters(parameters, target=DocumentFormattingParameters)


@dataclasses.dataclass(frozen=True)
class DocumentFormattingResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    """describing the modification to the document to be formatted"""

    list_of_edits: List[TextEdit]

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "DocumentFormattingResponse":
        return _parse_parameters(parameters, target=DocumentFormattingResponse)


@dataclasses.dataclass(frozen=True)
class WorkspaceSymbol(json_mixins.CamlCaseAndExcludeJsonMixin):
    name: str
    kind: SymbolKind
    container_name: Optional[str]
    location: LspLocation

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "WorkspaceSymbol":
        return _parse_parameters(parameters, target=WorkspaceSymbol)


@dataclasses.dataclass(frozen=True)
class WorkspaceSymbolResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    workspace_symbols: List[WorkspaceSymbol]

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "WorkspaceSymbolResponse":
        return _parse_parameters(parameters, target=WorkspaceSymbolResponse)
