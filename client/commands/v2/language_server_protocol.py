# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio
import dataclasses
import enum
from typing import List, Iterable, Optional

import dataclasses_json

from ... import json_rpc
from . import async_server_connection


class ServerNotInitializedError(json_rpc.JSONRPCException):
    def error_code(self) -> int:
        return -32002


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


class SerializationSafeIntEnum(enum.IntEnum):
    def __repr(self) -> str:
        return str(self.value)


class DiagnosticTag(SerializationSafeIntEnum):
    UNNECESSARY = 1
    DEPRECATED = 2


class TextDocumentSyncKind(SerializationSafeIntEnum):
    NONE = 0
    FULL = 1
    INCREMENTAL = 2


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
class ClientCapabilities:
    text_document: Optional[TextDocumentClientCapabilities] = None


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


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class InitializeParameters:
    capabilities: ClientCapabilities
    process_id: Optional[int] = None
    client_info: Optional[Info] = None

    @staticmethod
    def from_json_rpc_parameters(
        parameters: json_rpc.Parameters,
    ) -> "InitializeParameters":
        if not isinstance(parameters, json_rpc.ByNameParameters):
            raise json_rpc.InvalidRequestError(
                "Parameters for initialize request must be passed by name"
            )
        try:
            # pyre-fixme[16]: Pyre doesn't understand `dataclasses_json`
            return InitializeParameters.schema().load(parameters.values)
        except (KeyError, ValueError, dataclasses_json.mm.ValidationError) as error:
            raise json_rpc.InvalidRequestError(str(error)) from error


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class InitializeResult:
    capabilities: ServerCapabilities
    server_info: Optional[Info] = None
