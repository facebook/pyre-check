# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module contains the definition of code navigation requests and an API to convert a given LSP request
to a corresponding code navigation request. Also contains an API that sends a given request to the code navigation
server and gets a response.
"""
from __future__ import annotations

import abc
import dataclasses
import json
from pathlib import Path
from typing import Any, Dict, List, Optional, Type, TypeVar, Union

import dataclasses_json

from .. import dataclasses_json_extensions as json_mixins

from . import daemon_connection, protocol as lsp

from .protocol import PyreHoverResponse


class Module(abc.ABC):
    @staticmethod
    def module_to_json(module: Module) -> List[object]:
        if isinstance(module, ModuleOfName):
            return ["OfName", module.name]
        assert isinstance(module, ModuleOfPath)
        return ["OfPath", f"{module.path}"]

    @staticmethod
    def module_from_json(module_json: List[object]) -> Module:
        if not isinstance(module_json, list):
            raise AssertionError("JSON is not a list")
        if (
            len(module_json) != 2
            or module_json[0] not in ("OfName", "OfPath")
            or not isinstance(module_json[1], str)
        ):
            raise AssertionError(
                'JSON must be a list of form ["OfName"/"OfPath", module]'
            )
        if module_json[0] == "OfName":
            return ModuleOfName(name=str(module_json[1]))
        return ModuleOfPath(path=Path(str(module_json[1])))

    def to_json(self) -> List[object]:
        return Module.module_to_json(self)


@dataclasses.dataclass(frozen=True)
class ModuleOfName(Module):
    name: str


@dataclasses.dataclass(frozen=True)
class ModuleOfPath(Module):
    path: Path


@dataclasses.dataclass(frozen=True)
class HoverRequest:
    module: Module
    overlay_id: Optional[str]
    position: lsp.PyrePosition

    def to_json(self) -> List[object]:
        return [
            "Hover",
            {
                "module": self.module.to_json(),
                "overlay_id": self.overlay_id,
                "position": {
                    "line": self.position.line,
                    "column": self.position.character,
                },
            },
        ]


@dataclasses.dataclass(frozen=True)
class LocationOfDefinitionRequest:
    module: Module
    overlay_id: Optional[str]
    position: lsp.PyrePosition

    def to_json(self) -> List[object]:
        return [
            "LocationOfDefinition",
            {
                "module": self.module.to_json(),
                "overlay_id": self.overlay_id,
                "position": {
                    "line": self.position.line,
                    "column": self.position.character,
                },
            },
        ]


@dataclasses.dataclass(frozen=True)
class ErrorResponse:
    message: str


@dataclasses.dataclass(frozen=True)
class HoverResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    contents: List[PyreHoverResponse]


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
class DefinitionResponse:
    path: str
    range: CodeNavigationRange

    def to_lsp_definition_response(self) -> lsp.LspLocation:
        return lsp.LspLocation(uri=self.path, range=self.range.to_lsp_range())


@dataclasses.dataclass(frozen=True)
class LocationOfDefinitionResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    definitions: List[DefinitionResponse]


@dataclasses.dataclass(frozen=True)
class LocalUpdate:
    module: Module
    content: str
    overlay_id: str

    def to_json(self) -> List[object]:
        return [
            "LocalUpdate",
            {
                "module": self.module.to_json(),
                "content": self.content,
                "overlay_id": self.overlay_id,
            },
        ]


@dataclasses.dataclass(frozen=True)
class FileOpened:
    path: Path
    content: Optional[str]
    overlay_id: Optional[str]

    def to_json(self) -> List[object]:
        return [
            "FileOpened",
            {
                "path": f"{self.path}",
                "content": self.content,
                "overlay_id": self.overlay_id,
            },
        ]


@dataclasses.dataclass(frozen=True)
class FileClosed:
    path: Path
    overlay_id: Optional[str]

    def to_json(self) -> List[object]:
        return [
            "FileClosed",
            {
                "path": f"{self.path}",
                "overlay_id": self.overlay_id,
            },
        ]


@dataclasses.dataclass(frozen=True)
class ClassExpression(json_mixins.SnakeCaseAndExcludeJsonMixin):
    module: Module = dataclasses.field(
        metadata=dataclasses_json.config(
            encoder=Module.module_to_json, decoder=Module.module_from_json
        )
    )
    qualified_name: str


@dataclasses.dataclass(frozen=True)
class SuperclassesRequest:
    class_: ClassExpression
    overlay_id: str | None

    def to_json(self) -> List[object]:
        return [
            "Superclasses",
            {"class": self.class_.to_dict(), "overlay_id": self.overlay_id},
        ]


@dataclasses.dataclass(frozen=True)
class SuperclassesResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    superclasses: List[ClassExpression]


def invalid_response(response: str) -> ErrorResponse:
    return ErrorResponse(message=f"Invalid response {response} to hover request.")


ResponseKind = TypeVar("ResponseKind", bound=json_mixins.CamlCaseAndExcludeJsonMixin)


def parse_response(
    response: Dict[str, Any], response_type: Type[ResponseKind]
) -> ResponseKind | ErrorResponse:
    try:
        return response_type.cached_schema().load(response)
    except AssertionError as error:
        return ErrorResponse(
            message=f"Assertion error when parsing JSON into the response schema: {error}"
        )


def parse_raw_response(
    raw_response: str, expected_response_kind: str, response_type: Type[ResponseKind]
) -> ResponseKind | ErrorResponse:
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
        return ErrorResponse(message=response.error_message)
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
        return ErrorResponse(message=response.error_message)
    return parse_raw_response(
        response,
        expected_response_kind="LocationOfDefinition",
        response_type=LocationOfDefinitionResponse,
    )


async def async_handle_local_update(
    socket_path: Path, local_update: LocalUpdate
) -> str | daemon_connection.DaemonConnectionFailure:
    raw_command = json.dumps(["Command", local_update.to_json()])
    response = await daemon_connection.attempt_send_async_raw_request(
        socket_path, raw_command
    )
    return response


async def async_handle_file_opened(
    socket_path: Path, file_opened: FileOpened
) -> str | daemon_connection.DaemonConnectionFailure:
    raw_command = json.dumps(["Command", file_opened.to_json()])
    response = await daemon_connection.attempt_send_async_raw_request(
        socket_path, raw_command
    )
    return response


async def async_handle_file_closed(
    socket_path: Path, file_closed: FileClosed
) -> str | daemon_connection.DaemonConnectionFailure:
    raw_command = json.dumps(["Command", file_closed.to_json()])
    response = await daemon_connection.attempt_send_async_raw_request(
        socket_path, raw_command
    )
    return response


async def async_handle_superclasses(
    socket_path: Path,
    superclasses: SuperclassesRequest,
) -> str | daemon_connection.DaemonConnectionFailure:
    raw_command = json.dumps(["Query", superclasses.to_json()])
    response = await daemon_connection.attempt_send_async_raw_request(
        socket_path, raw_command
    )
    return response
