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
class TypeErrorsResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    errors: List[Dict[str, Any]]

    def to_errors_response(self) -> List[error.Error]:
        return [error.Error.from_json(error_response) for error_response in self.errors]


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


def invalid_response(response: str, raw_request: str) -> ErrorResponse:
    return ErrorResponse(
        message=f"Invalid response {response} to pyre language server request.",
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
