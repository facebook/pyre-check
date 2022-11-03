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

import dataclasses
import json
from pathlib import Path
from typing import Any, Dict, List, Optional, Type, TypeVar, Union

from .. import dataclasses_json_extensions as json_mixins

from . import daemon_connection, protocol as lsp


@dataclasses.dataclass(frozen=True)
class HoverRequest:
    path: Path
    overlay_id: Optional[str]
    position: lsp.PyrePosition

    def to_json(self) -> List[object]:
        return [
            "Hover",
            {
                "module": ["OfPath", f"{self.path}"],
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
class HoverContent(json_mixins.CamlCaseAndExcludeJsonMixin):
    kind: List[str]
    value: str


@dataclasses.dataclass(frozen=True)
class HoverResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    contents: List[HoverContent]


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
) -> Union[lsp.LspHoverResponse, ErrorResponse]:
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
    return lsp.LspHoverResponse(
        "\n".join(content.value for content in response.contents)
    )
