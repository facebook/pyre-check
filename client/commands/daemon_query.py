# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import dataclasses

import json
import logging
from pathlib import Path

from typing import Optional, Type, TypeVar

import dataclasses_json

from .. import dataclasses_json_extensions as json_mixins

from . import daemon_connection


QueryResponseType = TypeVar(
    "QueryResponseType", bound=json_mixins.CamlCaseAndExcludeJsonMixin
)

LOG: logging.Logger = logging.getLogger(__name__)


class InvalidQueryResponse(Exception):
    pass


@dataclasses.dataclass(frozen=True)
class Response:
    payload: object

    @staticmethod
    def from_json(
        response_json: object,
    ) -> Response:
        if (
            isinstance(response_json, list)
            and len(response_json) > 1
            and response_json[0] == "Query"
        ):
            return Response(response_json[1])
        else:
            raise InvalidQueryResponse(
                f"Unexpected JSON response from server: {response_json}"
            )

    @staticmethod
    def parse(
        response_text: str,
    ) -> Response:
        try:
            response_json = json.loads(response_text)
            return Response.from_json(response_json)
        except json.JSONDecodeError as decode_error:
            message = f"Cannot parse response as JSON: {decode_error}"
            raise InvalidQueryResponse(message) from decode_error


def execute_query(socket_path: Path, query_text: str) -> Response:
    raw_request = json.dumps(["Query", query_text])
    raw_response = daemon_connection.send_raw_request(socket_path, raw_request)
    return Response.parse(raw_response)


async def attempt_async_query(
    socket_path: Path,
    query_text: str,
    overlay_id: Optional[str] = None,
) -> Optional[Response]:
    response_text = await daemon_connection.attempt_send_async_raw_request(
        socket_path=socket_path,
        request=json.dumps(
            # TODO:T126924773 send a regular Query when overlay id is None
            [
                "QueryWithOverlay",
                {"query_text": query_text, "overlay_id": overlay_id},
            ]
        ),
    )
    try:
        return Response.parse(response_text) if response_text else None
    except InvalidQueryResponse as exception:
        LOG.info(f"Failed to parse json {response_text} due to exception: {exception}")
        return None


async def attempt_typed_async_query(
    response_type: Type[QueryResponseType],
    socket_path: Path,
    query_text: str,
    overlay_id: Optional[str] = None,
) -> Optional[QueryResponseType]:
    try:
        response = await attempt_async_query(
            socket_path,
            query_text,
            overlay_id,
        )
        if response is None:
            return None
        else:
            if not isinstance(response.payload, dict):
                raise ValueError(
                    f"Expected a doct, got {response.payload!r} as response"
                )
            return response_type.from_dict(response.payload)
    except (
        KeyError,
        ValueError,
        dataclasses_json.mm.ValidationError,
    ) as exception:
        LOG.info(
            f"When interpretting {response.payload} as {response_type.__name__} "
            f"got: {type(exception).__name__}({exception})"
        )
        return None
