# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
The daemon query module contains APIs for sending requests to the Pyre daemon.
The responsibility of this layer is to serialize queries and send them over the connection,
but this module is not responsible for constructing the query/parsing the response
(which is left to the higher layers). It is also not responsible for the low-level details
of how the message is sent over the connection.

Note that this module does not handle requests of every format (such as overlay updates
nor incremental updates), it only handles queries.
"""


from __future__ import annotations

import dataclasses

import json
import logging
from pathlib import Path

from typing import List, Optional, Type, TypeVar, Union

import dataclasses_json

from .. import dataclasses_json_extensions as json_mixins, error

from ..language_server import daemon_connection
from . import incremental
from .query_response import InvalidQueryResponse, Response


QueryResponseType = TypeVar(
    "QueryResponseType", bound=json_mixins.CamlCaseAndExcludeJsonMixin
)

LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class DaemonQueryFailure(json_mixins.CamlCaseAndExcludeJsonMixin):
    error_message: str


def execute_query(socket_path: Path, query_text: str) -> Response:
    raw_request = json.dumps(["Query", query_text])
    raw_response = daemon_connection.send_raw_request(socket_path, raw_request)
    return Response.parse(raw_response)


async def attempt_async_query(
    socket_path: Path,
    query_text: str,
    overlay_id: Optional[str] = None,
) -> Union[Response, DaemonQueryFailure]:
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
    if isinstance(response_text, daemon_connection.DaemonConnectionFailure):
        return DaemonQueryFailure(
            f"In attempt async query with response_text, got DaemonConnectionFailure exception: ({response_text.error_message})"
        )
    try:
        return Response.parse(response_text)
    except InvalidQueryResponse as exception:
        return DaemonQueryFailure(
            f"In attempt async query with response_text, got InvalidQueryResponse exception: ({exception})"
        )


async def attempt_typed_async_query(
    response_type: Type[QueryResponseType],
    socket_path: Path,
    query_text: str,
    overlay_id: Optional[str] = None,
) -> Union[QueryResponseType | DaemonQueryFailure]:
    try:
        response = await attempt_async_query(
            socket_path,
            query_text,
            overlay_id,
        )
        if isinstance(response, DaemonQueryFailure):
            return response
        else:
            if not isinstance(response.payload, dict):
                raise ValueError(
                    f"Expected a doct, got {response.payload!r} as response"
                )
            if "error" in response.payload:
                return DaemonQueryFailure(
                    f"Daemon query returned error: {response.payload} for query: {query_text}"
                )
            return response_type.from_dict(response.payload)
    except (
        KeyError,
        ValueError,
        dataclasses_json.mm.ValidationError,
    ) as exception:
        return DaemonQueryFailure(
            f"When interpretting response type: {response_type.__name__} got: {type(exception).__name__}({exception})"
        )


async def attempt_async_overlay_type_errors(
    socket_path: Path,
    source_code_path: Path,
    overlay_id: str,
) -> Union[List[error.Error], DaemonQueryFailure]:
    """
    In order to type check unsaved changes, we need to get type errors for one
    specific module in an overlay.

    We cannot use `attempt_async_query` for this because the output types are
    not quite compatible, but otherwise the logic works the same.
    """
    response_text = await daemon_connection.attempt_send_async_raw_request(
        socket_path=socket_path,
        request=json.dumps(
            [
                "GetOverlayTypeErrors",
                {"path": str(source_code_path), "overlay_id": overlay_id},
            ]
        ),
    )
    if isinstance(response_text, daemon_connection.DaemonConnectionFailure):
        return DaemonQueryFailure(
            f"In attempt async query with response_text, got DaemonConnectionFailure exception: ({response_text.error_message})"
        )
    try:
        return incremental.parse_type_error_response(response_text)
    except incremental.InvalidServerResponse as exception:
        return DaemonQueryFailure(
            f"In attempt async query with response_text, parsing led to uncaught error: ({exception})"
        )
