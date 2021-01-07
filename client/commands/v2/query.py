# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import json
import logging
from pathlib import Path

from ... import commands, configuration as configuration_module
from . import server_connection


LOG: logging.Logger = logging.getLogger(__name__)


class InvalidQueryResponse(Exception):
    pass


@dataclasses.dataclass(frozen=True)
class Response:
    payload: object


def parse_query_response_json(response_json: object) -> Response:
    if (
        isinstance(response_json, list)
        and len(response_json) > 1
        and response_json[0] == "Query"
    ):
        return Response(response_json[1])
    raise InvalidQueryResponse(f"Unexpected JSON response from server: {response_json}")


def parse_query_response(text: str) -> Response:
    try:
        response_json = json.loads(text)
        return parse_query_response_json(response_json)
    except json.JSONDecodeError as decode_error:
        message = f"Cannot parse response as JSON: {decode_error}"
        raise InvalidQueryResponse(message) from decode_error


def run(
    configuration: configuration_module.Configuration, query: str
) -> commands.ExitCode:
    socket_path = server_connection.get_default_socket_path(
        log_directory=Path(configuration.log_directory)
    )
    try:
        LOG.warning("Not implemented yet!")
        return commands.ExitCode.SUCCESS
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during pyre query: {error}"
        ) from error
