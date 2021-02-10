# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import json
import logging
from pathlib import Path
from typing import TextIO

from ... import commands, configuration as configuration_module, log
from . import server_connection, remote_logging


LOG: logging.Logger = logging.getLogger(__name__)


class InvalidQueryResponse(Exception):
    pass


@dataclasses.dataclass(frozen=True)
class Response:
    payload: object


def _print_help_message() -> None:
    log.stdout.write(commands.query.HELP_MESSAGE)


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


def _send_query_request(output_channel: TextIO, query_text: str) -> None:
    query_message = json.dumps(["Query", query_text])
    LOG.debug(f"Sending `{query_message}`")
    output_channel.write(f"{query_message}\n")


def _receive_query_response(input_channel: TextIO) -> Response:
    query_message = input_channel.readline().strip()
    LOG.debug(f"Received `{query_message}`")
    return parse_query_response(query_message)


def query_server(socket_path: Path, query_text: str) -> Response:
    with server_connection.connect_in_text_mode(socket_path) as (
        input_channel,
        output_channel,
    ):
        _send_query_request(output_channel, query_text)
        return _receive_query_response(input_channel)


@remote_logging.log_usage(command_name="query")
def run(
    configuration: configuration_module.Configuration, query_text: str
) -> commands.ExitCode:
    socket_path = server_connection.get_default_socket_path(
        log_directory=Path(configuration.log_directory)
    )
    try:
        if query_text == "help":
            _print_help_message()
            return commands.ExitCode.SUCCESS

        response = query_server(socket_path, query_text)
        log.stdout.write(json.dumps(response.payload))
        return commands.ExitCode.SUCCESS
    except server_connection.ConnectionFailure:
        LOG.warning(
            "A running Pyre server is required for queries to be responded. "
            "Please run `pyre` first to set up a server."
        )
        return commands.ExitCode.SERVER_NOT_FOUND
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during pyre query: {error}"
        ) from error
