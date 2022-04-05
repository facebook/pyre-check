# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import json
import logging
from pathlib import Path
from typing import TextIO

from .. import configuration as configuration_module, log
from . import commands, remote_logging, server_connection


LOG: logging.Logger = logging.getLogger(__name__)


HELP_MESSAGE: str = """
Possible queries:
  - attributes(class_name)
    Returns a list of attributes, including functions, for a class.
  - batch(query1(arg), query2(arg))
    Runs a batch of queries and returns a map of responses. List of given queries
    may include any combination of other valid queries except for `batch` itself.
  - callees(function)
    Calls from a given function.
  - callees_with_location(function)
    Calls from a given function, including the locations at which they are called.
  - defines(module_or_class_name)
    Returns a JSON with the signature of all defines for given module or class.
  - dump_call_graph()
    Returns a comprehensive JSON of caller -> list of callees.
  - inline_decorators(qualified_function_name, decorators_to_skip=[decorator1, ...])
    Returns the function definition after inlining decorators.
    Allows skipping certain decorators when inlining.
  - less_or_equal(T1, T2)
    Returns whether T1 is a subtype of T2.
  - path_of_module(module)
    Gives an absolute path for `module`.
  - save_server_state('path')
    Saves Pyre's serialized state into `path`.
  - superclasses(class_name1, class_name2, ...)
    Returns a mapping of class_name to the list of superclasses for `class_name`.
    If no class name is provided, return the mapping for all classes Pyre knows about.
  - type(expression)
    Evaluates the type of `expression`.
  - types(path='path') or types('path1', 'path2', ...)
    Returns a map from each given path to a list of all types for that path.
  - validate_taint_models('optional path')
    Validates models and returns errors.
    Defaults to model path in configuration if no parameter is passed in.
"""


class InvalidQueryResponse(Exception):
    pass


@dataclasses.dataclass(frozen=True)
class Response:
    payload: object


def _print_help_message() -> None:
    log.stdout.write(HELP_MESSAGE)


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
    LOG.debug(f"Sending `{log.truncate(query_message, 400)}`")
    output_channel.write(f"{query_message}\n")


def _receive_query_response(input_channel: TextIO) -> Response:
    query_message = input_channel.readline().strip()
    LOG.debug(f"Received `{log.truncate(query_message, 400)}`")
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
        project_root=Path(configuration.project_root),
        relative_local_root=Path(configuration.relative_local_root)
        if configuration.relative_local_root
        else None,
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
            f"Exception occurred during pyre query: {error}"
        ) from error
