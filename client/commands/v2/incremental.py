# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
from pathlib import Path
from typing import List

from ... import (
    command_arguments,
    commands,
    configuration as configuration_module,
    error,
)
from . import server_connection, start


LOG: logging.Logger = logging.getLogger(__name__)


class InvalidServerResponse(Exception):
    pass


def parse_type_error_response(response: str) -> List[error.Error]:
    try:
        response_json = json.loads(response)
        # The response JSON is expected to have the following form:
        # `["TypeErrors", [error_json0, error_json1, ...]]`
        if (
            isinstance(response_json, list)
            and len(response_json) > 1
            and response_json[0] == "TypeErrors"
        ):
            errors_json = response_json[1]
            if isinstance(errors_json, list):
                return [error.Error.from_json(error_json) for error_json in errors_json]

        raise InvalidServerResponse(
            f"Unexpected JSON response from server: {response_json}"
        )
    except json.JSONDecodeError as decode_error:
        message = f"Cannot parse response as JSON: {decode_error}"
        raise InvalidServerResponse(message) from decode_error
    except error.ErrorParsingFailure as parsing_error:
        message = f"Unexpected error JSON from server: {parsing_error}"
        raise InvalidServerResponse(message) from parsing_error


def _display_type_errors(socket_path: Path) -> None:
    with server_connection.connect_in_text_mode(socket_path) as (
        input_channel,
        output_channel,
    ):
        # The empty list argument means we want all type errors from the server.
        output_channel.write('["DisplayTypeError", []]\n')
        type_errors = parse_type_error_response(input_channel.readline())
        error.print_errors(type_errors, output="text")


def _show_progress_and_display_type_errors(log_path: Path, socket_path: Path) -> None:
    LOG.info("Waiting for server...")
    with start.background_logging(log_path):
        _display_type_errors(socket_path)


def run_incremental(
    configuration: configuration_module.Configuration,
    incremental_arguments: command_arguments.IncrementalArguments,
) -> None:
    socket_path = server_connection.get_default_socket_path(
        log_directory=Path(configuration.log_directory)
    )
    # Need to be consistent with the log symlink location in start command
    log_path = Path(configuration.log_directory) / "new_server" / "server.stderr"
    try:
        _show_progress_and_display_type_errors(log_path, socket_path)
    except OSError:
        if incremental_arguments.no_start:
            raise commands.ClientException("Cannot find a running Pyre server.")

        LOG.info("Cannot find a running Pyre server. Starting a new one...")
        start_status = start.run(configuration, incremental_arguments.start_arguments)
        if start_status != commands.ExitCode.SUCCESS:
            raise commands.ClientException(
                f"`pyre start` failed with non-zero exit code: {start_status}"
            )
        _show_progress_and_display_type_errors(log_path, socket_path)


def run(
    configuration: configuration_module.Configuration,
    incremental_arguments: command_arguments.IncrementalArguments,
) -> commands.ExitCode:
    try:
        run_incremental(configuration, incremental_arguments)
        return commands.ExitCode.SUCCESS
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during incremental query: {error}"
        ) from error
