# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import json
import logging
import os
from pathlib import Path
from typing import List

from ... import (
    command_arguments,
    commands,
    configuration as configuration_module,
    error,
)
from . import server_connection, start, remote_logging


LOG: logging.Logger = logging.getLogger(__name__)


class InvalidServerResponse(Exception):
    pass


def parse_type_error_response_json(response_json: object) -> List[error.Error]:
    try:
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
    except error.ErrorParsingFailure as parsing_error:
        message = f"Unexpected error JSON from server: {parsing_error}"
        raise InvalidServerResponse(message) from parsing_error


def parse_type_error_response(response: str) -> List[error.Error]:
    try:
        response_json = json.loads(response)
        return parse_type_error_response_json(response_json)
    except json.JSONDecodeError as decode_error:
        message = f"Cannot parse response as JSON: {decode_error}"
        raise InvalidServerResponse(message) from decode_error


def _relativize_error_path(error: error.Error) -> error.Error:
    relativized_path = os.path.relpath(str(error.path), str(Path.cwd()))
    return dataclasses.replace(error, path=relativized_path)


def _display_type_errors(socket_path: Path, output: str) -> None:
    with server_connection.connect_in_text_mode(socket_path) as (
        input_channel,
        output_channel,
    ):
        # The empty list argument means we want all type errors from the server.
        output_channel.write('["DisplayTypeError", []]\n')
        type_errors = parse_type_error_response(input_channel.readline())
        error.print_errors(
            [_relativize_error_path(error) for error in type_errors], output=output
        )


def _show_progress_and_display_type_errors(
    log_path: Path, socket_path: Path, output: str
) -> None:
    LOG.info("Waiting for server...")
    with start.background_logging(log_path):
        _display_type_errors(socket_path, output)


def run_incremental(
    configuration: configuration_module.Configuration,
    incremental_arguments: command_arguments.IncrementalArguments,
) -> remote_logging.ExitCodeWithAdditionalLogging:
    socket_path = server_connection.get_default_socket_path(
        log_directory=Path(configuration.log_directory)
    )
    # Need to be consistent with the log symlink location in start command
    log_path = Path(configuration.log_directory) / "new_server" / "server.stderr"
    output = incremental_arguments.output
    try:
        _show_progress_and_display_type_errors(log_path, socket_path, output)
        return remote_logging.ExitCodeWithAdditionalLogging(
            exit_code=commands.ExitCode.SUCCESS,
            additional_logging={
                "connected_to": "already_running_server",
            },
        )
    except server_connection.ConnectionFailure:
        if incremental_arguments.no_start:
            raise commands.ClientException("Cannot find a running Pyre server.")

        LOG.info("Cannot find a running Pyre server. Starting a new one...")
        start_status = start.run_start(
            configuration, incremental_arguments.start_arguments
        )
        if start_status != commands.ExitCode.SUCCESS:
            raise commands.ClientException(
                f"`pyre start` failed with non-zero exit code: {start_status}"
            )
        _show_progress_and_display_type_errors(log_path, socket_path, output)
        return remote_logging.ExitCodeWithAdditionalLogging(
            exit_code=commands.ExitCode.SUCCESS,
            additional_logging={
                "connected_to": "newly_started_server",
            },
        )


@remote_logging.log_usage_with_additional_info(command_name="incremental")
def run(
    configuration: configuration_module.Configuration,
    incremental_arguments: command_arguments.IncrementalArguments,
) -> remote_logging.ExitCodeWithAdditionalLogging:
    try:
        return run_incremental(configuration, incremental_arguments)
    except Exception as error:
        raise commands.ClientException(f"{error}") from error
