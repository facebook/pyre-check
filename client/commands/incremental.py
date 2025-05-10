# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module provides the logic for the `pyre incremental` command,
which is what is also run when `pyre` is invoked with no subcommand.

What `pyre incremental` does is try to connect to a running daemon
and start one if none is found, then ask for type errors and display
them in the same format as `pyre check` would.
"""

import dataclasses
import enum
import json
import logging
from pathlib import Path
from typing import List, Optional

from .. import (
    backend_arguments,
    command_arguments,
    daemon_socket,
    error,
    frontend_configuration,
    identifiers,
)
from ..language_server import connections
from . import commands, server_event, start

LOG: logging.Logger = logging.getLogger(__name__)

COMMAND_NAME = "incremental"


@dataclasses.dataclass(frozen=True)
class TypeErrors:
    errors: List[error.Error] = dataclasses.field(default_factory=list)
    build_failure: Optional[str] = None


class InvalidServerResponse(Exception):
    pass


class ServerStatus(str, enum.Enum):
    _value_: str
    NEWLY_STARTED = "newly_started_server"
    ALREADY_RUNNING = "already_running_server"

    def __str__(self) -> str:
        return self.value


@dataclasses.dataclass(frozen=True)
class ExitStatus:
    exit_code: commands.ExitCode
    connected_to: ServerStatus


def parse_type_error_response_json(response_json: object) -> TypeErrors:
    try:
        # The response JSON is expected to have one of the following form:
        # `["TypeErrors", [error_json0, error_json1, ...]]` (legacy form)
        # `["TypeErrors", {"errors": [error_json0, ...], "build_failure": "..."}]`
        if (
            isinstance(response_json, list)
            and len(response_json) > 1
            and response_json[0] == "TypeErrors"
        ):
            errors_json = response_json[1]
            if isinstance(errors_json, list):
                return TypeErrors(
                    errors=[
                        error.Error.from_json(error_json) for error_json in errors_json
                    ],
                    build_failure=None,
                )
            elif isinstance(errors_json, dict):
                error_list = errors_json.get("errors", [])
                build_failure = errors_json.get("build_failure", None)
                if isinstance(error_list, list) and (
                    build_failure is None or isinstance(build_failure, str)
                ):
                    return TypeErrors(
                        errors=[
                            error.Error.from_json(error_json)
                            for error_json in error_list
                        ],
                        build_failure=build_failure,
                    )

        raise InvalidServerResponse(
            f"Unexpected JSON response from server: {response_json}"
        )
    except error.ErrorParsingFailure as parsing_error:
        message = f"Unexpected error JSON from server: {parsing_error}"
        raise InvalidServerResponse(message) from parsing_error


def parse_type_error_response(response: str) -> TypeErrors:
    try:
        response_json = json.loads(response)
        return parse_type_error_response_json(response_json)
    except json.JSONDecodeError as decode_error:
        message = f"Cannot parse response as JSON: {decode_error}"
        raise InvalidServerResponse(message) from decode_error


def _read_type_errors(socket_path: Path) -> TypeErrors:
    with connections.connect(socket_path) as (
        input_channel,
        output_channel,
    ):
        # The empty list argument means we want all type errors from the server.
        output_channel.write('["DisplayTypeError", []]\n')
        return parse_type_error_response(input_channel.readline())


def display_type_errors(errors: List[error.Error], output: str) -> None:
    error.print_errors(
        [error.relativize_path(against=Path.cwd()) for error in errors],
        output=output,
    )


def _show_progress_log_and_display_type_errors(
    log_path: Path,
    socket_path: Path,
    output: str,
    remote_logging: Optional[backend_arguments.RemoteLogging],
) -> commands.ExitCode:
    LOG.info("Waiting for server...")
    with start.background_logging(log_path):
        type_errors = _read_type_errors(socket_path)
        display_type_errors(type_errors.errors, output=output)
        if type_errors.build_failure is not None:
            LOG.warning("You may be seeing stale type checking results. Reason:")
            LOG.warning(type_errors.build_failure)
        return (
            commands.ExitCode.SUCCESS
            if len(type_errors.errors) == 0
            else commands.ExitCode.FOUND_ERRORS
        )


def run_incremental(
    configuration: frontend_configuration.Base,
    incremental_arguments: command_arguments.IncrementalArguments,
) -> ExitStatus:
    flavor = identifiers.PyreFlavor.CLASSIC
    socket_path = daemon_socket.get_socket_path(
        configuration.get_project_identifier(),
        flavor=flavor,
    )
    # Need to be consistent with the log symlink location in start command
    log_path = (
        configuration.get_log_directory()
        / flavor.server_log_subdirectory()
        / "server.stderr"
    )
    output = incremental_arguments.output
    remote_logging = backend_arguments.RemoteLogging.create(
        configuration.get_remote_logger(),
        incremental_arguments.start_arguments.get_log_identifier(),
    )
    try:
        exit_code = _show_progress_log_and_display_type_errors(
            log_path, socket_path, output, remote_logging
        )
        return ExitStatus(
            exit_code=exit_code, connected_to=ServerStatus.ALREADY_RUNNING
        )
    except connections.ConnectionFailure:
        pass

    if incremental_arguments.no_start:
        raise commands.ClientException("Cannot find a running Pyre server.")

    LOG.info("Cannot find a running Pyre server. Starting a new one...")
    start_status = start.run(configuration, incremental_arguments.start_arguments)
    if start_status != commands.ExitCode.SUCCESS:
        raise commands.ClientException(
            f"`pyre start` failed with non-zero exit code: {start_status}"
        )
    exit_code = _show_progress_log_and_display_type_errors(
        log_path, socket_path, output, remote_logging
    )
    return ExitStatus(exit_code=exit_code, connected_to=ServerStatus.NEWLY_STARTED)


def run(
    configuration: frontend_configuration.Base,
    incremental_arguments: command_arguments.IncrementalArguments,
) -> ExitStatus:
    try:
        return run_incremental(configuration, incremental_arguments)
    except server_event.ServerStartException as error:
        raise commands.ClientException(
            f"{error}", exit_code=error.kind.to_exit_code()
        ) from error
