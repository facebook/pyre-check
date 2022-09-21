# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import dataclasses
import enum
import json
import logging
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence

from .. import (
    command_arguments,
    configuration as configuration_module,
    error,
    identifiers,
    statistics_logger,
)
from . import (
    backend_arguments,
    commands,
    connections,
    daemon_socket,
    frontend_configuration,
    server_event,
    start,
)


LOG: logging.Logger = logging.getLogger(__name__)

COMMAND_NAME = "incremental"


class InvalidServerResponse(Exception):
    pass


class ServerStatus(str, enum.Enum):
    NEWLY_STARTED: str = "newly_started_server"
    ALREADY_RUNNING: str = "already_running_server"

    def __str__(self) -> str:
        return self.value


@dataclasses.dataclass(frozen=True)
class ExitStatus:
    exit_code: commands.ExitCode
    connected_to: ServerStatus


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


def _read_type_errors(socket_path: Path) -> List[error.Error]:
    with connections.connect(socket_path) as (
        input_channel,
        output_channel,
    ):
        # The empty list argument means we want all type errors from the server.
        output_channel.write('["DisplayTypeError", []]\n')
        return parse_type_error_response(input_channel.readline())


def compute_error_statistics_per_code(
    type_errors: Sequence[error.Error],
) -> Iterable[Dict[str, int]]:
    errors_grouped_by_code: Dict[int, List[error.Error]] = {}
    for type_error in type_errors:
        errors_grouped_by_code.setdefault(type_error.code, []).append(type_error)

    for code, errors in errors_grouped_by_code.items():
        yield {"code": code, "count": len(errors)}


def log_error_statistics(
    remote_logging: Optional[backend_arguments.RemoteLogging],
    type_errors: Sequence[error.Error],
    command_name: str,
) -> None:
    if remote_logging is None:
        return
    logger = remote_logging.logger
    if logger is None:
        return
    log_identifier = remote_logging.identifier
    for integers in compute_error_statistics_per_code(type_errors):
        statistics_logger.log(
            category=statistics_logger.LoggerCategory.ERROR_STATISTICS,
            logger=logger,
            integers=integers,
            normals={
                "command": command_name,
                **(
                    {"identifier": log_identifier} if log_identifier is not None else {}
                ),
            },
        )


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
        log_error_statistics(
            remote_logging=remote_logging,
            type_errors=type_errors,
            command_name=COMMAND_NAME,
        )
        display_type_errors(type_errors, output=output)
        return (
            commands.ExitCode.SUCCESS
            if len(type_errors) == 0
            else commands.ExitCode.FOUND_ERRORS
        )


def run_incremental(
    configuration: frontend_configuration.Base,
    incremental_arguments: command_arguments.IncrementalArguments,
) -> ExitStatus:
    socket_path = daemon_socket.get_socket_path(
        configuration.get_project_identifier(),
        flavor=identifiers.PyreFlavor.CLASSIC,
    )
    # Need to be consistent with the log symlink location in start command
    log_path = configuration.get_log_directory() / "new_server" / "server.stderr"
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
    start_status = start.run_start(configuration, incremental_arguments.start_arguments)
    if start_status != commands.ExitCode.SUCCESS:
        raise commands.ClientException(
            f"`pyre start` failed with non-zero exit code: {start_status}"
        )
    exit_code = _show_progress_log_and_display_type_errors(
        log_path, socket_path, output, remote_logging
    )
    return ExitStatus(exit_code=exit_code, connected_to=ServerStatus.NEWLY_STARTED)


def _exit_code_from_error_kind(error_kind: server_event.ErrorKind) -> commands.ExitCode:
    if error_kind == server_event.ErrorKind.WATCHMAN:
        return commands.ExitCode.WATCHMAN_ERROR
    elif error_kind == server_event.ErrorKind.BUCK_INTERNAL:
        return commands.ExitCode.BUCK_INTERNAL_ERROR
    elif error_kind == server_event.ErrorKind.BUCK_USER:
        return commands.ExitCode.BUCK_USER_ERROR
    return commands.ExitCode.FAILURE


def run(
    configuration: configuration_module.Configuration,
    incremental_arguments: command_arguments.IncrementalArguments,
) -> ExitStatus:
    try:
        return run_incremental(
            frontend_configuration.OpenSource(configuration), incremental_arguments
        )
    except server_event.ServerStartException as error:
        raise commands.ClientException(
            f"{error}", exit_code=_exit_code_from_error_kind(error.kind)
        )
