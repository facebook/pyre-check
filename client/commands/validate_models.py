# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
Entrypoint for the `pyre validate-models` command.

This command will sanity check that Pysa taint models make sense for
the given project.
"""


import dataclasses
import logging
import os
from pathlib import Path
from typing import Dict, List

from .. import (
    configuration as configuration_module,
    daemon_socket,
    error as error_module,
    identifiers,
)
from ..language_server import connections
from . import commands, daemon_query, frontend_configuration


LOG: logging.Logger = logging.getLogger(__name__)


def _relativize_error_path(
    error: error_module.ModelVerificationError,
) -> error_module.ModelVerificationError:
    if error.path is None:
        return error

    relativized_path = Path(os.path.relpath(error.path, Path.cwd()))
    return dataclasses.replace(error, path=relativized_path)


def parse_validation_errors(
    payload: Dict[str, object],
) -> List[error_module.ModelVerificationError]:
    errors_payload = [] if "errors" not in payload else payload["errors"]
    if not isinstance(errors_payload, list):
        message = f"Invalid error payload for model validation: `{errors_payload}`."
        raise daemon_query.InvalidQueryResponse(message)

    return sorted(
        (
            _relativize_error_path(error_module.ModelVerificationError.from_json(item))
            for item in errors_payload
        ),
        key=lambda error: (error.path or Path(), error.line, error.code),
    )


def parse_validation_errors_response(
    payload: object,
) -> List[error_module.ModelVerificationError]:
    if not isinstance(payload, dict) or "response" not in payload:
        message = f"Invalid payload for model validation: `{payload}`."
        raise daemon_query.InvalidQueryResponse(message)

    response_payload = payload["response"]
    if not isinstance(response_payload, dict):
        message = (
            f"Invalid response payload for model validation: `{response_payload}`."
        )
        raise daemon_query.InvalidQueryResponse(message)

    return parse_validation_errors(response_payload)


def run_validate_models(
    configuration: frontend_configuration.Base, output: str
) -> commands.ExitCode:
    socket_path = daemon_socket.get_socket_path(
        configuration.get_project_identifier(),
        flavor=identifiers.PyreFlavor.CLASSIC,
    )
    try:
        response = daemon_query.execute_query(socket_path, "validate_taint_models()")
        validation_errors = parse_validation_errors_response(response.payload)
        error_module.print_errors(
            validation_errors, output=output, error_kind="model verification"
        )
        return commands.ExitCode.SUCCESS
    except connections.ConnectionFailure:
        LOG.warning(
            "A running Pyre server is required for models to be validated. "
            "Please run `pyre` first to set up a server."
        )
        return commands.ExitCode.SERVER_NOT_FOUND


def run(
    configuration: configuration_module.Configuration, output: str
) -> commands.ExitCode:
    return run_validate_models(
        frontend_configuration.OpenSource(configuration),
        output,
    )
