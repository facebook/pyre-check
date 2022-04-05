# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import logging
import os
from pathlib import Path
from typing import Dict, List

from .. import configuration as configuration_module, error as error_module
from . import commands, query, remote_logging, server_connection


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
        raise query.InvalidQueryResponse(message)

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
        raise query.InvalidQueryResponse(message)

    response_payload = payload["response"]
    if not isinstance(response_payload, dict):
        message = (
            f"Invalid response payload for model validation: `{response_payload}`."
        )
        raise query.InvalidQueryResponse(message)

    return parse_validation_errors(response_payload)


@remote_logging.log_usage(command_name="validate-models")
def run(
    configuration: configuration_module.Configuration, output: str
) -> commands.ExitCode:
    socket_path = server_connection.get_default_socket_path(
        project_root=Path(configuration.project_root),
        relative_local_root=Path(configuration.relative_local_root)
        if configuration.relative_local_root
        else None,
    )
    try:
        response = query.query_server(socket_path, "validate_taint_models()")
        validation_errors = parse_validation_errors_response(response.payload)
        error_module.print_errors(
            validation_errors, output=output, error_kind="model verification"
        )
        return commands.ExitCode.SUCCESS
    except server_connection.ConnectionFailure:
        LOG.warning(
            "A running Pyre server is required for models to be validated. "
            "Please run `pyre` first to set up a server."
        )
        return commands.ExitCode.SERVER_NOT_FOUND
    except Exception as error:
        raise commands.ClientException(
            f"Exception occurred during model validation: {error}"
        ) from error
