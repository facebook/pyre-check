# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import logging
import os
from pathlib import Path
from typing import List

from ... import commands, configuration as configuration_module, error as error_module
from . import query, server_connection, remote_logging


LOG: logging.Logger = logging.getLogger(__name__)


def _relativize_error_path(
    error: error_module.ModelVerificationError,
) -> error_module.ModelVerificationError:
    if error.path is None:
        return error

    relativized_path = Path(os.path.relpath(error.path, Path.cwd()))
    return dataclasses.replace(error, path=relativized_path)


def parse_validation_errors(
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

    errors_payload = (
        [] if "errors" not in response_payload else response_payload["errors"]
    )
    if not isinstance(errors_payload, list):
        message = f"Invalid error payload for model validation: `{errors_payload}`."
        raise query.InvalidQueryResponse(message)

    return sorted(
        (
            _relativize_error_path(error_module.ModelVerificationError.from_json(item))
            for item in errors_payload
        ),
        key=lambda error: (error.path, error.line, error.code),
    )


@remote_logging.log_usage(command_name="validate-models")
def run(
    configuration: configuration_module.Configuration, output: str
) -> commands.ExitCode:
    socket_path = server_connection.get_default_socket_path(
        log_directory=Path(configuration.log_directory)
    )
    try:
        response = query.query_server(socket_path, "validate_taint_models()")
        validation_errors = parse_validation_errors(response.payload)
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
            f"Exception occured during model validation: {error}"
        ) from error
