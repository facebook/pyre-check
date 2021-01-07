# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
from pathlib import Path

from ... import command_arguments, commands, configuration as configuration_module
from . import incremental, server_connection, start, stop


LOG: logging.Logger = logging.getLogger(__name__)


def _stop_server_if_needed(configuration: configuration_module.Configuration) -> None:
    try:
        socket_path = server_connection.get_default_socket_path(
            log_directory=Path(configuration.log_directory)
        )
        LOG.info("Stopping the server if needed...")
        stop.stop_server(socket_path)
        LOG.info(f"Stopped server at `{start.get_server_identifier(configuration)}`")
    except server_connection.ConnectionFailure:
        # This usually means there's no server running
        pass


def run(
    configuration: configuration_module.Configuration,
    incremental_arguments: command_arguments.IncrementalArguments,
) -> commands.ExitCode:
    try:
        _stop_server_if_needed(configuration)
        incremental.run_incremental(configuration, incremental_arguments)
        return commands.ExitCode.SUCCESS
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during pyre restart: {error}"
        ) from error
