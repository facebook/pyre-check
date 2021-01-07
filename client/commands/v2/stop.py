# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
from pathlib import Path

from ... import commands, configuration as configuration_module
from . import server_connection, start


LOG: logging.Logger = logging.getLogger(__name__)


def stop_server(socket_path: Path) -> None:
    with server_connection.connect_in_text_mode(socket_path) as (
        input_channel,
        output_channel,
    ):
        output_channel.write('["Stop"]\n')
        # Wait for the server to shutdown on its side
        input_channel.read()


def run(configuration: configuration_module.Configuration) -> commands.ExitCode:
    socket_path = server_connection.get_default_socket_path(
        log_directory=Path(configuration.log_directory)
    )
    try:
        LOG.info("Stopping the server...")
        stop_server(socket_path)

        LOG.info(f"Stopped server at `{start.get_server_identifier(configuration)}`\n")
        return commands.ExitCode.SUCCESS
    except server_connection.ConnectionFailure:
        LOG.warning("No running Pyre server to stop.")
        return commands.ExitCode.SERVER_NOT_FOUND
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during server stop: {error}"
        ) from error
