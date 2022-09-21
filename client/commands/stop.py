# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import logging
from pathlib import Path

from .. import configuration as configuration_module, identifiers
from . import commands, connections, daemon_socket, frontend_configuration


LOG: logging.Logger = logging.getLogger(__name__)


def stop_server(socket_path: Path) -> None:
    with connections.connect(socket_path) as (
        input_channel,
        output_channel,
    ):
        output_channel.write('["Stop"]\n')
        # Wait for the server to shutdown on its side
        input_channel.read()


def remove_socket_if_exists(socket_path: Path) -> None:
    try:
        socket_path.unlink()
    except FileNotFoundError:
        pass
    except OSError as error:
        LOG.warning(f"Failed to remove socket file at `{socket_path}`: {error}")
    try:
        socket_path.with_suffix(socket_path.suffix + ".lock").unlink()
    except FileNotFoundError:
        pass
    except OSError as error:
        LOG.warning(f"Failed to remove lock file at `{socket_path}.lock`: {error}")


def run_stop(configuration: frontend_configuration.Base) -> commands.ExitCode:
    socket_path = daemon_socket.get_socket_path(
        configuration.get_project_identifier(),
        flavor=identifiers.PyreFlavor.CLASSIC,
    )
    try:
        LOG.info("Stopping server...")
        stop_server(socket_path)
        LOG.info(f"Stopped server at `{configuration.get_project_identifier()}`\n")
        return commands.ExitCode.SUCCESS
    except connections.ConnectionFailure:
        LOG.info("No running Pyre server to stop.\n")
        remove_socket_if_exists(socket_path)
        return commands.ExitCode.SERVER_NOT_FOUND


def run(configuration: configuration_module.Configuration) -> commands.ExitCode:
    return run_stop(frontend_configuration.OpenSource(configuration))
