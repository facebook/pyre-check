# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
Core logic for the `pyre stop` command, which attempts to do a clean
shutdown of a running Pyre daemon by sending it a shutdown request
via a socket connection.
"""


import logging
from pathlib import Path

from .. import daemon_socket, frontend_configuration, identifiers
from ..language_server import connections
from . import commands


LOG: logging.Logger = logging.getLogger(__name__)


def stop_message(flavor: identifiers.PyreFlavor) -> str:
    if flavor == identifiers.PyreFlavor.CODE_NAVIGATION:
        return '["Command", ["Stop"]]'
    else:
        if flavor not in (
            identifiers.PyreFlavor.CLASSIC,
            identifiers.PyreFlavor.SHADOW,
        ):
            raise AssertionError(
                f"Attempted to stop a server for unsupported flavor {flavor}"
            )
        return '["Stop"]'


def stop_server(socket_path: Path, flavor: identifiers.PyreFlavor) -> None:
    with connections.connect(socket_path) as (
        input_channel,
        output_channel,
    ):
        output_channel.write(f"{stop_message(flavor)}\n")
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


def run(
    configuration: frontend_configuration.Base, flavor: identifiers.PyreFlavor
) -> commands.ExitCode:
    socket_path = daemon_socket.get_socket_path(
        configuration.get_project_identifier(),
        flavor,
    )
    try:
        LOG.info(f"Stopping {flavor.value} server...")
        LOG.info(f"Socket is {socket_path}")
        stop_server(socket_path, flavor)
        LOG.info(
            f"Stopped {flavor.value} server at `{configuration.get_project_identifier()}`\n"
        )
        return commands.ExitCode.SUCCESS
    except connections.ConnectionFailure:
        LOG.info("No running Pyre server to stop.\n")
        remove_socket_if_exists(socket_path)
        return commands.ExitCode.SERVER_NOT_FOUND
