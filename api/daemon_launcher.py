# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module provides a Python API for starting and stopping Pyre daemons
(using the new code navigation backend).
"""

import abc
import logging
import os
from dataclasses import dataclass
from pathlib import Path
from typing import Union

from pyre_extensions import override

from ..client import (
    command_arguments,
    daemon_socket,
    frontend_configuration,
    identifiers,
)
from ..client.commands import initialization, start, stop
from ..client.language_server import (
    code_navigation_request,
    connections,
    daemon_connection,
)

FLAVOR: identifiers.PyreFlavor = identifiers.PyreFlavor.CODE_NAVIGATION
LOG: logging.Logger = logging.getLogger(__name__)


@dataclass
class StartedServerInfo:
    socket_path: Path
    client_id: str


@dataclass
class StartFailure:
    message: str


@dataclass(frozen=True)
class RegitrationSuccess:
    client_id: str


@dataclass(frozen=True)
class RegistrationFailure:
    message: str


class PyreServerStarterBase(abc.ABC):
    @abc.abstractmethod
    async def run(
        self,
        binary_location: str,
        configuration: frontend_configuration.Base,
        flavor: identifiers.PyreFlavor,
    ) -> Union[
        initialization.StartSuccess,
        initialization.BuckStartFailure,
        initialization.OtherStartFailure,
    ]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def register_client(
        self,
        socket_path: Path,
    ) -> Union[RegitrationSuccess, RegistrationFailure]:
        raise NotImplementedError()


class PyreServerStarter(PyreServerStarterBase):
    @override
    async def run(
        self,
        binary_location: str,
        configuration: frontend_configuration.Base,
        flavor: identifiers.PyreFlavor,
    ) -> Union[
        initialization.StartSuccess,
        initialization.BuckStartFailure,
        initialization.OtherStartFailure,
    ]:
        command_argument = command_arguments.CommandArguments(
            binary=str(configuration.get_binary_location())
        )
        start_arguments = command_arguments.StartArguments.create(
            command_argument=command_argument,
            flavor=FLAVOR,
            skip_initial_type_check=True,
            use_lazy_module_tracking=True,
        )
        pyre_arguments = start.create_server_arguments(
            configuration,
            start_arguments,
        )
        return await initialization.async_start_pyre_server(
            binary_location,
            pyre_arguments,
            flavor,
        )

    @override
    async def register_client(
        self,
        socket_path: Path,
    ) -> Union[RegitrationSuccess, RegistrationFailure]:
        client_id = _get_client_id()
        request = code_navigation_request.RegisterClient(client_id)
        response = await code_navigation_request.async_handle_register_client(
            socket_path, request
        )
        if isinstance(response, daemon_connection.DaemonConnectionFailure):
            return RegistrationFailure(response.error_message)
        return RegitrationSuccess(client_id)


def _get_client_id() -> str:
    return f"pyre_api_{os.getpid()}"


async def _start_server(
    configuration: frontend_configuration.Base,
    server_starter: PyreServerStarterBase,
) -> Union[StartedServerInfo, StartFailure]:
    socket_path = daemon_socket.get_socket_path(
        configuration.get_project_identifier(),
        flavor=identifiers.PyreFlavor.CODE_NAVIGATION,
    )
    registration = await server_starter.register_client(
        socket_path,
    )
    if isinstance(registration, RegitrationSuccess):
        LOG.info("Pyre server already exists.")
        return StartedServerInfo(socket_path, registration.client_id)

    LOG.info("Starting new Pyre server.")
    server_start_status = await server_starter.run(
        str(configuration.get_binary_location(True)),
        configuration,
        FLAVOR,
    )
    if not isinstance(server_start_status, initialization.StartSuccess):
        return StartFailure(server_start_status.message)
    else:
        registration = await server_starter.register_client(socket_path)
        if isinstance(registration, RegitrationSuccess):
            return StartedServerInfo(socket_path, registration.client_id)
        else:
            return StartFailure(registration.message)


async def start_server(
    configuration: frontend_configuration.Base,
) -> Union[StartedServerInfo, StartFailure]:
    """Not thread-safe."""
    return await _start_server(configuration, PyreServerStarter())


async def stop_server(server_info: StartedServerInfo) -> None:
    """Stops the server completely. If any other clients are relying on this server as well, it will kill their connection so use sparingly."""
    with connections.connect(server_info.socket_path) as (
        input_channel,
        output_channel,
    ):
        output_channel.write(f"{stop.stop_message(FLAVOR)}\n")
        # Wait for the server to shutdown on its side
        input_channel.read()
