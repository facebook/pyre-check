# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import abc
from dataclasses import dataclass
from typing import Union

from pyre_extensions import override

from ..client import command_arguments, identifiers

from ..client.commands import frontend_configuration, initialization, start

FLAVOR: identifiers.PyreFlavor = identifiers.PyreFlavor.CODE_NAVIGATION


@dataclass
class StartedServerInfo:
    pass


@dataclass
class StartFailure:
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


async def _start_server(
    configuration: frontend_configuration.Base,
    server_starter: PyreServerStarterBase,
) -> Union[StartedServerInfo, StartFailure]:
    server_start_status = await server_starter.run(
        str(configuration.get_binary_location(True)),
        configuration,
        FLAVOR,
    )
    if not isinstance(server_start_status, initialization.StartSuccess):
        return StartFailure(server_start_status.message)
    else:
        return StartedServerInfo()


async def start_server(
    configuration: frontend_configuration.Base,
) -> Union[StartedServerInfo, StartFailure]:
    return await _start_server(configuration, PyreServerStarter())


async def stop_server() -> None:
    """Stops the server completely. If any other clients are relying on this server as well, it will kill their connection so use sparingly."""
    raise NotImplementedError()
