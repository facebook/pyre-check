# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import dataclasses
import logging
from typing import Optional

import dataclasses_json

from .. import (
    command_arguments,
    configuration as configuration_module,
    identifiers,
    version,
)
from . import commands, daemon_socket, frontend_configuration

LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class Info(dataclasses_json.DataClassJsonMixin):
    socket_path: str
    binary_location: Optional[str]
    client_version: str
    binary_version: Optional[str]
    log_directory: str
    client_logs: str
    server_log_directory: str
    current_server_logs: str

    def display(self) -> str:
        return "\n".join(
            [
                f"{key}: {value}"
                for key, value in self.to_dict().items()
                if value is not None
            ]
        )

    @classmethod
    def get(
        cls,
        configuration: frontend_configuration.Base,
        arguments: command_arguments.CommandArguments,
    ) -> "Info":
        binary_location = configuration.get_binary_location(download_if_needed=False)
        if binary_location is None:
            LOG.warning("Could not locate a Pyre binary to run.")
        log_directory = configuration.get_log_directory()
        client_logs = log_directory / "pyre.stderr"
        server_log_directory = log_directory / "new_server"
        current_server_logs = server_log_directory / "server.stderr"
        client_version = version.__version__
        try:
            binary_version = configuration.get_binary_version()
        except Exception:
            binary_version = None
        socket_path = daemon_socket.get_socket_path(
            configuration.get_project_identifier(),
            flavor=identifiers.PyreFlavor.CLASSIC,
        )
        return cls(
            socket_path=str(socket_path),
            current_server_logs=str(current_server_logs),
            log_directory=str(log_directory),
            server_log_directory=str(server_log_directory),
            client_logs=str(client_logs),
            binary_location=None if binary_location is None else str(binary_location),
            client_version=client_version,
            binary_version=binary_version,
        )


def run_info(
    configuration: frontend_configuration.Base,
    arguments: command_arguments.CommandArguments,
) -> None:
    info = Info.get(
        configuration=configuration,
        arguments=arguments,
    )
    if arguments.output == command_arguments.JSON:
        print(info.to_json(indent=2))
    else:
        print(info.display())


def run(
    configuration: configuration_module.Configuration,
    arguments: command_arguments.CommandArguments,
) -> commands.ExitCode:
    run_info(
        configuration=frontend_configuration.OpenSource(configuration),
        arguments=arguments,
    )
    return commands.ExitCode.SUCCESS
