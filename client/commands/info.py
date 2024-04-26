# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
Logic for the `pyre info` command, which prints information about
the current pyre environment (e.g. socket path, log locations).
"""


import dataclasses
import logging
from typing import Optional

import dataclasses_json

from .. import (
    command_arguments,
    daemon_socket,
    frontend_configuration,
    identifiers,
    version,
)
from . import commands, start

LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class Info(dataclasses_json.DataClassJsonMixin):
    socket_path: str
    binary_location: Optional[str]
    client_version: str
    binary_version: Optional[str]
    log_directory: str
    client_logs: str
    codenav_client_logs: str
    server_log_directory: str
    codenav_server_log_directory: str
    current_server_logs: str
    current_codenav_server_logs: str

    def display(self) -> str:
        max_key_length = max(len(key) for key, _ in self.to_dict().items()) + 1
        return "\n".join(
            [
                f"{str(key).ljust(max_key_length)}: {value}"
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
        flavor = identifiers.PyreFlavor.CLASSIC
        start_command = configuration.get_server_start_command(download_if_needed=True)
        if start_command is None:
            LOG.warning("Could not locate a Pyre binary to run.")
        log_directory = configuration.get_log_directory()
        client_logs = log_directory / "pyre.stderr"
        codenav_client_logs = log_directory / "pyre__code_navigation.stderr"
        server_log_directory = log_directory / flavor.server_log_subdirectory()
        codenav_server_log_directory = (
            log_directory
            / identifiers.PyreFlavor.CODE_NAVIGATION.server_log_subdirectory()
        )
        current_server_logs = start.deamon_current_log_path(
            server_log_directory, flavor
        )
        codenav_current_server_log = start.deamon_current_log_path(
            codenav_server_log_directory, identifiers.PyreFlavor.CODE_NAVIGATION
        )
        client_version = version.__version__
        try:
            binary_version = configuration.get_binary_version()
        except Exception:
            binary_version = None
        socket_path = daemon_socket.get_socket_path(
            configuration.get_project_identifier(),
            flavor=flavor,
        )
        return cls(
            socket_path=str(socket_path),
            current_server_logs=str(current_server_logs),
            current_codenav_server_logs=str(codenav_current_server_log),
            log_directory=str(log_directory),
            server_log_directory=str(server_log_directory),
            codenav_server_log_directory=str(codenav_server_log_directory),
            client_logs=str(client_logs),
            codenav_client_logs=str(codenav_client_logs),
            binary_location=(
                None
                if start_command is None
                else str(start_command.get_pyre_binary_location())
            ),
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
    configuration: frontend_configuration.Base,
    arguments: command_arguments.CommandArguments,
) -> commands.ExitCode:
    run_info(
        configuration=configuration,
        arguments=arguments,
    )
    return commands.ExitCode.SUCCESS
