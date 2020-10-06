# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import json
import logging
from pathlib import Path
from typing import List, NamedTuple, Optional

from .. import command_arguments, configuration as configuration_module, log
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from .command import Command
from .stop import Stop


LOG: logging.Logger = logging.getLogger(__name__)


ROOT_PLACEHOLDER_NAME = "<root>"


class ServerDetails(NamedTuple):
    local_root: str
    pid: int
    server_pid_path: Path

    @staticmethod
    def _from_server_path(
        server_pid_path: Path, dot_pyre_root: Path
    ) -> "ServerDetails":
        return ServerDetails(
            pid=int(server_pid_path.read_text()),
            server_pid_path=server_pid_path,
            local_root=str(server_pid_path.relative_to(dot_pyre_root).parent.parent),
        )

    def is_root(self) -> bool:
        return self.local_root == "."

    @property
    def name(self) -> str:
        if self.is_root():
            return ROOT_PLACEHOLDER_NAME
        else:
            return self.local_root


class Servers(Command):
    NAME: str = "servers"

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        original_directory: str,
        *,
        configuration: Configuration,
        analysis_directory: Optional[AnalysisDirectory] = None,
        subcommand: Optional[str],
    ) -> None:
        super(Servers, self).__init__(
            command_arguments, original_directory, configuration, analysis_directory
        )
        self._subcommand = subcommand

    def is_root_server_running(self) -> bool:
        return any(server.is_root() for server in self._all_server_details())

    @staticmethod
    def _print_server_details(
        all_server_details: List[ServerDetails], output_format: str
    ) -> None:
        if output_format == command_arguments.JSON:
            server_details = [
                {"pid": details.pid, "name": details.name}
                for details in all_server_details
            ]
            log.stdout.write(json.dumps(server_details))
        else:
            if len(all_server_details) > 0:
                log.stdout.write(f"{'PID':<8}  project\n")
                for details in all_server_details:
                    log.stdout.write(f"{details.pid:<8}  {details.name}\n")
            else:
                log.stdout.write("No servers running")

    def _find_servers(self) -> List[Path]:
        return list(self._configuration.dot_pyre_directory.glob("**/server.pid"))

    def _all_server_details(self) -> List[ServerDetails]:
        return sorted(
            (
                ServerDetails._from_server_path(
                    server_path, self._configuration.dot_pyre_directory
                )
                for server_path in self._find_servers()
            ),
            key=lambda details: details.local_root,
        )

    def _stop_servers(self, servers: List[ServerDetails]) -> None:
        for server in servers:
            LOG.warning("Stopping server for `%s` with pid %d", server.name, server.pid)
            local_configuration = configuration_module.create_configuration(
                dataclasses.replace(
                    self._command_arguments, local_configuration=server.local_root
                ),
                Path(self._configuration.project_root),
            )
            if local_configuration is not None:
                Stop(
                    command_arguments=self._command_arguments,
                    original_directory=str(
                        Path(self._configuration.project_root, server.local_root)
                    ),
                    configuration=local_configuration,
                ).run()

    def _run(self) -> None:
        all_server_details = self._all_server_details()

        if self._subcommand == "list" or self._subcommand is None:
            self._print_server_details(all_server_details, self._output)
        elif self._subcommand == "stop":
            self._stop_servers(all_server_details)
