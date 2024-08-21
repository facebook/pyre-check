# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module provides the logic for the `pyre servers` command, which lists
running servers (i.e. daemons - background backend processes available for
incremental checks and queries, including from language servers).
"""

import dataclasses
import json
import logging
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Sequence, Union

import tabulate
from typing_extensions import TypedDict

from .. import command_arguments, daemon_socket, identifiers, log
from ..language_server import connections
from . import commands, stop

LOG: logging.Logger = logging.getLogger(__name__)


class _ServerConfigurationJSONSchema(TypedDict):
    global_root: str


class _ServerInfoJSONSchema(TypedDict):
    pid: int
    version: str
    configuration: _ServerConfigurationJSONSchema


class InvalidServerResponse(Exception):
    pass


@dataclasses.dataclass(frozen=True)
class RunningServerStatus:
    pid: int
    version: str
    global_root: str
    flavor: str
    relative_local_root: Optional[str] = None

    @staticmethod
    def from_json(
        input_json: Dict[str, object], flavor: identifiers.PyreFlavor
    ) -> "RunningServerStatus":
        pid = input_json.get("pid", None)
        if not isinstance(pid, int):
            raise InvalidServerResponse(f"Expect `pid` to be an int but got {pid}")
        version = input_json.get("version", None)
        if not isinstance(version, str):
            raise InvalidServerResponse(
                f"Expect `version` to be a string but got {version}"
            )
        global_root = input_json.get("global_root", None)
        if not isinstance(global_root, str):
            raise InvalidServerResponse(
                f"Expect `global_root` to be a string but got {global_root}"
            )
        relative_local_root = input_json.get("relative_local_root", None)
        if relative_local_root is not None and not isinstance(relative_local_root, str):
            raise InvalidServerResponse(
                "Expected `relative_local_root` to be a string but got "
                f"{relative_local_root}"
            )
        return RunningServerStatus(
            pid=pid,
            version=version,
            global_root=global_root,
            relative_local_root=relative_local_root,
            flavor=flavor.value,
        )

    @staticmethod
    def from_server_response(
        response: str, flavor: identifiers.PyreFlavor
    ) -> "RunningServerStatus":
        try:
            response_json = json.loads(response)
            if (
                not isinstance(response_json, list)
                or len(response_json) < 2
                or response_json[0] != "Info"
                or not isinstance(response_json[1], dict)
            ):
                message = f"Unexpected JSON response: {response_json}"
                raise InvalidServerResponse(message)
            return RunningServerStatus.from_json(response_json[1], flavor)
        except json.JSONDecodeError as error:
            message = f"Cannot parse response as JSON: {error}"
            raise InvalidServerResponse(message) from error

    def to_json(self) -> Dict[str, object]:
        return {
            "status": "running",
            "pid": self.pid,
            "version": self.version,
            "global_root": self.global_root,
            "relative_local_root": self.relative_local_root,
            "flavor": self.flavor,
        }


@dataclasses.dataclass(frozen=True)
class DefunctServerStatus:
    socket_path: str

    def to_json(self) -> Dict[str, object]:
        return {"status": "defunct", "socket": self.socket_path}


@dataclasses.dataclass(frozen=True)
class AllServerStatus:
    running: List[RunningServerStatus] = dataclasses.field(default_factory=list)
    defunct: List[DefunctServerStatus] = dataclasses.field(default_factory=list)

    def to_json(self) -> List[Dict[str, Any]]:
        return [status.to_json() for status in self.running] + [
            status.to_json() for status in self.defunct
        ]


def _get_server_status(
    socket_path: Path,
    flavor: identifiers.PyreFlavor,
) -> Union[RunningServerStatus, DefunctServerStatus]:
    try:
        with connections.connect(socket_path) as (
            input_channel,
            output_channel,
        ):
            if flavor != identifiers.PyreFlavor.CODE_NAVIGATION:
                output_channel.write('["GetInfo"]\n')
                return RunningServerStatus.from_server_response(
                    input_channel.readline(), flavor
                )
            # For now, we assume that a code-navigation server we can connect to is fine.
            output_channel.write('["Query", ["GetInfo"]]\n')
            return RunningServerStatus.from_server_response(
                input_channel.readline(), flavor
            )

    except connections.ConnectionFailure:
        return DefunctServerStatus(str(socket_path))


def _print_running_server_status(running_status: Sequence[RunningServerStatus]) -> None:
    if len(running_status) == 0:
        log.stdout.write("No server is currently running.\n")
    else:
        log.stdout.write("Running Servers:\n\n")
        log.stdout.write(
            tabulate.tabulate(
                [
                    [
                        status.pid,
                        status.global_root,
                        status.relative_local_root or "",
                        status.version,
                        status.flavor,
                    ]
                    for status in running_status
                ],
                headers=[
                    "PID",
                    "Global Root",
                    "Relative Local Root",
                    "Version",
                    "Flavor",
                ],
            ),
        )
        log.stdout.write("\n")
    log.stdout.write("\n")


def _print_defunct_server_status(defunct_status: Sequence[DefunctServerStatus]) -> None:
    defunct_count = len(defunct_status)
    if defunct_count > 0:
        plurality = "" if defunct_count == 1 else "s"
        log.stdout.write(f"Found {defunct_count} defunct server{plurality} at:\n")
        for status in defunct_status:
            log.stdout.write(f" {status.socket_path}\n")
        log.stdout.write("\n")


def _print_server_status_json(server_status: AllServerStatus) -> None:
    log.stdout.write(json.dumps(server_status.to_json()))
    log.stdout.write("\n")


def _print_server_status(server_status: AllServerStatus, output_format: str) -> None:
    if output_format == command_arguments.TEXT:
        _print_running_server_status(server_status.running)
        _print_defunct_server_status(server_status.defunct)
    elif output_format == command_arguments.JSON:
        _print_server_status_json(server_status)


def _stop_server(socket_path: Path, flavor: identifiers.PyreFlavor) -> None:
    try:
        LOG.info(f"Stopping server at `{socket_path}...`")
        stop.stop_server(socket_path, flavor)
        LOG.info(f"Successfully stopped `{socket_path}.`")
    except connections.ConnectionFailure:
        LOG.info(f"Failed to connect to `{socket_path}`. Removing it...")
        stop.remove_socket_if_exists(socket_path)
    except Exception as error:
        LOG.warning(
            f"Exception occurred when trying to stop server at `{socket_path}`: {error}"
        )


def _find_server_flavor(socket_path: Path) -> identifiers.PyreFlavor:
    # Socket paths are of the form `/tmp/pyre_{md5}[__{flavor}].sock`.
    serialized_path = str(socket_path)
    for flavor in identifiers.PyreFlavor:
        if flavor.value in serialized_path:
            return flavor
    # No suffix indicates a classic server.
    return identifiers.PyreFlavor.CLASSIC


def find_all_servers(socket_paths: Iterable[Path]) -> AllServerStatus:
    running_servers = []
    defunct_servers = []

    for socket_path in socket_paths:
        flavor = _find_server_flavor(socket_path)
        status = _get_server_status(socket_path, flavor)
        if isinstance(status, RunningServerStatus):
            running_servers.append(status)
        else:
            defunct_servers.append(status)

    return AllServerStatus(running_servers, defunct_servers)


def find_all_servers_under(socket_root: Path) -> AllServerStatus:
    return find_all_servers(daemon_socket.find_socket_files(socket_root))


def run_list(output_format: str) -> commands.ExitCode:
    server_status = find_all_servers_under(daemon_socket.get_default_socket_root())
    _print_server_status(server_status, output_format)
    return commands.ExitCode.SUCCESS


def run_stop() -> commands.ExitCode:
    for socket_path in daemon_socket.find_socket_files(
        daemon_socket.get_default_socket_root()
    ):
        flavor = _find_server_flavor(socket_path)
        _stop_server(socket_path, flavor)
    LOG.info("Done\n")
    return commands.ExitCode.SUCCESS
