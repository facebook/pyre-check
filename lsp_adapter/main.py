# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import asyncio
import json
import logging
import os
import random
import re
import subprocess
import sys
from asyncio.events import AbstractEventLoop
from pathlib import Path
from typing import Any, Dict, List, Optional, Union

from ..client.commands.persistent import Persistent
from ..client.find_directories import find_global_and_local_root
from ..client.json_rpc import JSON, Request, write_lsp_request, ByNameParameters
from ..client.resources import get_configuration_value, log_directory
from ..client.socket_connection import SocketConnection


class AdapterException(Exception):
    pass


def _find_local_root(base: Path) -> Optional[Path]:
    found_root = find_global_and_local_root(base)
    return None if found_root is None else found_root.local_root


def _get_log_file(current_directory: str) -> str:
    local_root = _find_local_root(Path(current_directory))
    return str(
        log_directory(
            current_directory,
            str(local_root) if local_root is not None else None,
            "server",
        )
        / "adapter.log"
    )


def _socket_exists(current_directory: str) -> bool:
    local_root = _find_local_root(Path(current_directory))
    return Path.exists(
        log_directory(
            current_directory,
            str(local_root) if local_root is not None else None,
            "server",
        )
        / "adapter.sock"
    )


def _start_server(current_directory: str) -> None:
    subprocess.run(["pyre", "start"], cwd=current_directory, env=os.environ, check=True)


def _get_version(root: str) -> str:
    return get_configuration_value(root, "version")


def _parse_json_rpc(data: bytes) -> List[JSON]:
    json_bodies = []
    for request_data in re.split(r"Content-Length:.+\d*\r\n\r\n", data.decode("utf-8")):
        if not request_data == "":
            json_bodies.append(json.loads(request_data))
    return json_bodies


def _should_restart(data: JSON) -> bool:
    result = data.get("result")
    return result is not None and result.get("title") == "restart"


class Notifications:
    @classmethod
    def show_message(
        cls, method: str, message: str, actions: Optional[List[Dict[str, str]]] = None
    ) -> None:
        request = Request(
            method=method,
            parameters=ByNameParameters(
                {"message": message, "type": 1, "actions": actions or []}
            ),
            id=str(random.randrange(1000)),
        )
        write_lsp_request(sys.stdout.buffer, request)

    @classmethod
    def prompt_restart(cls) -> None:
        cls.show_message(
            method="window/showMessageRequest",
            message="Pyre server has crashed. Restart to reconnect.",
            actions=[{"title": "restart"}],
        )

    @classmethod
    def show_server_crashed(cls) -> None:
        cls.show_message(method="window/showStatus", message="Pyre server crashed.")

    @classmethod
    def show_pyre_initialize_error(cls, project_root: str) -> None:
        cls.show_message(
            method="window/showMessageRequest",
            message=f"Unable to start Pyre server. Pyre errors will \
            not be shown for files in `{project_root}`",
        )


class AdapterProtocol(asyncio.Protocol):
    """
    Listens to requests from VSCode, and writes them to the
    Pyre server via the open socket connection.
    """

    def __init__(self, socket: SocketConnection, root: str) -> None:
        self.socket = socket
        self.root = root
        self.transport: Optional[asyncio.transports.BaseTransport] = None

    def connection_made(self, transport: asyncio.transports.BaseTransport) -> None:
        self.transport = transport

    def data_received(self, data: bytes) -> None:
        json_bodies = _parse_json_rpc(data)
        for json_body in json_bodies:
            if _should_restart(json_body):
                transport = self.transport
                if transport:
                    transport.close()
                raise AdapterException
            else:
                request = Request.from_json(json_body)
                write_lsp_request(self.socket.output, request)
                self.socket.output.flush()


class SocketProtocol(asyncio.Protocol):
    """
    Listens for messages from the Pyre Server and writes
    them to stdout (or to VSCode)
    """

    def data_received(self, data: bytes) -> None:
        sys.stdout.buffer.write(data)
        sys.stdout.buffer.flush()

    def connection_lost(self, exc: Optional[Exception]) -> None:
        Notifications.show_server_crashed()
        Notifications.prompt_restart()


def add_socket_connection(loop: AbstractEventLoop, root: str) -> SocketConnection:
    local_root = _find_local_root(Path(root))
    socket_connection = SocketConnection(
        str(log_directory(root, str(local_root) if local_root is not None else None)),
        "adapter.sock",
    )
    socket_connection.connect()
    socket_connection.perform_handshake(_get_version(root))
    # pyre-fixme[16]: `AbstractEventLoop` has no attribute `connect_accepted_socket`.
    socket_reader = loop.connect_accepted_socket(
        SocketProtocol, socket_connection.socket
    )
    loop.run_until_complete(socket_reader)
    return socket_connection


def error_handler(loop: AbstractEventLoop, context: Dict[str, Any]) -> None:
    if isinstance(context["exception"], AdapterException):
        try:
            loop.stop()
            loop.close()
        except Exception as exception:
            logging.debug(exception)
    else:
        logging.debug(context)


def run_server(loop: AbstractEventLoop, root: str) -> None:
    logging.basicConfig(
        filename=_get_log_file(root),
        level=logging.DEBUG,
        format="%(asctime)s %(message)s",
        datefmt="%m/%d/%Y %I:%M:%S %p",
        filemode="w",
    )
    logging.info("Starting adapter.")
    socket_connection = add_socket_connection(loop, root)
    stdin_pipe_reader = loop.connect_read_pipe(
        lambda: AdapterProtocol(socket_connection, root), sys.stdin
    )
    loop.run_until_complete(stdin_pipe_reader)
    loop.set_exception_handler(error_handler)
    loop.run_forever()


def start_and_run_server(loop: AbstractEventLoop, root: str) -> None:
    try:
        _start_server(root)
        run_server(loop, root)
    except Exception:
        Persistent.run_null_server()


def main(root: str, null_server: bool) -> None:
    try:
        loop: AbstractEventLoop = asyncio.get_event_loop()
        start_and_run_server(loop, root)
    finally:
        loop.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="LSP adapter for Pyre.")
    parser.add_argument("--null-server", default=False, action="store_true")
    parser.add_argument("--root", type=str, required=True)
    arguments: argparse.Namespace = parser.parse_args()
    main(arguments.root, arguments.null_server)
