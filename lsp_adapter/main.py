# Copyright (c) 2019-present, Facebook, Inc.
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

from ..client.find_directories import find_local_root
from ..client.json_rpc import JSON, Request, Response
from ..client.resources import get_configuration_value, log_directory
from ..client.socket_connection import SocketConnection


class AdapterException(Exception):
    pass


def _should_run_null_server(null_server_flag: bool) -> bool:
    # TODO[T58989824]: We also need to check if the project can be run here.
    # Needs updating to mimic the current implementation (i.e. catch the buck errors)
    return null_server_flag


def _get_log_file(current_directory: str) -> str:
    local_root = find_local_root(original_directory=current_directory)
    return str(log_directory(current_directory, local_root, "server") / "adapter.log")


def _socket_exists(current_directory: str) -> bool:
    local_root = find_local_root(original_directory=current_directory)
    return Path.exists(
        log_directory(current_directory, local_root, "server") / "adapter.sock"
    )


def _start_server(current_directory: str) -> None:
    subprocess.run(["pyre", "start"], cwd=current_directory, env=os.environ, check=True)


def _get_version(root: str) -> str:
    return get_configuration_value(root, "version")


def _null_initialize_response(request_id: Optional[Union[str, int]]) -> None:
    response = Response(id=request_id, result={"capabilities": {}})
    response.write(sys.stdout.buffer)


def _parse_json_rpc(data: bytes) -> JSON:
    body = re.sub(r"Content-Length:.+\d*\r\n\r\n", "", data.decode("utf-8"))
    return json.loads(body)


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
            parameters={"message": message, "type": 1, "actions": actions or []},
            id=str(random.randrange(1000)),
        )
        request.write(sys.stdout.buffer)

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


class NullServerAdapterProtocol(asyncio.Protocol):
    def data_received(self, data: bytes) -> None:
        json_body = _parse_json_rpc(data)
        _null_initialize_response(json_body["id"])


class AdapterProtocol(asyncio.Protocol):
    def __init__(self, socket: SocketConnection, root: str) -> None:
        self.socket = socket
        self.root = root
        self.transport: Optional[asyncio.transports.BaseTransport] = None

    def connection_made(self, transport: asyncio.transports.BaseTransport) -> None:
        self.transport = transport

    def data_received(self, data: bytes) -> None:
        json_body = _parse_json_rpc(data)
        if _should_restart(json_body):
            transport = self.transport
            if transport:
                transport.close()
            raise AdapterException
        else:
            self.socket.output.write(data)
            self.socket.output.flush()


class SocketProtocol(asyncio.Protocol):
    def data_received(self, data: bytes) -> None:
        sys.stdout.buffer.write(data)
        sys.stdout.buffer.flush()

    def connection_lost(self, exc: Optional[Exception]) -> None:
        Notifications.show_server_crashed()
        Notifications.prompt_restart()


def run_null_server(loop: AbstractEventLoop) -> None:
    stdin_pipe_reader = loop.connect_read_pipe(NullServerAdapterProtocol, sys.stdin)
    loop.run_until_complete(stdin_pipe_reader)
    loop.run_forever()


def add_socket_connection(loop: AbstractEventLoop, root: str) -> SocketConnection:
    local_root = find_local_root(original_directory=root)
    socket_connection = SocketConnection(
        str(log_directory(root, local_root)), "adapter.sock"
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
    logging.info("Starting adapter.")
    socket_connection = add_socket_connection(loop, root)
    stdin_pipe_reader = loop.connect_read_pipe(
        lambda: AdapterProtocol(socket_connection, root), sys.stdin
    )
    loop.run_until_complete(stdin_pipe_reader)
    loop.set_exception_handler(error_handler)
    loop.run_forever()


def main(arguments: argparse.Namespace) -> None:
    root = arguments.root
    logging.basicConfig(
        filename=_get_log_file(root),
        level=logging.DEBUG,
        format="%(asctime)s %(message)s",
        datefmt="%m/%d/%Y %I:%M:%S %p",
        filemode="w",
    )
    loop: AbstractEventLoop = asyncio.get_event_loop()
    try:
        if _should_run_null_server(arguments.null_server):
            return run_null_server(loop)
        _start_server(root)
        run_server(loop, root)
    finally:
        loop.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="LSP adapter for Pyre.")
    parser.add_argument("--null-server", default=False, action="store_true")
    parser.add_argument("--root", type=str, required=True)
    arguments: argparse.Namespace = parser.parse_args()
    main(arguments)
