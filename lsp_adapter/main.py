# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import asyncio
import json
import re
import subprocess
import sys
from asyncio.events import AbstractEventLoop
from pathlib import Path
from typing import Optional, Union

from ..client.find_directories import find_local_root
from ..client.json_rpc import Response
from ..client.resources import get_configuration_value, log_directory
from ..client.socket_connection import SocketConnection


def _should_run_null_server(null_server_flag: bool) -> bool:
    # TODO[T58989824]: We also need to check if the project can be run here.
    # Needs updating to mimic the current implementation (i.e. catch the buck errors)
    return null_server_flag


def socket_exists(current_directory: str) -> bool:
    local_root = find_local_root(original_directory=current_directory)
    return Path.exists(
        log_directory(current_directory, local_root, "server") / "adapter.sock"
    )


def start_server(current_directory: str) -> None:
    subprocess.run(["pyre", "start"], cwd=current_directory)


def get_version(root: str) -> str:
    return get_configuration_value(root, "version")


def _null_initialize_response(request_id: Optional[Union[str, int]]) -> None:
    response = Response(id=request_id, result={"capabilities": {}})
    response.write(sys.stdout.buffer)


class NullServerAdapterProtocol(asyncio.Protocol):
    def data_received(self, data: bytes) -> None:
        body = re.sub(r"Content-Length:.+\d*\r\n\r\n", "", data.decode("utf-8"))
        json_body = json.loads(body)
        _null_initialize_response(json_body["id"])


class AdapterProtocol(asyncio.Protocol):
    def __init__(self, socket: SocketConnection) -> None:
        self.socket = socket

    def data_received(self, data: bytes) -> None:
        # TODO[T58989824]: Handle errors and lost connections
        self.socket.output.write(data)
        self.socket.output.flush()


class SocketProtocol(asyncio.Protocol):
    def data_received(self, data: bytes) -> None:
        sys.stdout.buffer.write(data)
        sys.stdout.buffer.flush()

    def connection_lost(self, exc: Optional[Exception]) -> None:
        # TODO[T58989824]: Handle when we lose connection to the server.
        pass


def run_null_server(loop: AbstractEventLoop) -> None:
    stdin_pipe_reader = loop.connect_read_pipe(NullServerAdapterProtocol, sys.stdin)
    loop.run_until_complete(stdin_pipe_reader)
    loop.run_forever()


def main(arguments: argparse.Namespace) -> None:
    root = arguments.root
    loop: AbstractEventLoop = asyncio.get_event_loop()
    try:
        if _should_run_null_server(arguments.null_server):
            return run_null_server(loop)
        if not socket_exists(root):
            start_server(root)
        local_root = find_local_root(original_directory=root)
        socket_connection = SocketConnection(
            str(log_directory(root, local_root)), "adapter.sock"
        )
        socket_connection.connect()
        socket_connection.perform_handshake(get_version(root))
        socket_reader = loop.connect_accepted_socket(
            SocketProtocol, socket_connection.socket
        )
        stdin_pipe_reader = loop.connect_read_pipe(
            lambda: AdapterProtocol(socket_connection), sys.stdin
        )
        loop.run_until_complete(stdin_pipe_reader)
        loop.run_until_complete(socket_reader)
        loop.run_forever()
    finally:
        loop.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="LSP adapter for Pyre.")
    parser.add_argument("--null-server", default=False, action="store_true")
    parser.add_argument("--root", type=str, required=True)
    arguments: argparse.Namespace = parser.parse_args()
    main(arguments)
