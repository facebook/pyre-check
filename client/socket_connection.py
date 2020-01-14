# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import logging
import os
import socket
from types import TracebackType
from typing import BinaryIO, Optional

from . import json_rpc


LOG: logging.Logger = logging.getLogger(__name__)


class SocketException(Exception):
    pass


class SocketConnection(object):
    def __init__(self, root: str) -> None:
        self.socket: socket.socket = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.root = root
        self.input: BinaryIO = self.socket.makefile(mode="rb")
        self.output: BinaryIO = self.socket.makefile(mode="wb")

    def connect(self) -> "SocketConnection":
        socket_path = self._socket_path()
        try:
            self.socket.connect(os.path.realpath(socket_path))
            return self
        except OSError as error:
            raise SocketException(
                "Failed to connect to server at `{}`. Reason: `{}`".format(
                    socket_path, error
                )
            )

    def perform_handshake(self, version_hash: str) -> None:
        try:
            json_rpc.perform_handshake(self.input, self.output, version_hash)
        except (OSError, ValueError) as error:
            raise SocketException(
                "Exception encountered during handshake: `{}`".format(error)
            )

    def send(self, request: json_rpc.Request) -> None:
        if not request.write(self.output):
            LOG.info("Failed to communicate with server. Shutting down.")
            raise SocketException

    def read(self) -> json_rpc.Response:
        return json_rpc.read_response(self.input)

    def _socket_path(self) -> str:
        return os.path.join(self.root, "server", "json_server.sock")

    def __enter__(self) -> "SocketConnection":
        self.connect()
        return self

    def __exit__(
        self,
        _type: Optional[BaseException],
        _value: Optional[BaseException],
        _traceback: Optional[TracebackType],
    ) -> None:
        self.close()

    def __del__(self) -> None:
        self.close()

    def close(self) -> None:
        try:
            self.socket.close()
        except OSError:
            pass
