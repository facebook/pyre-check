# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import os
import socket
from types import TracebackType
from typing import BinaryIO, Optional

from . import language_server_protocol


class SocketException(Exception):
    pass


class SocketConnection(object):
    def __init__(self, socket_path: str) -> None:
        self.socket = socket.socket(
            socket.AF_UNIX, socket.SOCK_STREAM
        )  # type: socket.socket
        self.socket_path = socket_path
        self.input: BinaryIO = self.socket.makefile(mode="rb")
        self.output: BinaryIO = self.socket.makefile(mode="wb")

    def _connect(self) -> "SocketConnection":
        try:
            self.socket.connect(os.path.realpath(self.socket_path))
            return self
        except (ConnectionRefusedError, FileNotFoundError, OSError) as error:
            raise SocketException(
                "Failed to connect to server at `{}`. Reason: `{}`".format(
                    self.socket_path, error
                )
            )

    def perform_handshake(self, version_hash: str) -> None:
        try:
            language_server_protocol.perform_handshake(
                self.input, self.output, version_hash
            )
        except (OSError, ValueError) as error:
            raise SocketException(
                "Exception encountered during handshake: `{}`".format(error)
            )

    def __enter__(self) -> "SocketConnection":
        self._connect()
        return self

    def __exit__(
        self,
        _type: Optional[BaseException],
        _value: Optional[BaseException],
        _traceback: Optional[TracebackType],
    ) -> None:
        try:
            self.socket.close()
        except OSError:
            pass
