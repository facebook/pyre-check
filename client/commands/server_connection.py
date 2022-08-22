# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import contextlib
import io
import socket
from pathlib import Path
from typing import BinaryIO, Iterator, TextIO, Tuple


class ConnectionFailure(Exception):
    pass


@contextlib.contextmanager
def connect(
    socket_path: Path,
) -> Iterator[Tuple[BinaryIO, BinaryIO]]:
    """
    Connect to the socket at given path. Once connected, create an input and
    an output stream from the socket. Both the input stream and the output
    stream are in raw binary mode: read/write APIs of the streams need to use
    `bytes` rather than `str`. The API is intended to be used like this:

    ```
    with connect(socket_path) as (input_stream, output_stream):
        # Read from input_stream and write into output_stream here
        ...
    ```

    Socket creation, connection, and closure will be automatically handled
    inside this context manager. If any of the socket operations fail, raise
    `ConnectionFailure`.
    """
    try:
        with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as client_socket:
            client_socket.connect(str(socket_path))
            with client_socket.makefile(
                mode="rb"
            ) as input_channel, client_socket.makefile(mode="wb") as output_channel:
                yield (input_channel, output_channel)
    except OSError as error:
        raise ConnectionFailure() from error


@contextlib.contextmanager
def connect_in_text_mode(
    socket_path: Path,
) -> Iterator[Tuple[TextIO, TextIO]]:
    """
    This is a line-oriented higher-level API than `connect`. It can be used
    when the caller does not want to deal with the complexity of binary I/O.

    The behavior is the same as `connect`, except the streams that are created
    operates in text mode. Read/write APIs of the streams uses UTF-8 encoded
    `str` instead of `bytes`. Those operations are also line-buffered, meaning
    that the streams will automatically be flushed once the newline character
    is encountered.
    """
    with connect(socket_path) as (input_channel, output_channel):
        yield (
            io.TextIOWrapper(
                input_channel,
                line_buffering=True,
                errors="replace",
            ),
            io.TextIOWrapper(
                output_channel,
                line_buffering=True,
                errors="replace",
            ),
        )
