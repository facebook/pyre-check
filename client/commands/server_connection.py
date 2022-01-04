# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import contextlib
import hashlib
import io
import socket
import tempfile
from pathlib import Path
from typing import BinaryIO, Generator, Optional, TextIO, Tuple


class ConnectionFailure(Exception):
    pass


def get_socket_path(
    root: Path, global_root: Path, relative_local_root: Optional[Path]
) -> Path:
    """
    Determine where the server socket file is located. We can't directly use
    `log_directory` because of the ~100 character length limit on Unix socket
    file paths.

    Implementation needs to be kept in sync with the `user_independent_socket_path_of`
    function in `pyre/new_server/start.ml`.
    """
    project_identifier = str(global_root)
    if relative_local_root is not None:
        project_identifier = project_identifier + "//" + str(relative_local_root)
    project_identifier = project_identifier.encode("utf-8")

    project_hash = hashlib.md5(project_identifier).hexdigest()
    return root / f"pyre_server_{project_hash}.sock"


def get_default_socket_root() -> Path:
    # TODO(T77556312): It might be cleaner to turn the root dir into a
    # configuration option instead.
    return Path(tempfile.gettempdir())


def get_default_socket_path(
    project_root: Path, relative_local_root: Optional[Path]
) -> Path:
    return get_socket_path(get_default_socket_root(), project_root, relative_local_root)


@contextlib.contextmanager
def connect(socket_path: Path) -> Generator[Tuple[BinaryIO, BinaryIO], None, None]:
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
) -> Generator[Tuple[TextIO, TextIO], None, None]:
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
            io.TextIOWrapper(input_channel, encoding="utf-8", line_buffering=True),
            io.TextIOWrapper(output_channel, encoding="utf-8", line_buffering=True),
        )
