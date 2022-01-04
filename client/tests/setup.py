# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio
import contextlib
import functools
import json
import os
import socketserver
import tempfile
import threading
from pathlib import Path
from typing import (
    Any,
    Awaitable,
    Callable,
    Generator,
    Iterable,
    Mapping,
    Optional,
    Type,
    TypeVar,
)

from pyre_extensions import ParameterSpecification

from ..find_directories import CONFIGURATION_FILE, LOCAL_CONFIGURATION_FILE


TParams = ParameterSpecification("TParams")
T = TypeVar("T")


def ensure_files_exist(root: Path, relatives: Iterable[str]) -> None:
    for relative in relatives:
        full_path = root / relative
        full_path.parent.mkdir(parents=True, exist_ok=True)
        full_path.touch(exist_ok=True)


def ensure_directories_exists(root: Path, relatives: Iterable[str]) -> None:
    for relative in relatives:
        full_path = root / relative
        full_path.mkdir(parents=True, exist_ok=True)


def write_configuration_file(
    root: Path, content: Mapping[str, Any], relative: Optional[str] = None
) -> None:
    if relative is None:
        (root / CONFIGURATION_FILE).write_text(json.dumps(content))
    else:
        local_root = root / relative
        local_root.mkdir(parents=True, exist_ok=True)
        (local_root / LOCAL_CONFIGURATION_FILE).write_text(json.dumps(content))


@contextlib.contextmanager
def switch_working_directory(directory: Path) -> Generator[None, None, None]:
    original_directory = Path(".").resolve()
    try:
        os.chdir(str(directory))
        yield None
    finally:
        os.chdir(str(original_directory))


@contextlib.contextmanager
def switch_environment(environment: Mapping[str, str]) -> Generator[None, None, None]:
    old_environment = dict(os.environ)
    os.environ.clear()
    os.environ.update(environment)
    try:
        yield
    finally:
        os.environ.clear()
        os.environ.update(old_environment)


def async_test(func: Callable[TParams, Awaitable[T]]) -> Callable[TParams, T]:
    """
    Simple Decorator to allow for asyncio test methods in a standard
    `unittest.TestCase`.
    """

    @functools.wraps(func)
    def wrapper(*args: TParams.args, **kwargs: TParams.kwargs) -> T:
        return asyncio.get_event_loop().run_until_complete(func(*args, **kwargs))

    return wrapper


class TestServer(socketserver.ThreadingMixIn, socketserver.UnixStreamServer):
    pass


@contextlib.contextmanager
def spawn_unix_stream_server_with_socket(
    handler: Type[socketserver.BaseRequestHandler], socket_path: Path
) -> Generator[None, None, None]:
    # Spawn a test server on another thread
    server = TestServer(str(socket_path), handler)
    server_thread = threading.Thread(target=server.serve_forever)
    try:
        server_thread.start()
        yield
    finally:
        # Shutdown the server and terminate the test
        server.shutdown()
        server.server_close()


@contextlib.contextmanager
def spawn_unix_stream_server(
    handler: Type[socketserver.BaseRequestHandler],
) -> Generator[Path, None, None]:
    # Force /tmp as the temporary root, so path length would be under control
    with tempfile.TemporaryDirectory(dir="/tmp") as socket_root:
        socket_path = Path(socket_root) / "test.socket"
        with spawn_unix_stream_server_with_socket(handler, socket_path):
            yield socket_path
