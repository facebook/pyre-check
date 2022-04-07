# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import fcntl
import logging
import os
from contextlib import contextmanager
from pathlib import Path
from typing import Generator, Optional


LOG: logging.Logger = logging.getLogger(__name__)


class EnvironmentException(Exception):
    pass


def readable_directory(directory: str) -> str:
    if not os.path.isdir(directory):
        raise EnvironmentException(f"`{directory}` is not a valid directory.")
    if not os.access(directory, os.R_OK):
        raise EnvironmentException(f"`{directory}` is not a readable directory.")
    return directory


def writable_directory(path: str) -> str:
    # Create the directory if it does not exist.
    try:
        os.makedirs(path)
    except FileExistsError:
        pass
    path = os.path.abspath(path)
    if not os.path.isdir(path):
        raise EnvironmentException(f"{path} is not a valid directory.")
    if not os.access(path, os.W_OK):
        raise EnvironmentException(f"{path} is not a writable directory.")
    return path


def expand_relative_path(root: str, path: str) -> str:
    expanded_path = Path(path).expanduser()
    if expanded_path.is_absolute():
        return str(expanded_path)
    else:
        return str(Path(root) / expanded_path)


def expand_global_root(path: str, global_root: str) -> str:
    if path.startswith("//"):
        return expand_relative_path(global_root, path[2:])
    return path


def file_or_directory_exists(path: str) -> str:
    if os.path.isdir(path) or os.path.isfile(path):
        return path
    raise ValueError(f"{path} is not a valid path")


def _lock_command(blocking: bool, is_shared_reader: bool) -> int:
    lock_command = fcntl.LOCK_SH if is_shared_reader else fcntl.LOCK_EX
    return lock_command if blocking else lock_command | fcntl.LOCK_NB


@contextmanager
def acquire_lock(
    path: str, blocking: bool, is_shared_reader: bool = False
) -> Generator[Optional[int], None, None]:
    """Raise an OSError if `blocking` is False and the lock can't be acquired.

    If `is_shared_reader=True`, then other processes can acquire the same
    lock with `is_shared_reader=True`, but not with `is_shared_reader=False`.
    Conversely, if `is_shared_reader=False`, then no other process can
    acquire the lock until it is released."""

    LOG.debug(
        "Trying to acquire %slock on file %s",
        "shared reader " if is_shared_reader else "",
        path,
    )
    try:
        with open(path, "w+") as lockfile:
            try:
                fcntl.lockf(
                    lockfile.fileno(), _lock_command(blocking, is_shared_reader)
                )
                yield lockfile.fileno()
            finally:
                fcntl.lockf(lockfile.fileno(), fcntl.LOCK_UN)
    except FileNotFoundError:
        LOG.debug(f"Unable to acquire lock because lock file {path} was not found")
        yield
