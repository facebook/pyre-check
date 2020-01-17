# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import logging
from contextlib import contextmanager
from pathlib import Path
from typing import Generator, List, Optional

import psutil

from .filesystem import remove_if_exists


LOG = logging.getLogger(__name__)  # type: logging.Logger


SUBDIRECTORY_NAME = "pid_files"


@contextmanager
def _register(pid: int, absolute_pid_path: Path) -> Generator[None, None, None]:
    absolute_pid_path.parent.mkdir(parents=True, exist_ok=True)
    LOG.debug(
        "Registering process with pid %d in pid file `%s`", pid, absolute_pid_path
    )
    absolute_pid_path.write_text(str(pid))
    try:
        yield
    finally:
        LOG.debug("Removing pid file: `%s`", str(absolute_pid_path))
        remove_if_exists(str(absolute_pid_path))


def get_process(pid_path: str) -> Optional[psutil.Process]:
    try:
        pid = int(Path(pid_path).resolve().read_text())
    except FileNotFoundError:
        return None

    try:
        return psutil.Process(pid)
    except psutil.Error:
        return None


@contextmanager
def register_unique_process(pid: int, pid_path: str) -> Generator[None, None, None]:
    absolute_pid_path = Path(pid_path).resolve()
    with _register(pid, absolute_pid_path):
        yield


@contextmanager
def register_non_unique_process(
    pid: int, name: str, log_directory: str
) -> Generator[None, None, None]:
    absolute_pid_path = Path(
        log_directory, SUBDIRECTORY_NAME, f"{name}-{pid}.pid"
    ).resolve()
    with _register(pid, absolute_pid_path):
        yield


def get_processes(name: str, log_directory: str) -> List[psutil.Process]:
    process_paths = Path(log_directory, SUBDIRECTORY_NAME).glob(f"{name}-*.pid")
    return list(filter(None, (get_process(str(path)) for path in process_paths)))
