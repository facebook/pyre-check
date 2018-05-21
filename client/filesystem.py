# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import errno
import fcntl
import json
import logging
import os
import shutil
import subprocess
from contextlib import contextmanager
from time import time
from typing import Generator, List, Optional

from . import log


LOG = logging.getLogger(__name__)


class SharedSourceDirectory:

    def __init__(self, source_directories, isolate: bool = False):
        self._source_directories = set(source_directories)
        self._isolate = isolate

    def get_root(self) -> str:
        suffix = "_{}".format(str(os.getpid())) if self._isolate else ""
        return ".pyre/shared_source_directory{}".format(suffix)

    def prepare(self) -> None:
        start = time()
        root = self.get_root()
        LOG.info("Constructing shared directory `%s`", root)

        try:
            os.makedirs(root)
        except OSError:
            pass  # Swallow.

        lock = os.path.join(root, ".pyre.lock")
        with try_lock(lock):
            try:
                with open(os.path.join(root, ".pyre.source_directories")) as file:
                    tracked = set(json.load(file))

                if self._source_directories.issubset(tracked):
                    # We might want to merge in additional files.
                    LOG.info("Shared source directory is up to date")
                    return
            except (OSError, json.JSONDecodeError):
                pass

            # Clear the directory and merge in files.
            LOG.info("Shared source directory is stale, updating...")
            self._clear()
            self._merge()

            # Write out tracked targets.
            with open(os.path.join(root, ".pyre.source_directories"), "w") as file:
                json.dump(list(self._source_directories), file)

            LOG.log(log.PERFORMANCE, "Merged source directories in %fs", time() - start)

    def _clear(self):
        root = self.get_root()
        for path in os.listdir(root):
            if path.startswith(".pyre"):
                continue

            path = os.path.join(root, path)
            remove_if_exists(path)

    def _merge(self) -> None:
        root = self.get_root()

        all_paths = {}
        for source_directory in self._source_directories:
            paths = find_python_paths(root=source_directory)
            for path in paths:
                if not path:
                    continue
                if is_empty(path):
                    # don't symlink empty __init__ files which might
                    # override legitimate ones
                    continue

                relative = os.path.relpath(path, source_directory)
                all_paths[relative] = os.path.join(path)

        for relative, original in all_paths.items():
            merged = os.path.join(root, relative)
            directory = os.path.dirname(merged)
            try:
                os.makedirs(directory)
            except OSError:
                pass
            try:
                os.symlink(original, merged)
            except OSError as error:
                if error.errno == errno.EEXIST:
                    os.unlink(merged)
                    os.symlink(original, merged)
                else:
                    LOG.error(str(error))


def find_python_paths(root: str) -> List[str]:
    root = os.path.abspath(root)  # Return absolute paths.
    try:
        output = (
            subprocess.check_output(
                [
                    "find",
                    root,
                    # All files ending in .py or .pyi ...
                    "(",
                    "-name",
                    "*.py",
                    "-or",
                    "-name",
                    "*.pyi",
                    ")",
                    # ... and that are either regular files ...
                    "(",
                    "-type",
                    "f",
                    "-or",
                    # ... or symlinks pointing to existing files.
                    "(",
                    "-type",
                    "l",
                    "-exec",
                    "test",
                    "-f",
                    "{}",
                    ";",
                    ")",
                    ")",
                    # Print all such files.
                    "-print",
                ]
            )
            .decode("utf-8")
            .strip()
        )
        return output.split("\n")
    except subprocess.CalledProcessError:
        LOG.error(
            "pyre was unable to locate a source directory. "
            "Ensure that your project is built and re-run pyre."
        )
        exit(1)


def is_empty(path: str) -> bool:
    try:
        return os.stat(path).st_size == 0
    except FileNotFoundError:
        return False


def remove_if_exists(path: str) -> None:
    try:
        os.remove(path)
    except OSError:
        pass  # Not a file.
    try:
        shutil.rmtree(path)
    except OSError:
        pass  # Not a directory.


@contextmanager
def try_lock(path: str) -> Generator[Optional[int], None, None]:
    """Raises an OSError if the lock can't be acquired"""
    try:
        with open(path, "w+") as lockfile:
            fcntl.lockf(lockfile.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
            yield lockfile.fileno()
            fcntl.lockf(lockfile.fileno(), fcntl.LOCK_UN)

    except FileNotFoundError:
        yield
