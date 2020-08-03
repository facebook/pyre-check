# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import errno
import fcntl
import functools
import logging
import os
import shutil
import subprocess
from contextlib import contextmanager
from typing import ContextManager, Dict, Generator, Iterable, List, Optional, Set

from .exceptions import EnvironmentException


LOG: logging.Logger = logging.getLogger(__name__)


def assert_readable_directory(directory: str, error_message_prefix: str = "") -> None:
    if not os.path.isdir(directory):
        raise EnvironmentException(
            f"{error_message_prefix}`{directory}` is not a valid directory."
        )
    if not os.access(directory, os.R_OK):
        raise EnvironmentException(
            f"{error_message_prefix}`{directory}` is not a readable directory."
        )


def readable_directory(directory: str) -> str:
    assert_readable_directory(directory)
    return directory


def assert_writable_directory(directory: str) -> None:
    if not os.path.isdir(directory):
        raise EnvironmentException("{} is not a valid directory.".format(directory))
    if not os.access(directory, os.W_OK):
        raise EnvironmentException("{} is not a writable directory.".format(directory))


def writable_directory(path: str) -> str:
    # Create the directory if it does not exist.
    try:
        os.makedirs(path)
    except FileExistsError:
        pass
    path = os.path.abspath(path)
    assert_writable_directory(path)
    return path


def translate_path(root: str, path: str) -> str:
    if os.path.isabs(path):
        return path

    translated = os.path.join(root, path)
    if os.path.exists(translated):
        return os.path.realpath(translated)

    return path


def expand_relative_path(root: str, path: str) -> str:
    path = os.path.expanduser(path)
    if os.path.isabs(path):
        return path
    else:
        return os.path.join(root, path)


def translate_paths(paths: Set[str], original_directory: str) -> Set[str]:
    current_directory = os.getcwd()
    if not original_directory.startswith(current_directory):
        return paths
    translation = os.path.relpath(original_directory, current_directory)
    if not translation:
        return paths
    return {translate_path(translation, path) for path in paths}


def find_root(original_directory: str, target_file: str) -> Optional[str]:
    current_directory = os.path.abspath(original_directory)
    while True:
        absolute = os.path.join(current_directory, target_file)
        if os.path.isfile(absolute):
            return current_directory

        parent_directory = os.path.dirname(current_directory)
        if current_directory == parent_directory:
            break
        current_directory = parent_directory
    return None


def exists(path: str) -> str:
    if not os.path.isfile(path):
        raise ValueError("%s is not a valid file" % path)
    return path


def is_parent(parent: str, child: str) -> bool:
    return child.startswith(parent.rstrip(os.sep) + os.sep)


def find_paths_with_extensions(root: str, extensions: Iterable[str]) -> List[str]:
    root = os.path.abspath(root)  # Return absolute paths.
    extension_filter = []
    for extension in extensions:
        if len(extension_filter) > 0:
            extension_filter.append("-or")
        extension_filter.extend(["-name", "*.{}".format(extension)])

    output = (
        subprocess.check_output(
            [
                "find",
                root,
                # All files ending with the given extensions ...
                "(",
                *extension_filter,
                ")",
                # ... and that are either regular files ...
                "(",
                "-type",
                "f",
                "-or",
                # ... or symlinks.
                "-type",
                "l",
                ")",
                # Print all such files.
                "-print",
            ],
            stderr=subprocess.DEVNULL,
        )
        .decode("utf-8")
        .strip()
    )
    return output.split("\n") if output else []


def find_python_paths(root: str) -> List[str]:
    try:
        return find_paths_with_extensions(root, ["py", "pyi"])
    except subprocess.CalledProcessError:
        raise EnvironmentException(
            "Pyre was unable to locate an analysis directory. "
            "Ensure that your project is built and re-run pyre."
        )


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


def _compute_symbolic_link_mapping(
    directory: str, extensions: Iterable[str]
) -> Dict[str, str]:
    """
        Given a shared analysis directory, produce a mapping from actual source files
        to files contained within this directory. Only includes files which have
        one of the provided extensions.

        Watchman watches actual source files, so when a change is detected to a
        file, this mapping can be used to identify what file changed from Pyre's
        perspective.
    """
    symbolic_links = {}
    try:
        for symbolic_link in find_paths_with_extensions(directory, extensions):
            symbolic_links[os.path.realpath(symbolic_link)] = symbolic_link
    except subprocess.CalledProcessError as error:
        LOG.warning(
            "Exception encountered trying to find source files "
            "in the analysis directory: `%s`",
            error,
        )
        LOG.warning("Starting with an empty set of tracked files.")
    return symbolic_links


def _delete_symbolic_link(link_path: str) -> None:
    os.unlink(link_path)


def add_symbolic_link(link_path: str, actual_path: str) -> None:
    directory = os.path.dirname(link_path)
    try:
        os.makedirs(directory)
    except OSError:
        pass
    try:
        os.symlink(actual_path, link_path)
    except OSError as error:
        if error.errno == errno.EEXIST:
            os.unlink(link_path)
            os.symlink(actual_path, link_path)
        else:
            LOG.error(str(error))


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


@contextmanager
def do_nothing() -> Generator[None, None, None]:
    yield


def acquire_lock_if_needed(
    lock_path: str, blocking: bool, needed: bool
) -> ContextManager[Optional[int]]:
    if needed:
        return acquire_lock(lock_path, blocking)
    else:
        return do_nothing()


class Filesystem:
    def list(
        self, root: str, patterns: List[str], exclude: Optional[List[str]] = None
    ) -> List[str]:
        """
            Return the list of files that match any of the patterns within root.
            If exclude is provided, files that match an exclude pattern are omitted.

            Note: The `find` command does not understand globs properly.
                e.g. 'a/*.py' will match 'a/b/c.py'
            For this reason, avoid calling this method with glob patterns.
        """

        command = ["find", "."]
        command += self._match_any(patterns)
        if exclude:
            command += ["-and", "!"]
            command += self._match_any(exclude)
        return (
            subprocess.run(command, stdout=subprocess.PIPE, cwd=root)
            .stdout.decode("utf-8")
            .split()
        )

    def _match_any(self, patterns: List[str]) -> List[str]:
        expression = []
        for pattern in patterns:
            if expression:
                expression.append("-or")
            expression.extend(["-path", "./{}".format(pattern)])
        return ["(", *expression, ")"]


class MercurialBackedFilesystem(Filesystem):
    def list(
        self, root: str, patterns: List[str], exclude: Optional[List[str]] = None
    ) -> List[str]:
        try:
            command = ["hg", "files"]
            for pattern in patterns:
                command += ["--include", pattern]
            if exclude:
                for pattern in exclude:
                    command += ["--exclude", pattern]
            return (
                subprocess.run(
                    command, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, cwd=root
                )
                .stdout.decode("utf-8")
                .split()
            )
        except FileNotFoundError:
            raise EnvironmentException("hg executable not found.")


@functools.lru_cache(1)
def get_filesystem() -> Filesystem:
    try:
        subprocess.check_output(["hg", "status"], stderr=subprocess.DEVNULL)
        return MercurialBackedFilesystem()
    except (subprocess.CalledProcessError, FileNotFoundError):
        return Filesystem()
