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
from time import time
from typing import Dict, Generator, Iterable, List, Optional, Set

from . import buck, log
from .exceptions import EnvironmentException


LOG = logging.getLogger(__name__)


def translate_path(root: str, path: str) -> str:
    if os.path.isabs(path):
        return path

    translated = os.path.join(root, path)
    if os.path.exists(translated):
        return os.path.realpath(translated)

    return path


def translate_paths(paths: Set[str], original_directory: str) -> Set[str]:
    current_directory = os.getcwd()
    if not original_directory.startswith(current_directory):
        return paths
    translation = os.path.relpath(original_directory, current_directory)
    if not translation:
        return paths
    return {translate_path(translation, path) for path in paths}


def find_root(original_directory: str, target_file: str) -> Optional[str]:
    current_directory = original_directory
    while current_directory != "/":
        absolute = os.path.join(current_directory, target_file)
        if os.path.isfile(absolute):
            return current_directory
        current_directory = os.path.dirname(current_directory)
    return None


def exists(path: str) -> str:
    if not os.path.isfile(path):
        raise ValueError("%s is not a valid file" % path)
    return path


class AnalysisDirectory:
    def __init__(self, path: str, filter_paths: Optional[List[str]] = None) -> None:
        self._path = path
        self._filter_paths = filter_paths

    def get_root(self) -> str:
        return self._path

    def get_filter_root(self) -> List[str]:
        return self._filter_paths or [self.get_root()]

    def prepare(self) -> None:
        pass

    def cleanup(self) -> None:
        pass


class SharedAnalysisDirectory(AnalysisDirectory):
    def __init__(
        self,
        source_directories: List[str],
        targets: List[str],
        original_directory: Optional[str] = None,
        filter_paths: Optional[List[str]] = None,
        local_configuration_root: Optional[str] = None,
        isolate: bool = False,
        build: bool = False,
        prompt: bool = False,
    ):
        self._source_directories = set(source_directories)
        self._targets = set(targets)
        self._original_directory = original_directory
        self._filter_paths = filter_paths
        self._local_configuration_root = local_configuration_root
        self._isolate = isolate
        self._build = build
        self._prompt = prompt

    def get_scratch_directory(self) -> str:
        try:
            return (
                subprocess.check_output(["scratch", "path", "--subdir", "pyre"])
                .decode("utf-8")
                .strip()
            )
        except Exception:
            return os.path.join(os.getcwd(), ".pyre")

    @functools.lru_cache(1)
    def get_root(self) -> str:
        path_to_root = self._local_configuration_root or "shared_analysis_directory"
        suffix = "_{}".format(str(os.getpid())) if self._isolate else ""
        return os.path.join(
            self.get_scratch_directory(), "{}{}".format(path_to_root, suffix)
        )

    # Exposed for testing.
    def _resolve_source_directories(self):
        if self._targets:
            new_source_directories = buck.generate_source_directories(
                self._targets, build=self._build, prompt=self._prompt
            )
            original_directory = self._original_directory
            if original_directory is not None:
                new_source_directories = translate_paths(
                    new_source_directories, original_directory
                )
            self._source_directories.update(new_source_directories)

        if len(self._source_directories) == 0:
            raise EnvironmentException("No targets or source directories to analyze.")

    def prepare(self) -> None:
        start = time()
        root = self.get_root()
        LOG.info("Constructing shared directory `%s`", root)

        self._resolve_source_directories()

        try:
            os.makedirs(root)
        except OSError:
            pass  # Swallow.

        lock = os.path.join(root, ".pyre.lock")
        with acquire_lock(lock, blocking=True):
            self._clear()
            self._merge()
            LOG.log(
                log.PERFORMANCE, "Merged analysis directories in %fs", time() - start
            )

    def cleanup(self) -> None:
        try:
            if self._isolate:
                shutil.rmtree(self.get_root())
        except Exception:
            pass

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
            self._merge_into_paths(source_directory, all_paths)
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

    # Exposed for testing.
    def _merge_into_paths(
        self, source_directory: str, all_paths: Dict[str, str]
    ) -> None:
        paths = _find_python_paths(root=source_directory)
        for path in paths:
            relative = os.path.relpath(path, source_directory)
            if not path:
                continue
            # don't bother stat'ing paths that are already in the analysis directory.
            if relative in all_paths:
                continue
            try:
                absolute = os.path.realpath(path)
                # Don't merge symlinked directories.
                if not os.path.isfile(absolute):
                    continue
                if relative.endswith("__init__.py") and is_empty(absolute):
                    # Don't let empty __init__.py files override legitimate files.
                    continue
                all_paths[relative] = absolute
            except FileNotFoundError:
                continue


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
    return output.split("\n")


def _find_python_paths(root: str) -> List[str]:
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


@contextmanager
def acquire_lock(path: str, blocking: bool) -> Generator[Optional[int], None, None]:
    """Raises an OSError if the lock can't be acquired"""
    try:
        with open(path, "w+") as lockfile:
            if not blocking:
                lock_command = fcntl.LOCK_EX | fcntl.LOCK_NB
            else:
                lock_command = fcntl.LOCK_EX

            fcntl.lockf(lockfile.fileno(), lock_command)
            yield lockfile.fileno()
            fcntl.lockf(lockfile.fileno(), fcntl.LOCK_UN)

    except FileNotFoundError:
        yield


class Filesystem:
    def list(self, root: str, pattern: str) -> List[str]:
        return (
            subprocess.run(
                ["find", root, "-name", "*{}".format(pattern)], stdout=subprocess.PIPE
            )
            .stdout.decode("utf-8")
            .split()
        )


class MercurialBackedFilesystem(Filesystem):
    def list(self, root: str, pattern: str) -> List[str]:
        try:
            return (
                subprocess.run(
                    ["hg", "files", "--include", "**{}".format(pattern)],
                    stdout=subprocess.PIPE,
                    stderr=subprocess.DEVNULL,
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
