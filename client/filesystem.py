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


class BuckBuilder:
    def build(self, targets: Iterable[str]) -> Iterable[str]:
        """
            Build the given targets, and return a list of output directories
            containing the target output.
        """
        raise NotImplementedError


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
    current_directory = os.path.abspath(original_directory)
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


def is_parent(parent: str, child: str) -> bool:
    return child.startswith(parent.rstrip(os.sep) + os.sep)


def make_pyre_directory() -> str:
    pyre_directory = ".pyre"
    try:
        os.mkdir(pyre_directory)
    except FileExistsError:
        pass
    return pyre_directory


class AnalysisDirectory:
    def __init__(
        self,
        path: str,
        filter_paths: Optional[List[str]] = None,
        search_path: Optional[List[str]] = None,
    ) -> None:
        self._path = path
        self._filter_paths = filter_paths or []
        self._search_path = search_path or []

    def get_root(self) -> str:
        return self._path

    def get_filter_root(self) -> List[str]:
        return self._filter_paths or [self.get_root()]

    def prepare(self) -> None:
        pass

    def process_updated_files(self, paths: List[str]) -> List[str]:
        """
            Process a list of paths which were added/removed/updated, making any
            necessary changes to the directory:
                - For an AnalysisDirectory, nothing needs to be changed, since
                  the mapping from source file to analysis file is 1:1.
                - For a SharedAnalysisDirectory, the symbolic links (as well as
                  the reverse-mapping we track) need to be updated to account for
                  new and deleted files.

            Return a list of files (corresponding to the given paths) that Pyre
            should be tracking.
        """
        return [path for path in paths if self._is_tracked(path)]

    def cleanup(self) -> None:
        pass

    @property
    @functools.lru_cache(1)
    def _tracked_directories(self) -> List[str]:
        tracked_directories = [
            self.get_root(),
            *[os.path.join(*path.split("$")) for path in self._search_path],
        ]
        return [os.path.abspath(path) for path in tracked_directories]

    def _is_tracked(self, path: str) -> bool:
        return any(
            is_parent(directory, path) for directory in self._tracked_directories
        )


class SharedAnalysisDirectory(AnalysisDirectory):
    def __init__(
        self,
        source_directories: List[str],
        targets: List[str],
        original_directory: Optional[str] = None,
        filter_paths: Optional[List[str]] = None,
        local_configuration_root: Optional[str] = None,
        extensions: Optional[List[str]] = None,
        search_path: Optional[List[str]] = None,
        isolate: bool = False,
        buck_builder: Optional[BuckBuilder] = None,
    ):
        self._source_directories = set(source_directories)
        self._targets = set(targets)
        self._original_directory = original_directory
        self._filter_paths = filter_paths or []
        self._local_configuration_root = local_configuration_root
        self._extensions = set(extensions or []) | {"py", "pyi"}
        self._search_path = search_path or []
        self._isolate = isolate
        self._buck_builder = buck_builder or buck.SimpleBuckBuilder()

        # Mapping from source files in the project root to symbolic links in the
        # analysis directory.
        self._symbolic_links = {}  # type: Dict[str, str]

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

    def get_filter_root(self) -> List[str]:
        return self._filter_paths or [os.getcwd()]

    # Exposed for testing.
    def _resolve_source_directories(self):
        if self._targets:
            new_source_directories = self._buck_builder.build(self._targets)
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
        self._symbolic_links.update(
            _compute_symbolic_link_mapping(self.get_root(), self._extensions)
        )

    def process_updated_files(self, paths: List[str]) -> List[str]:
        """
            Return the paths in the analysis directory (symbolic links)
            corresponding to the given paths.
            Result also includes any files which are within a tracked directory.

            This method will remove/add symbolic links for deleted/new files.
        """
        tracked_files = []

        deleted_paths = [path for path in paths if not os.path.isfile(path)]
        # TODO(T40580762) use buck targets to properly identify what new files belong
        # to the project rather than checking if they are within the current directory
        new_paths = [
            path
            for path in paths
            if path not in self._symbolic_links
            and os.path.isfile(path)
            and is_parent(os.getcwd(), path)
        ]
        updated_paths = [
            path
            for path in paths
            if path not in deleted_paths and path not in new_paths
        ]

        if deleted_paths:
            LOG.info("Detected deleted paths: `%s`.", "`,`".join(deleted_paths))
        for path in deleted_paths:
            link = self._symbolic_links.pop(path, None)
            if link:
                try:
                    _delete_symbolic_link(link)
                    tracked_files.append(link)
                except OSError:
                    LOG.warning("Failed to delete link at `%s`.", link)

        if new_paths:
            LOG.info("Detected new paths: %s.", ",".join(new_paths))
            try:
                for path, relative_link in buck.resolve_relative_paths(
                    new_paths
                ).items():
                    link = os.path.join(self.get_root(), relative_link)
                    try:
                        add_symbolic_link(link, path)
                        self._symbolic_links[path] = link
                        tracked_files.append(link)
                    except OSError:
                        LOG.warning("Failed to add link at %s.", link)
            except buck.BuckException as error:
                LOG.error("Exception occurred when querying buck: %s", error)
                LOG.error("No new paths will be added to the analysis directory.")

        for path in updated_paths:
            if path in self._symbolic_links:
                tracked_files.append(self._symbolic_links[path])
            elif self._is_tracked(path):
                tracked_files.append(path)

        return tracked_files

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
            add_symbolic_link(merged, original)

    # Exposed for testing.
    def _merge_into_paths(
        self, source_directory: str, all_paths: Dict[str, str]
    ) -> None:
        paths = find_python_paths(root=source_directory)
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
