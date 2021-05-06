# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import enum
import functools
import logging
import os
import shutil
import subprocess
import sys
import tempfile
import textwrap
from itertools import chain
from pathlib import Path
from time import time
from typing import ContextManager, Dict, List, NamedTuple, Optional, Set, Tuple

from . import buck, json_rpc, log, statistics
from .buck import BuckBuilder, find_buck_root
from .configuration import Configuration, SearchPathElement, SimpleSearchPathElement
from .exceptions import EnvironmentException
from .filesystem import (
    _compute_symbolic_link_mapping,
    _delete_symbolic_link,
    acquire_lock,
    acquire_lock_if_needed,
    add_symbolic_link,
    do_nothing,
    find_python_paths,
    is_empty,
    is_parent,
    remove_if_exists,
    translate_path,
    translate_paths,
)
from .find_directories import CONFIGURATION_FILE, LOCAL_CONFIGURATION_FILE
from .socket_connection import SocketConnection, SocketException


LOG: logging.Logger = logging.getLogger(__name__)


# If there are a lot of tracked files that are updated at the same time, it is
# probably a rebase. So, rebuild just to be safe.
REBUILD_THRESHOLD_FOR_UPDATED_PATHS: int = 50


# If there are a lot of new or deleted files, it is probably a rebase.
# This is a separate threshold from the number of updated tracked files because
# we don't know for sure if these new files should actually be tracked. So, we
# might want a higher threshold in order to avoid rebuilding for spurious new
# files.
REBUILD_THRESHOLD_FOR_NEW_OR_DELETED_PATHS: int = 50


DONT_CARE_PROGRESS_VALUE = 1

BUCK_BUILDER_CACHE_PREFIX = ".buck_builder_cache"


READER_WRITER_LOCK = "analysis_directory_reader_writer.lock"

TEMPORARY_DIRECTORY_PREFIX = "pyre_tmp_"


class NotWithinLocalConfigurationException(Exception):
    pass


class BuckEvent(enum.Enum):
    BUILD = "build"
    REBUILD = "rebuild"
    PROCESS_NEW_PATHS = "process_new_paths"


def _resolve_filter_paths(
    source_directories: List[str],
    targets: List[str],
    configuration: Configuration,
    original_directory: str,
) -> Set[str]:
    filter_paths = set()
    if source_directories or targets:
        if source_directories:
            filter_paths.update(source_directories)
        if targets:
            filter_paths.update(
                [buck.presumed_target_root(target) for target in targets]
            )
    else:
        local_configuration_root = configuration.local_root
        if local_configuration_root:
            filter_paths = {local_configuration_root}
    return translate_paths(filter_paths, original_directory)


class UpdatedPaths(NamedTuple):
    updated_paths: List[str]
    deleted_paths: List[str]

    def is_empty(self) -> bool:
        return not self.updated_paths and not self.deleted_paths


class AnalysisDirectory:
    def __init__(
        self,
        path: SearchPathElement,
        filter_paths: Optional[Set[str]] = None,
        search_path: Optional[List[str]] = None,
    ) -> None:
        self._path = path
        self._filter_paths: Set[str] = filter_paths or set()
        self._search_path_directories: List[str] = self._get_search_path_directories(
            search_path or []
        )

    def get_command_line_root(self) -> str:
        return os.path.abspath(self._path.command_line_argument())

    def get_root(self) -> str:
        return self._path.path()

    def get_root_path(self) -> SearchPathElement:
        return self._path

    def get_filter_roots(self) -> Set[str]:
        current_project_directories = self._filter_paths or {self.get_root()}
        return {
            translate_path(os.getcwd(), filter_root)
            for filter_root in current_project_directories
        }

    def prepare(self) -> None:
        pass

    def compute_symbolic_links(self) -> Dict[str, str]:
        return {}

    def process_updated_files(self, paths: List[str]) -> UpdatedPaths:
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
        deleted_paths = [path for path in paths if not os.path.isfile(path)]
        tracked_paths = [
            path
            for path in paths
            if self._is_tracked(path) and path not in deleted_paths
        ]
        tracked_paths.extend(deleted_paths)
        return UpdatedPaths(updated_paths=tracked_paths, deleted_paths=deleted_paths)

    def cleanup(self, delete_long_lasting_files: bool) -> None:
        pass

    @staticmethod
    def _get_search_path_directories(search_path: List[str]) -> List[str]:
        return [os.path.abspath(os.path.join(*path.split("$"))) for path in search_path]

    @property
    @functools.lru_cache(1)
    def _tracked_directories(self) -> List[str]:
        return [os.path.abspath(self.get_root()), *self._search_path_directories]

    def _is_tracked(self, path: str) -> bool:
        return any(
            is_parent(directory, path) for directory in self._tracked_directories
        )

    def _is_in_search_path(self, path: str) -> bool:
        return any(
            is_parent(directory, path) for directory in self._search_path_directories
        )

    def acquire_shared_reader_lock(self) -> ContextManager[Optional[int]]:
        return do_nothing()

    def acquire_writer_lock(self) -> ContextManager[Optional[int]]:
        return do_nothing()


class SharedAnalysisDirectory(AnalysisDirectory):
    def __init__(
        self,
        source_directories: List[SearchPathElement],
        targets: List[str],
        project_root: str,
        original_directory: Optional[str] = None,
        filter_paths: Optional[Set[str]] = None,
        local_configuration_root: Optional[str] = None,
        extensions: Optional[List[str]] = None,
        search_path: Optional[List[str]] = None,
        isolate: bool = False,
        buck_builder: Optional[BuckBuilder] = None,
        configuration: Optional[Configuration] = None,
        temporary_directories: Optional[List[str]] = None,
    ) -> None:
        self._source_directories: Set[SearchPathElement] = set(source_directories)
        self._targets: List[str] = targets
        self._original_directory = original_directory
        self._project_root = project_root
        self._filter_paths: Set[str] = filter_paths or set()
        self._local_configuration_root = local_configuration_root
        self._extensions: Set[str] = set(extensions or []) | {"py", "pyi"}
        self._search_path_directories: List[str] = self._get_search_path_directories(
            search_path or []
        )
        self._isolate = isolate
        self._buck_builder: BuckBuilder = buck_builder or buck.SimpleBuckBuilder()
        self._temporary_directories: List[str] = temporary_directories or []

        # Mapping from source files in the project root to symbolic links in the
        # analysis directory.
        self._symbolic_links: Dict[str, str] = {}

        self._configuration = configuration
        self._last_singly_deleted_path_and_link: Optional[Tuple[str, str]] = None

    def get_scratch_directory(self) -> str:
        try:
            return os.path.realpath(
                subprocess.check_output(["mkscratch", "path", "--subdir", "pyre"])
                .decode("utf-8")
                .strip()
            )
        except Exception:
            return os.path.join(self._project_root, ".pyre")

    @functools.lru_cache(1)
    def get_command_line_root(self) -> str:
        path_to_root = self._local_configuration_root or "shared_analysis_directory"
        suffix = "_{}".format(str(os.getpid())) if self._isolate else ""
        return os.path.join(
            self.get_scratch_directory(), "{}{}".format(path_to_root, suffix)
        )

    @functools.lru_cache(1)
    def get_root(self) -> str:
        return self.get_command_line_root()

    def get_root_path(self) -> SearchPathElement:
        return SimpleSearchPathElement(self.get_root())

    def get_filter_roots(self) -> Set[str]:
        current_project_directories = self._filter_paths or {self._project_root}
        if len(self._targets) == 0:
            return {
                translate_path(os.getcwd(), filter_root)
                for filter_root in current_project_directories
            }
        else:
            buck_root = find_buck_root(self._project_root)
            if buck_root is None:
                raise EnvironmentException(
                    "Cannot find buck root when constructing filter directories"
                )
            return {
                translate_path(buck_root, filter_root)
                for filter_root in current_project_directories
            }

    def _log_build_event(
        self,
        event_type: BuckEvent,
        runtime: float,
        number_of_user_changed_files: int,
        number_of_updated_files: int,
        number_of_unsupported_files: int = 0,
    ) -> None:
        configuration = self._configuration
        if not configuration or not configuration.logger:
            return

        statistics.log_with_configuration(
            configuration=configuration,
            category=statistics.LoggerCategory.BUCK_EVENTS,
            integers={
                "runtime": int(runtime * 1000),
                "number_of_user_changed_files": number_of_user_changed_files,
                "number_of_updated_files": number_of_updated_files,
                "number_of_unsupported_files": number_of_unsupported_files,
            },
            normals={
                "command_line": " ".join(sys.argv),
                "event_type": event_type.value,
                "local_root": self._local_configuration_root,
                "buck_builder_type": str(self._buck_builder),
            },
        )

    # Exposed for testing.
    def _resolve_source_directories(self) -> Optional[buck.BuckBuildOutput]:
        buck_build_output = None
        if self._targets:
            buck_build_output = self._buck_builder.build(self._targets)
            new_source_directories = buck_build_output.output_directories
            original_directory = self._original_directory
            if original_directory is not None:
                new_source_directories = translate_paths(
                    # pyre-fixme[6]: Expected `Set[str]` for 1st positional only parameter
                    # to call `translate_paths` but got `typing.Iterable[str]`.
                    new_source_directories,
                    original_directory,
                )
            self._source_directories.update(
                (SimpleSearchPathElement(path) for path in new_source_directories)
            )

        if len(self._source_directories) == 0:
            message = """
            No targets or source directories to analyze.
            Did you mean to run the command within a Pyre project?
            (A Pyre project is a directory with a `.pyre_configuration.local` file.
            If it is in directory foo/, you can also run `pyre -l foo`.)
            See `pyre --help` for more details.
            """
            raise NotWithinLocalConfigurationException(textwrap.dedent(message).strip())
        return buck_build_output

    def prepare(self) -> None:
        start = time()
        root = self.get_root()
        LOG.info("Constructing shared directory `%s`", root)

        buck_build_output = self._resolve_source_directories()

        try:
            os.makedirs(root)
        except OSError:
            pass  # Swallow.

        lock = os.path.join(root, ".pyre.lock")
        with acquire_lock_if_needed(lock, blocking=True, needed=not self._isolate):
            self._clear()
            self._merge()
        self._symbolic_links.update(self.compute_symbolic_links())

        runtime = time() - start
        LOG.log(log.PERFORMANCE, "Merged analysis directories in %fs", runtime)
        self._log_build_event(
            BuckEvent.BUILD,
            runtime,
            number_of_user_changed_files=0,
            number_of_updated_files=len(self._symbolic_links),
            number_of_unsupported_files=len(buck_build_output.unsupported_files)
            if buck_build_output is not None
            else 0,
        )

    def rebuild(self) -> None:
        root = self.get_root()

        self._resolve_source_directories()

        with acquire_lock_if_needed(
            os.path.join(root, ".pyre.lock"), blocking=True, needed=not self._isolate
        ):
            all_paths = {}
            for source_directory in self._source_directories:
                self._merge_into_paths(source_directory.path(), all_paths)
            for relative_path, project_path in all_paths.items():
                scratch_path = os.path.join(root, relative_path)
                if os.path.realpath(scratch_path) != project_path:
                    add_symbolic_link(scratch_path, project_path)
            for scratch_path in self._symbolic_links.values():
                if not os.path.exists(scratch_path):
                    os.remove(scratch_path)
        self._symbolic_links = self.compute_symbolic_links()

    def compute_symbolic_links(self) -> Dict[str, str]:
        return _compute_symbolic_link_mapping(self.get_root(), self._extensions)

    @staticmethod
    def should_rebuild(
        updated_tracked_paths: List[str], new_paths: List[str], deleted_paths: List[str]
    ) -> bool:
        if any(
            Path(path).suffix == ".thrift" or Path(path).name == "TARGETS"
            for path in chain(updated_tracked_paths, new_paths, deleted_paths)
        ):
            return True
        return (
            len(updated_tracked_paths) >= REBUILD_THRESHOLD_FOR_UPDATED_PATHS
            or len(new_paths) + len(deleted_paths)
            >= REBUILD_THRESHOLD_FOR_NEW_OR_DELETED_PATHS
        )

    def _notify_about_rebuild(self, is_start_message: bool = True) -> None:
        configuration = self._configuration
        if configuration is None:
            return

        LOG.info(
            "Notifying server of %s of rebuild.", "start" if is_start_message else "end"
        )
        if is_start_message:
            message = (
                "Pyre is rebuilding because a significant number of files were "
                + "changed. Your results may be outdated until this is finished."
            )
            short_message = "Rebuilding..."
            message_type = json_rpc.LanguageServerMessageType.WARNING.value
        else:
            message = "Done rebuilding."
            short_message = "Done rebuilding."
            message_type = json_rpc.LanguageServerMessageType.INFORMATION.value
        show_status_message = json_rpc.Request(
            method="window/showStatus",
            parameters=json_rpc.ByNameParameters(
                {
                    "message": message,
                    "shortMessage": short_message,
                    "type": message_type,
                    "actions": [],
                    "progress": {
                        "numerator": DONT_CARE_PROGRESS_VALUE,
                        "denominator": DONT_CARE_PROGRESS_VALUE,
                    },
                }
            ),
        )
        try:
            with SocketConnection(configuration.log_directory) as socket_connection:
                socket_connection.perform_handshake(
                    configuration.get_version_hash_respecting_override()
                    or "unversioned"
                )
                socket_connection.send(show_status_message)
        except (
            SocketException,
            ResourceWarning,
            json_rpc.JSONRPCException,
        ) as exception:
            LOG.error("Error while communicating with server: %s", str(exception))

    def _cache_last_deleted_link(
        self,
        deleted_paths: List[str],
        deleted_scratch_paths: List[str],
        tracked_search_paths: List[str],
    ) -> None:
        """Cache the current deleted path and link if there is only one updated
        path and it is deleted. This will help avoid a costly buck query for
        Vim's behavior of deletion followed by addition."""
        if (
            len(deleted_scratch_paths) == 1
            and tracked_search_paths == deleted_scratch_paths
        ):
            self._last_singly_deleted_path_and_link = (
                deleted_paths[0],
                deleted_scratch_paths[0],
            )
            LOG.debug(
                "Caching singly-deleted file and link: %s",
                self._last_singly_deleted_path_and_link,
            )
        else:
            self._last_singly_deleted_path_and_link = None

    def _fetch_cached_absolute_link_map(
        self, new_paths: List[str], deleted_paths: List[str]
    ) -> Optional[Dict[str, str]]:
        """Fetch a cached absolute link map if it exists.

        Editing a file in Vim leads to a "deletion" and "addition" for the
        edited file. If the user calls `pyre` after we process the deletion but
        before we process the addition, then we get spurious "could not find the
        module" errors. So, cache the link map that the seconds-long `buck
        query` would have returned so that there is less chance of such a
        race."""
        last_singly_deleted_path_and_link = self._last_singly_deleted_path_and_link
        if (
            last_singly_deleted_path_and_link is not None
            and new_paths == [last_singly_deleted_path_and_link[0]]
            and deleted_paths == []
        ):
            return {
                last_singly_deleted_path_and_link[0]: last_singly_deleted_path_and_link[
                    1
                ]
            }
        else:
            return None

    def _process_rebuilt_files(
        self, tracked_paths: List[str], new_paths: List[str], deleted_paths: List[str]
    ) -> Tuple[List[str], List[str]]:
        self._notify_about_rebuild(is_start_message=True)

        old_symbolic_links = self._symbolic_links
        old_paths = set(self._symbolic_links.keys())
        number_of_user_changed_files = (
            len(tracked_paths) + len(new_paths) + len(deleted_paths)
        )

        # We need to inform the server of any files updated in place by the
        # rebuild, such as .pyi files from thrift.
        rebuild_start_time = time()
        LOG.info("Updating shared directory `%s`", self.get_root())

        self.rebuild()
        # pyre-fixme[35]: Target cannot be annotated.
        new_paths: Set[str] = set(self._symbolic_links.keys())

        buck.clear_buck_query_cache()
        self._notify_about_rebuild(is_start_message=False)

        newly_created_paths = new_paths - old_paths
        tracked_paths.extend(newly_created_paths)

        # Using the modified time instead of a Watchman `since` query because
        # these files will be in the buck builder cache or in /tmp, and Watchman
        # doesn't track those.
        updated_paths = [
            shared_analysis_path
            for shared_analysis_path, original_path in self._symbolic_links.items()
            if os.path.getmtime(original_path) > rebuild_start_time
        ]
        tracked_paths.extend(updated_paths)

        # Translate the paths here because we need the old symbolic links
        # mapping to get their old scratch path.
        deleted_scratch_paths = [
            old_symbolic_links.get(path, path) for path in old_paths - new_paths
        ]

        runtime = time() - rebuild_start_time
        LOG.log(log.PERFORMANCE, "Updated shared directory in %fs", runtime)
        number_of_updated_files = (
            len(updated_paths) + len(newly_created_paths) + len(deleted_scratch_paths)
        )
        self._log_build_event(
            BuckEvent.REBUILD,
            runtime,
            number_of_user_changed_files,
            number_of_updated_files,
        )

        return tracked_paths, deleted_scratch_paths

    def _process_new_paths(
        self, new_paths: List[str], tracked_paths: List[str], deleted_paths: List[str]
    ) -> List[str]:
        start_time = time()
        absolute_link_map = self._fetch_cached_absolute_link_map(
            new_paths, deleted_paths
        )
        if absolute_link_map is None:
            relative_link_map = {}
            try:
                configuration = self._configuration
                buck_mode = (
                    configuration.buck_mode if configuration is not None else None
                )
                isolation_prefix = (
                    configuration.isolation_prefix
                    if configuration is not None
                    else None
                )
                relative_link_map = buck.query_buck_relative_paths(
                    new_paths,
                    self._targets,
                    buck_mode=buck_mode,
                    isolation_prefix=isolation_prefix,
                )
            except buck.BuckException as error:
                LOG.error("Exception occurred when querying buck: %s", error)
                LOG.error("No new paths will be added to the analysis directory.")

            absolute_link_map = {
                path: os.path.join(self.get_root(), relative_link)
                for path, relative_link in relative_link_map.items()
            }
        tracked_paths.extend(absolute_link_map.keys())
        for path, absolute_link in absolute_link_map.items():
            try:
                add_symbolic_link(absolute_link, path)
                self._symbolic_links[path] = absolute_link
            except OSError:
                LOG.warning("Failed to add link at %s.", absolute_link)

        runtime = time() - start_time
        self._log_build_event(
            BuckEvent.PROCESS_NEW_PATHS,
            runtime,
            number_of_user_changed_files=len(new_paths),
            number_of_updated_files=len(absolute_link_map),
        )
        return tracked_paths

    def _process_deleted_paths(
        self, deleted_paths: List[str]
    ) -> Tuple[List[str], List[str]]:
        # Translate the paths here because we need the old symbolic links
        # mapping to get their old scratch path.
        deleted_scratch_paths = [self._symbolic_links[path] for path in deleted_paths]
        for path in deleted_paths:
            link = self._symbolic_links.pop(path, None)
            if link:
                try:
                    _delete_symbolic_link(link)
                except OSError:
                    LOG.warning("Failed to delete link at `%s`.", link)
        return deleted_paths, deleted_scratch_paths

    def _get_new_deleted_and_tracked_paths(
        self, paths: List[str]
    ) -> Tuple[List[str], List[str], List[str]]:
        deleted_paths = [
            path
            for path in paths
            if not os.path.isfile(path)
            and (path in self._symbolic_links or self._is_tracked(path))
        ]
        new_paths = [
            path
            for path in paths
            if path not in self._symbolic_links
            and not self._is_in_search_path(path)
            and os.path.isfile(path)
            and is_parent(self._project_root, path)
        ]
        tracked_paths = [
            path
            for path in paths
            if path not in new_paths
            and path not in deleted_paths
            and (path in self._symbolic_links or self._is_tracked(path))
        ]
        return new_paths, deleted_paths, tracked_paths

    def _process_updated_files(self, paths: List[str]) -> UpdatedPaths:
        """Update the analysis directory for any new or deleted files.
        Rebuild the directory using buck if needed.
        Return the updated and deleted paths."""
        deleted_scratch_paths = []
        (
            new_paths,
            deleted_paths,
            tracked_paths,
        ) = self._get_new_deleted_and_tracked_paths(paths)
        if SharedAnalysisDirectory.should_rebuild(
            tracked_paths, new_paths, deleted_paths
        ):
            tracked_paths, deleted_scratch_paths = self._process_rebuilt_files(
                tracked_paths, new_paths, deleted_paths
            )
        elif new_paths or deleted_paths:
            if new_paths:
                LOG.info("Detected new paths: %s.", ",".join(new_paths))
                tracked_paths = self._process_new_paths(
                    new_paths, tracked_paths, deleted_paths
                )
            if deleted_paths:
                LOG.info("Detected deleted paths: `%s`.", "`,`".join(deleted_paths))
                deleted_paths, deleted_scratch_paths = self._process_deleted_paths(
                    deleted_paths
                )

        tracked_scratch_paths = [
            self._symbolic_links.get(path, path) for path in tracked_paths
        ] + deleted_scratch_paths

        self._cache_last_deleted_link(
            deleted_paths, deleted_scratch_paths, tracked_scratch_paths
        )

        # The server expects scratch paths.
        return UpdatedPaths(
            updated_paths=tracked_scratch_paths, deleted_paths=deleted_scratch_paths
        )

    def process_updated_files(self, paths: List[str]) -> UpdatedPaths:
        with self.acquire_writer_lock():
            return self._process_updated_files(paths)

    def _directories_to_clean_up(self, delete_long_lasting_files: bool) -> List[str]:
        if not delete_long_lasting_files:
            return []

        if isinstance(self._buck_builder, buck.SimpleBuckBuilder):
            return [self.get_root()]

        if isinstance(self._buck_builder, buck.SourceDatabaseBuckBuilder):
            return [self.get_root(), *self._temporary_directories]

        project_name = _get_project_name(self._isolate, self._local_configuration_root)
        buck_builder_cache_path = os.path.join(
            self.get_scratch_directory(), f"{BUCK_BUILDER_CACHE_PREFIX}_{project_name}"
        )
        return [self.get_root(), buck_builder_cache_path, *self._temporary_directories]

    def cleanup(self, delete_long_lasting_files: bool) -> None:
        delete_long_lasting_files = delete_long_lasting_files or self._isolate
        directories_to_clean_up = self._directories_to_clean_up(
            delete_long_lasting_files
        )
        LOG.debug(f"Cleaning up in the analysis directory: {directories_to_clean_up}.")
        for directory in directories_to_clean_up:
            try:
                shutil.rmtree(directory)
            except Exception as exception:
                LOG.debug(f"Could not clean up analysis directory: {exception}")

    def _clear(self) -> None:
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
            self._merge_into_paths(source_directory.path(), all_paths)
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

    def _reader_writer_lock_path(self) -> str:
        return os.path.join(self.get_root(), READER_WRITER_LOCK)

    def acquire_shared_reader_lock(self) -> ContextManager[Optional[int]]:
        return acquire_lock(
            self._reader_writer_lock_path(), blocking=True, is_shared_reader=True
        )

    def acquire_writer_lock(self) -> ContextManager[Optional[int]]:
        return acquire_lock(
            self._reader_writer_lock_path(), blocking=True, is_shared_reader=False
        )


def _get_project_name(
    isolate_per_process: bool, relative_local_root: Optional[str]
) -> Optional[str]:
    if isolate_per_process:
        return f"isolated_{os.getpid()}"
    if relative_local_root:
        return "_".join(Path(relative_local_root).parts)
    return None


def _get_buck_builder(
    configuration: Configuration,
    relative_local_root: Optional[str],
    isolate: bool,
) -> Tuple[BuckBuilder, List[str]]:
    if not configuration.use_buck_builder:
        return (buck.SimpleBuckBuilder(), [])

    project_root = configuration.project_root
    buck_mode = configuration.buck_mode
    buck_root = find_buck_root(project_root)
    if not buck_root:
        raise EnvironmentException(
            f"No Buck configuration at `{project_root}` or any of its ancestors."
        )

    output_directory = tempfile.mkdtemp(prefix=TEMPORARY_DIRECTORY_PREFIX)

    if configuration.use_buck_source_database:
        return (
            buck.SourceDatabaseBuckBuilder(
                buck_root=buck_root,
                buck_mode=buck_mode,
                output_directory=output_directory,
                isolation_prefix=(
                    configuration.get_isolation_prefix_respecting_override()
                ),
            ),
            [output_directory],
        )

    project_name = _get_project_name(
        isolate_per_process=isolate, relative_local_root=relative_local_root
    )
    return (
        buck.FastBuckBuilder(
            buck_root=buck_root,
            buck_builder_binary=configuration.buck_builder_binary,
            buck_mode=buck_mode,
            project_name=project_name,
            output_directory=output_directory,
            isolation_prefix=configuration.get_isolation_prefix_respecting_override(),
        ),
        [output_directory],
    )


def resolve_analysis_directory(
    source_directories: List[str],
    targets: List[str],
    configuration: Configuration,
    original_directory: str,
    filter_directory: Optional[str],
    isolate: bool = False,
    relative_local_root: Optional[str] = None,
) -> AnalysisDirectory:
    # Generate filter directories based on command-line input.
    if filter_directory:
        filter_paths = {filter_directory}
    else:
        filter_paths = _resolve_filter_paths(
            source_directories, targets, configuration, original_directory
        )

    # Only read from the configuration if no explicit targets are passed in.
    if not source_directories and not targets:
        source_paths: List[SearchPathElement] = configuration.get_source_directories()
        targets = list(configuration.targets or [])
    else:
        source_paths: List[SearchPathElement] = [
            # TODO: support SubdirectorySearchPathElement here too?
            SimpleSearchPathElement(path)
            for path in source_directories
        ]
        targets = targets or []
        if targets:
            configuration_name = LOCAL_CONFIGURATION_FILE
            command = "pyre init --local"
        else:
            configuration_name = CONFIGURATION_FILE
            command = "pyre init"
        LOG.warning(
            "Setting up a `%s` with `%s` may reduce overhead.",
            configuration_name,
            command,
        )

    project_root = configuration.project_root
    local_configuration_root = configuration.local_root
    if local_configuration_root:
        local_configuration_root = os.path.relpath(
            local_configuration_root, project_root
        )

    if len(source_paths) == 1 and len(targets) == 0:
        analysis_directory = AnalysisDirectory(
            source_paths.pop(),
            filter_paths=filter_paths,
            search_path=[
                search_path.path()
                for search_path in configuration.get_existent_search_paths()
            ],
        )
    else:
        buck_builder, temporary_directories = _get_buck_builder(
            configuration, relative_local_root, isolate
        )

        analysis_directory = SharedAnalysisDirectory(
            source_directories=source_paths,
            targets=targets,
            buck_builder=buck_builder,
            original_directory=original_directory,
            project_root=project_root,
            filter_paths=filter_paths,
            local_configuration_root=local_configuration_root,
            extensions=configuration.get_valid_extension_suffixes(),
            search_path=[
                search_path.path()
                for search_path in configuration.get_existent_search_paths()
            ],
            isolate=isolate,
            configuration=configuration,
            temporary_directories=temporary_directories,
        )

    return analysis_directory
