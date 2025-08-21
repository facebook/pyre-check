# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module defines shared logic for data sent to the Pyre backend
as a json arguments file when launching a new backend process.

Several commands define their own specific arguments types with extra
fields, but most of these contain a `base_arguments` field of type
backend_arguments.BaseArguments.
"""

import contextlib
import dataclasses
import json
import logging
import os
import shutil
import tempfile
from pathlib import Path
from typing import Any, Dict, IO, Iterator, List, Optional, Sequence, Set, Union

from typing_extensions import Protocol

from . import (
    configuration as configuration_module,
    find_directories,
    frontend_configuration,
    identifiers,
)
from .configuration import search_path

LOG: logging.Logger = logging.getLogger(__name__)

SERVER_ARTIFACT_ROOT_NAME: str = "link_trees"


@dataclasses.dataclass(frozen=True)
class RemoteLogging:
    logger: str
    identifier: str = ""

    @staticmethod
    def create(
        logger: Optional[str] = None, identifier: Optional[str] = None
    ) -> "Optional[RemoteLogging]":
        return (
            RemoteLogging(logger=logger, identifier=identifier or "")
            if logger is not None
            else None
        )

    def serialize(self) -> Dict[str, str]:
        return {"logger": self.logger, "identifier": self.identifier}


@dataclasses.dataclass(frozen=True)
class SimpleSourcePath:
    elements: Sequence[search_path.Element] = dataclasses.field(default_factory=list)

    def serialize(self) -> Dict[str, object]:
        return {
            "kind": "simple",
            "paths": [element.command_line_argument() for element in self.elements],
        }

    def get_checked_directory_allowlist(self) -> Set[str]:
        allow_list = set()
        for element in self.elements:
            if expected_element_path := element.path() is not None:
                allow_list.add(expected_element_path)
        return allow_list

    def cleanup(self) -> None:
        pass


@dataclasses.dataclass(frozen=True)
class WithUnwatchedDependencySourcePath:
    change_indicator_root: Path
    unwatched_dependency: configuration_module.UnwatchedDependency
    elements: Sequence[search_path.Element] = dataclasses.field(default_factory=list)

    def serialize(self) -> Dict[str, object]:
        return {
            "kind": "with_unwatched_dependency",
            "paths": [element.command_line_argument() for element in self.elements],
            "unwatched_dependency": {
                "change_indicator": {
                    "root": str(self.change_indicator_root),
                    "relative": self.unwatched_dependency.change_indicator,
                },
                "files": {
                    "root": self.unwatched_dependency.files.root,
                    "checksum_path": self.unwatched_dependency.files.checksum_path,
                },
            },
        }

    def get_checked_directory_allowlist(self) -> Set[str]:
        allow_list = set()
        for element in self.elements:
            if expected_element_path := element.path() is not None:
                allow_list.add(expected_element_path)
        return allow_list

    def cleanup(self) -> None:
        pass


@dataclasses.dataclass(frozen=True)
class BuckSourcePath:
    source_root: Path
    artifact_root: Path
    checked_directory: Path
    targets: Sequence[str] = dataclasses.field(default_factory=list)
    targets_fallback_sources: Optional[Sequence[search_path.Element]] = None
    mode: Optional[str] = None
    isolation_prefix: Optional[str] = None
    bxl_builder: Optional[str] = None
    use_buck2: bool = True
    kill_buck_after_build: bool = False
    number_of_threads: Optional[int] = None

    def serialize(self) -> Dict[str, object]:
        mode = self.mode
        isolation_prefix = self.isolation_prefix
        bxl_builder = self.bxl_builder
        targets_fallback_sources = self.targets_fallback_sources
        return {
            "kind": "buck",
            "targets": self.targets,
            **(
                {}
                if targets_fallback_sources is None
                else {
                    "targets_fallback_sources": [
                        element.command_line_argument()
                        for element in targets_fallback_sources
                    ],
                }
            ),
            **({} if mode is None else {"mode": mode}),
            **(
                {}
                if isolation_prefix is None
                else {"isolation_prefix": isolation_prefix}
            ),
            **({} if bxl_builder is None else {"bxl_builder": bxl_builder}),
            "use_buck2": self.use_buck2,
            "source_root": str(self.source_root),
            "artifact_root": str(self.artifact_root),
            "kill_buck_after_build": self.kill_buck_after_build,
            **(
                {}
                if self.number_of_threads is None
                else {"number_of_threads": self.number_of_threads}
            ),
        }

    def get_checked_directory_allowlist(self) -> Set[str]:
        return {str(self.checked_directory)}

    def cleanup(self) -> None:
        shutil.rmtree(str(self.artifact_root), ignore_errors=True)


SourcePath = Union[SimpleSourcePath, WithUnwatchedDependencySourcePath, BuckSourcePath]


@dataclasses.dataclass(frozen=True)
class BaseArguments:
    """
    Data structure for configuration options common to many backend commands.
    Need to keep in sync with `pyre/source/command/commandStartup.ml`
    """

    log_path: str
    global_root: str
    source_paths: SourcePath

    # A path will be checked if it is covered by the allows list *and*
    # it is not not covered by the denylist.
    #
    # These paths can be regular files, not just directories.
    checked_directory_allowlist: Sequence[str] = dataclasses.field(default_factory=list)
    checked_directory_blocklist: Sequence[str] = dataclasses.field(default_factory=list)

    debug: bool = False
    enable_readonly_analysis: Optional[bool] = None
    enable_strict_override_check: Optional[bool] = None
    enable_strict_any_check: Optional[bool] = None
    enable_unawaited_awaitable_analysis: Optional[bool] = None
    excludes: Sequence[str] = dataclasses.field(default_factory=list)
    extensions: Sequence[str] = dataclasses.field(default_factory=list)
    include_suppressed_errors: Optional[bool] = None
    relative_local_root: Optional[str] = None
    memory_profiling_output: Optional[Path] = None
    number_of_workers: int = 1
    parallel: bool = True
    profiling_output: Optional[Path] = None
    python_version: configuration_module.PythonVersion = (
        configuration_module.PythonVersion(major=3)
    )
    system_platform: Optional[str] = None
    shared_memory: configuration_module.SharedMemory = (
        configuration_module.SharedMemory()
    )
    remote_logging: Optional[RemoteLogging] = None
    search_paths: Sequence[search_path.Element] = dataclasses.field(
        default_factory=list
    )

    def get_local_root(self) -> Optional[str]:
        if self.relative_local_root is None:
            return None
        return os.path.join(self.global_root, self.relative_local_root)

    def serialize(self) -> Dict[str, Any]:
        local_root = self.get_local_root()
        return {
            "source_paths": self.source_paths.serialize(),
            "search_paths": [
                element.command_line_argument() for element in self.search_paths
            ],
            "excludes": self.excludes,
            "checked_directory_allowlist": self.checked_directory_allowlist,
            "checked_directory_blocklist": self.checked_directory_blocklist,
            **(
                {"enable_readonly_analysis": self.enable_readonly_analysis}
                if self.enable_readonly_analysis is not None
                else {}
            ),
            **(
                {"enable_strict_override_check": self.enable_strict_override_check}
                if self.enable_strict_override_check is not None
                else {}
            ),
            **(
                {"enable_strict_any_check": self.enable_strict_any_check}
                if self.enable_strict_any_check is not None
                else {}
            ),
            **(
                {
                    "enable_unawaited_awaitable_analysis": (
                        self.enable_unawaited_awaitable_analysis
                    )
                }
                if self.enable_unawaited_awaitable_analysis is not None
                else {}
            ),
            "extensions": self.extensions,
            **(
                {"include_suppressed_errors": (self.include_suppressed_errors)}
                if self.include_suppressed_errors is not None
                else {}
            ),
            "log_path": self.log_path,
            "global_root": self.global_root,
            **({} if local_root is None else {"local_root": local_root}),
            "debug": self.debug,
            "python_version": {
                "major": self.python_version.major,
                "minor": self.python_version.minor,
                "micro": self.python_version.micro,
            },
            **(
                {}
                if self.system_platform is None
                else {"system_platform": self.system_platform}
            ),
            "shared_memory": self.shared_memory.to_json(),
            "parallel": self.parallel,
            "number_of_workers": self.number_of_workers,
            **(
                {}
                if self.remote_logging is None
                else {"remote_logging": self.remote_logging.serialize()}
            ),
            **(
                {}
                if self.profiling_output is None
                else {"profiling_output": str(self.profiling_output)}
            ),
            **(
                {}
                if self.memory_profiling_output is None
                else {"memory_profiling_output": str(self.memory_profiling_output)}
            ),
        }


def find_watchman_root(
    base: Path,
    stop_search_after: Optional[int] = None,
) -> Optional[Path]:
    return find_directories.find_parent_directory_containing_file(
        base, ".watchmanconfig", stop_search_after
    )


def find_buck_root(
    base: Path,
    stop_search_after: Optional[int] = None,
) -> Optional[Path]:
    return find_directories.find_parent_directory_containing_file(
        base, ".buckconfig", stop_search_after
    )


def find_buck2_root(
    base: Path,
    stop_search_after: Optional[int] = None,
) -> Optional[Path]:
    # Buck2 uses project root instead of cell root as its base directory.
    # This is essentially what `buck2 root --kind project` does.
    return find_directories.find_outermost_directory_containing_file(
        base, ".buckconfig", stop_search_after
    )


def _get_global_or_local_root(
    configuration: frontend_configuration.Base,
) -> Path:
    global_root = configuration.get_global_root()
    relative_local_root = configuration.get_relative_local_root()
    return (
        (global_root / relative_local_root)
        if relative_local_root is not None
        else global_root
    )


def get_source_path(
    configuration: frontend_configuration.Base,
    artifact_root_name: str,
    flavor: identifiers.PyreFlavor,
    kill_buck_after_build: bool,
    number_of_buck_threads: Optional[int],
    watchman_root: Optional[Path],
) -> SourcePath:
    source_directories = configuration.is_source_directories_defined()
    targets = configuration.get_buck_targets()
    buck_mode = configuration.get_buck_mode()

    if source_directories and targets is None:
        elements: Sequence[search_path.Element] = (
            configuration.get_existent_source_directories()
        )
        if len(elements) == 0:
            LOG.warning("Pyre did not find an existent source directory.")

        unwatched_dependency = configuration.get_existent_unwatched_dependency()
        if unwatched_dependency is not None and watchman_root is not None:
            return WithUnwatchedDependencySourcePath(
                change_indicator_root=watchman_root,
                unwatched_dependency=unwatched_dependency,
                elements=elements,
            )
        else:
            return SimpleSourcePath(elements)

    if (
        source_directories
        and targets is not None
        and flavor == identifiers.PyreFlavor.CLASSIC
    ):
        raise configuration_module.InvalidConfiguration(
            "`source_directories` and `targets` are mutually exclusive for typechecking"
        )

    if targets is not None:
        use_buck2 = configuration.uses_buck2()
        search_base = _get_global_or_local_root(configuration)
        source_root = (
            find_buck2_root(search_base) if use_buck2 else find_buck_root(search_base)
        )
        if source_root is None:
            raise configuration_module.InvalidConfiguration(
                "Cannot find a buck root for the specified targets. "
                + "Make sure the project is covered by a `.buckconfig` file."
            )
        source_directories = configuration.get_existent_source_directories()
        return BuckSourcePath(
            source_root=source_root,
            artifact_root=configuration.get_dot_pyre_directory() / artifact_root_name,
            checked_directory=search_base,
            targets=targets,
            targets_fallback_sources=(
                None if len(source_directories) == 0 else source_directories
            ),
            mode=buck_mode,
            isolation_prefix=configuration.get_buck_isolation_prefix(),
            bxl_builder=configuration.get_buck_bxl_builder(),
            use_buck2=use_buck2,
            kill_buck_after_build=kill_buck_after_build,
            number_of_threads=number_of_buck_threads,
        )

    raise configuration_module.InvalidConfiguration(
        "Cannot find any source files to analyze. "
        + "Either `source_directories` or `targets` must be specified."
    )


def get_source_path_for_server(
    configuration: frontend_configuration.Base,
    flavor: identifiers.PyreFlavor,
    kill_buck_after_build: bool,
    number_of_buck_threads: Optional[int],
    watchman_root: Optional[Path] = None,
) -> SourcePath:
    # We know that for each source root there could be at most one server alive.
    # Therefore artifact root name can be a fixed constant.
    artifact_root_name = f"{SERVER_ARTIFACT_ROOT_NAME}{flavor.path_suffix()}"
    relative_local_root = configuration.get_relative_local_root()
    if relative_local_root is not None:
        # Prevent artifact roots of different local projects from clashing with
        # each other.
        artifact_root_name = str(Path(artifact_root_name) / relative_local_root)
    return get_source_path(
        configuration,
        artifact_root_name,
        flavor,
        kill_buck_after_build,
        number_of_buck_threads,
        watchman_root,
    )


def get_source_path_for_check(
    configuration: frontend_configuration.Base,
    kill_buck_after_build: bool,
    number_of_buck_threads: Optional[int],
) -> SourcePath:
    # Artifact for one-off check command should not be a fixed constant, to prevent
    # concurrent check commands overwriting each other's artifacts. Here we use process
    # ID to isolate the artifact root of each individual check command.
    return get_source_path(
        configuration,
        str(os.getpid()),
        identifiers.PyreFlavor.CLASSIC,
        kill_buck_after_build,
        number_of_buck_threads,
        watchman_root=None,
    )


def get_checked_directory_allowlist(
    configuration: frontend_configuration.Base, source_path: SourcePath
) -> List[str]:
    source_path_allowlist = list(source_path.get_checked_directory_allowlist())
    explicit_allowlist = list(configuration.get_only_check_paths())
    # If allowlist paths were specifically provided, do not include inferred paths.
    return explicit_allowlist or source_path_allowlist


def get_profiling_log_path(log_directory: Path) -> Path:
    return log_directory / "profiling.log"


class SerializableArguments(Protocol):
    def serialize(self) -> Dict[str, Any]: ...


def _write_argument_file(
    output_file: IO[str], arguments: SerializableArguments
) -> None:
    LOG.info(f"Writing arguments into {output_file.name}...")
    serialized_arguments = arguments.serialize()
    LOG.debug(f"Arguments:\n{json.dumps(serialized_arguments, indent=2)}")
    output_file.write(json.dumps(serialized_arguments))
    output_file.flush()


@contextlib.contextmanager
def temporary_argument_file(arguments: SerializableArguments) -> Iterator[Path]:
    with tempfile.NamedTemporaryFile(
        mode="w", prefix="pyre_arguments_", suffix=".json"
    ) as argument_file:
        _write_argument_file(argument_file, arguments)
        yield Path(argument_file.name)


@dataclasses.dataclass
class LogFile:
    name: str
    file: IO[str]


@contextlib.contextmanager
def backend_log_file(prefix: str) -> Iterator[LogFile]:
    with tempfile.NamedTemporaryFile(
        mode="w", prefix=prefix, suffix=".log", delete=True
    ) as argument_file:
        yield LogFile(name=argument_file.name, file=argument_file.file)
