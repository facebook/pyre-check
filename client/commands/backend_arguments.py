# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

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

from .. import configuration as configuration_module, find_directories
from ..configuration import search_path
from . import frontend_configuration

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
        return {element.path() for element in self.elements}

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
        return {element.path() for element in self.elements}

    def cleanup(self) -> None:
        pass


@dataclasses.dataclass(frozen=True)
class BuckSourcePath:
    source_root: Path
    artifact_root: Path
    checked_directory: Path
    targets: Sequence[str] = dataclasses.field(default_factory=list)
    mode: Optional[str] = None
    isolation_prefix: Optional[str] = None
    use_buck2: bool = False

    def serialize(self) -> Dict[str, object]:
        mode = self.mode
        isolation_prefix = self.isolation_prefix
        return {
            "kind": "buck",
            "targets": self.targets,
            **({} if mode is None else {"mode": mode}),
            **(
                {}
                if isolation_prefix is None
                else {"isolation_prefix": isolation_prefix}
            ),
            "use_buck2": self.use_buck2,
            "source_root": str(self.source_root),
            "artifact_root": str(self.artifact_root),
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
    Need to keep in sync with `pyre/command/newCommandStartup.ml`
    """

    log_path: str
    global_root: str
    source_paths: SourcePath

    checked_directory_allowlist: Sequence[str] = dataclasses.field(default_factory=list)
    checked_directory_blocklist: Sequence[str] = dataclasses.field(default_factory=list)
    debug: bool = False
    excludes: Sequence[str] = dataclasses.field(default_factory=list)
    extensions: Sequence[str] = dataclasses.field(default_factory=list)
    relative_local_root: Optional[str] = None
    memory_profiling_output: Optional[Path] = None
    number_of_workers: int = 1
    parallel: bool = True
    profiling_output: Optional[Path] = None
    python_version: configuration_module.PythonVersion = (
        configuration_module.PythonVersion(major=3)
    )
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
            "extensions": self.extensions,
            "log_path": self.log_path,
            "global_root": self.global_root,
            **({} if local_root is None else {"local_root": local_root}),
            "debug": self.debug,
            "python_version": {
                "major": self.python_version.major,
                "minor": self.python_version.minor,
                "micro": self.python_version.micro,
            },
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
    configuration: frontend_configuration.Base, artifact_root_name: str
) -> SourcePath:
    source_directories = configuration.is_source_directories_defined()
    targets = configuration.get_buck_targets()
    buck_mode = configuration.get_buck_mode()

    if source_directories and targets is None:
        elements: Sequence[
            search_path.Element
        ] = configuration.get_existent_source_directories()
        if len(elements) == 0:
            LOG.warning("Pyre did not find an existent source directory.")

        unwatched_dependency = configuration.get_existent_unwatched_dependency()
        if unwatched_dependency is not None:
            return WithUnwatchedDependencySourcePath(
                change_indicator_root=_get_global_or_local_root(configuration),
                unwatched_dependency=unwatched_dependency,
                elements=elements,
            )
        else:
            return SimpleSourcePath(elements)

    if targets is not None and not source_directories:
        if len(targets) == 0:
            LOG.warning("Pyre did not find any targets to check.")

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

        return BuckSourcePath(
            source_root=source_root,
            artifact_root=configuration.get_dot_pyre_directory() / artifact_root_name,
            checked_directory=search_base,
            targets=targets,
            mode=buck_mode,
            isolation_prefix=configuration.get_buck_isolation_prefix(),
            use_buck2=use_buck2,
        )

    if source_directories and targets is not None:
        raise configuration_module.InvalidConfiguration(
            "`source_directories` and `targets` are mutually exclusive"
        )

    raise configuration_module.InvalidConfiguration(
        "Cannot find any source files to analyze. "
        + "Either `source_directories` or `targets` must be specified."
    )


def get_source_path_for_server(
    configuration: frontend_configuration.Base,
) -> SourcePath:
    # We know that for each source root there could be at most one server alive.
    # Therefore artifact root name can be a fixed constant.
    artifact_root_name = SERVER_ARTIFACT_ROOT_NAME
    relative_local_root = configuration.get_relative_local_root()
    if relative_local_root is not None:
        # Prevent artifact roots of different local projects from clashing with
        # each other.
        artifact_root_name = str(Path(artifact_root_name) / relative_local_root)
    return get_source_path(configuration, artifact_root_name)


def get_source_path_for_check(
    configuration: frontend_configuration.Base,
) -> SourcePath:
    # Artifact for one-off check command should not be a fixed constant, to prevent
    # concurrent check commands overwriting each other's artifacts. Here we use process
    # ID to isolate the artifact root of each individual check command.
    return get_source_path(configuration, str(os.getpid()))


def get_checked_directory_allowlist(
    configuration: frontend_configuration.Base, source_path: SourcePath
) -> List[str]:
    source_path_allowlist = list(source_path.get_checked_directory_allowlist())
    explicit_allowlist = list(configuration.get_do_not_ignore_errors_in())
    # If allowlist paths were specifically provided, do not include inferred paths.
    return explicit_allowlist or source_path_allowlist


def get_profiling_log_path(log_directory: Path) -> Path:
    return log_directory / "profiling.log"


class SerializableArguments(Protocol):
    def serialize(self) -> Dict[str, Any]:
        ...


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
