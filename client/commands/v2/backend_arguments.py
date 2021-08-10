# Copyright (c) Facebook, Inc. and its affiliates.
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
from typing import Optional, Dict, Union, Sequence, Set, IO, Iterator, Any

from typing_extensions import Protocol

from ... import configuration as configuration_module, find_directories

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
    elements: Sequence[configuration_module.SearchPathElement] = dataclasses.field(
        default_factory=list
    )

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
class BuckSourcePath:
    source_root: Path
    artifact_root: Path
    checked_directory: Path
    targets: Sequence[str] = dataclasses.field(default_factory=list)
    mode: Optional[str] = None
    isolation_prefix: Optional[str] = None

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
            "source_root": str(self.source_root),
            "artifact_root": str(self.artifact_root),
        }

    def get_checked_directory_allowlist(self) -> Set[str]:
        return {str(self.checked_directory)}

    def cleanup(self) -> None:
        shutil.rmtree(str(self.artifact_root), ignore_errors=True)


SourcePath = Union[SimpleSourcePath, BuckSourcePath]


def find_watchman_root(base: Path) -> Optional[Path]:
    return find_directories.find_parent_directory_containing_file(
        base, ".watchmanconfig"
    )


def find_buck_root(base: Path) -> Optional[Path]:
    return find_directories.find_parent_directory_containing_file(base, ".buckconfig")


def get_source_path(
    configuration: configuration_module.Configuration, artifact_root_name: str
) -> SourcePath:
    source_directories = configuration.source_directories
    targets = configuration.targets

    if source_directories is not None and targets is None:
        elements: Sequence[
            configuration_module.SearchPathElement
        ] = configuration.get_source_directories()
        if len(elements) == 0:
            LOG.warning("Pyre did not find an existent source directory.")
        return SimpleSourcePath(elements)

    if targets is not None and source_directories is None:
        if len(targets) == 0:
            LOG.warning("Pyre did not find any targets to check.")

        search_base = Path(configuration.project_root)
        relative_local_root = configuration.relative_local_root
        if relative_local_root is not None:
            search_base = search_base / relative_local_root

        source_root = find_buck_root(search_base)
        if source_root is None:
            raise configuration_module.InvalidConfiguration(
                "Cannot find a buck root for the specified targets. "
                + "Make sure the project is covered by a `.buckconfig` file."
            )

        return BuckSourcePath(
            source_root=source_root,
            artifact_root=configuration.dot_pyre_directory / artifact_root_name,
            checked_directory=search_base,
            targets=targets,
            mode=configuration.buck_mode,
            isolation_prefix=configuration.isolation_prefix,
        )

    if source_directories is not None and targets is not None:
        raise configuration_module.InvalidConfiguration(
            "`source_directories` and `targets` are mutually exclusive"
        )

    raise configuration_module.InvalidConfiguration(
        "Cannot find any source files to analyze. "
        + "Either `source_directories` or `targets` must be specified."
    )


def get_source_path_for_server(
    configuration: configuration_module.Configuration,
) -> SourcePath:
    # We know that for each source root there could be at most one server alive.
    # Therefore artifact root name can be a fixed constant.
    artifact_root_name = SERVER_ARTIFACT_ROOT_NAME
    relative_local_root = configuration.relative_local_root
    if relative_local_root is not None:
        # Prevent artifact roots of different local projects from clashing with
        # each other.
        artifact_root_name = str(Path(artifact_root_name) / relative_local_root)
    return get_source_path(configuration, artifact_root_name)


def get_source_path_for_check(
    configuration: configuration_module.Configuration,
) -> SourcePath:
    # Artifact for one-off check command should not be a fixed constant, to prevent
    # concurrent check commands overwriting each other's artifacts. Here we use process
    # ID to isolate the artifact root of each individual check command.
    return get_source_path(configuration, str(os.getpid()))


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
