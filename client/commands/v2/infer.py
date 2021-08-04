# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import enum
import logging
import os
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence

from ... import commands, command_arguments, configuration as configuration_module
from . import remote_logging, backend_arguments

LOG: logging.Logger = logging.getLogger(__name__)


class InferMode(enum.Enum):
    LOCAL: str = "Local"
    INTERPROCEDURAL: str = "Interprocedural"

    def serialize(self) -> List[str]:
        return [self.value]


@dataclasses.dataclass(frozen=True)
class Arguments:
    """
    Data structure for configuration options the backend check command can recognize.
    Need to keep in sync with `pyre/command/newInferCommand.ml`
    """

    log_path: str
    global_root: str
    source_paths: backend_arguments.SourcePath

    checked_directory_allowlist: Sequence[str] = dataclasses.field(default_factory=list)
    checked_directory_blocklist: Sequence[str] = dataclasses.field(default_factory=list)
    debug: bool = False
    excludes: Sequence[str] = dataclasses.field(default_factory=list)
    extensions: Sequence[str] = dataclasses.field(default_factory=list)
    ignore_infer: Sequence[str] = dataclasses.field(default_factory=list)
    infer_mode: InferMode = InferMode.LOCAL
    memory_profiling_output: Optional[Path] = None
    number_of_workers: int = 1
    parallel: bool = True
    profiling_output: Optional[Path] = None
    python_version: configuration_module.PythonVersion = (
        configuration_module.PythonVersion(major=3)
    )
    relative_local_root: Optional[str] = None
    shared_memory: configuration_module.SharedMemory = (
        configuration_module.SharedMemory()
    )
    remote_logging: Optional[backend_arguments.RemoteLogging] = None
    search_paths: Sequence[configuration_module.SearchPathElement] = dataclasses.field(
        default_factory=list
    )

    @property
    def local_root(self) -> Optional[str]:
        if self.relative_local_root is None:
            return None
        return os.path.join(self.global_root, self.relative_local_root)

    def serialize(self) -> Dict[str, Any]:
        local_root = self.local_root
        return {
            "source_paths": self.source_paths.serialize(),
            "search_paths": [
                element.command_line_argument() for element in self.search_paths
            ],
            "excludes": self.excludes,
            "checked_directory_allowlist": self.checked_directory_allowlist,
            "checked_directory_blocklist": self.checked_directory_blocklist,
            "extensions": self.extensions,
            "ignore_infer": self.ignore_infer,
            "infer_mode": self.infer_mode.serialize(),
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


def _check_arguments(infer_arguments: command_arguments.InferArguments) -> None:
    if (
        infer_arguments.annotate_from_existing_stubs
        and infer_arguments.paths_to_modify is None
    ):
        raise ValueError(
            "`--annotate-from-existing-stubs` cannot be used without the"
            " `--in-place` flag"
        )


def _check_working_directory(
    working_directory: Path, global_root: Path, relative_local_root: Optional[str]
) -> None:
    candidate_locations: List[str] = []
    if working_directory == global_root:
        return
    candidate_locations.append(f"`{global_root}` with `--local-configuration` set")

    if relative_local_root is not None:
        local_root = global_root / relative_local_root
        if working_directory == local_root:
            return
        candidate_locations.append(f"`{local_root}`")

    valid_locations = " or from ".join(candidate_locations)
    raise ValueError(
        f"Infer must run from {valid_locations}. "
        f"Cannot run from current working directory `{working_directory}`."
    )


def run_infer(
    configuration: configuration_module.Configuration,
    infer_arguments: command_arguments.InferArguments,
) -> commands.ExitCode:
    working_directory = Path.cwd()
    _check_arguments(infer_arguments)
    _check_working_directory(
        working_directory=working_directory,
        global_root=Path(configuration.project_root),
        relative_local_root=configuration.relative_local_root,
    )
    LOG.warning("Coming soon...")
    return commands.ExitCode.SUCCESS


@remote_logging.log_usage(command_name="infer")
def run(
    configuration: configuration_module.Configuration,
    infer_arguments: command_arguments.InferArguments,
) -> commands.ExitCode:
    try:
        return run_infer(configuration, infer_arguments)
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during Pyre infer: {error}"
        ) from error
