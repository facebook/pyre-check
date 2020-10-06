# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import enum
import logging
from typing import Any, Dict, Optional, Sequence, Tuple, Union

from ... import command_arguments, commands, configuration


LOG: logging.Logger = logging.getLogger(__name__)


class MatchPolicy(enum.Enum):
    BASE_NAME = "base_name"
    FULL_PATH = "full_path"

    def __str__(self) -> str:
        return self.value


@dataclasses.dataclass(frozen=True)
class CriticalFile:
    policy: MatchPolicy
    path: str

    def serialize(self) -> Dict[str, str]:
        return {str(self.policy): self.path}


@dataclasses.dataclass(frozen=True)
class LoadSavedStateFromFile:
    shared_memory_path: str
    changed_files_path: Optional[str] = None

    def serialize(self) -> Tuple[str, Dict[str, str]]:
        return (
            "load_from_file",
            {
                "shared_memory_path": self.shared_memory_path,
                **(
                    {}
                    if self.changed_files_path is None
                    else {"changed_files_path": self.changed_files_path}
                ),
            },
        )


@dataclasses.dataclass(frozen=True)
class LoadSavedStateFromProject:
    project_name: str
    project_metadata: Optional[str] = None

    def serialize(self) -> Tuple[str, Dict[str, str]]:
        return (
            "load_from_project",
            {
                "project_name": self.project_name,
                **(
                    {}
                    if self.project_metadata is None
                    else {"project_metadata": self.project_metadata}
                ),
            },
        )


SavedStateAction = Union[LoadSavedStateFromFile, LoadSavedStateFromProject]


@dataclasses.dataclass(frozen=True)
class Arguments:
    """
    Data structure for configuration options the backend server can recognize.
    Need to keep in sync with `pyre/new_server/serverConfiguration.mli`
    """

    log_path: str
    global_root: str

    checked_directory_allowlist: Sequence[str] = dataclasses.field(default_factory=list)
    checked_directory_blocklist: Sequence[str] = dataclasses.field(default_factory=list)
    critical_files: Sequence[CriticalFile] = dataclasses.field(default_factory=list)
    debug: bool = False
    excludes: Sequence[str] = dataclasses.field(default_factory=list)
    extensions: Sequence[str] = dataclasses.field(default_factory=list)
    local_root: Optional[str] = None
    number_of_workers: int = 1
    parallel: bool = True
    saved_state_action: Optional[SavedStateAction] = None
    search_paths: Sequence[configuration.SearchPathElement] = dataclasses.field(
        default_factory=list
    )
    show_error_traces: bool = False
    source_paths: Sequence[str] = dataclasses.field(default_factory=list)
    store_type_check_resolution: bool = False
    strict: bool = False
    taint_models_path: Sequence[str] = dataclasses.field(default_factory=list)
    watchman_root: Optional[str] = None

    def serialize(self) -> Dict[str, Any]:
        return {
            "source_paths": self.source_paths,
            "search_paths": [
                element.command_line_argument() for element in self.search_paths
            ],
            "excludes": self.excludes,
            "checked_directory_allowlist": self.checked_directory_allowlist,
            "checked_directory_blocklist": self.checked_directory_blocklist,
            "extensions": self.extensions,
            "log_path": self.log_path,
            "global_root": self.global_root,
            **({} if self.local_root is None else {"local_root": self.local_root}),
            **(
                {}
                if self.watchman_root is None
                else {"watchman_root": self.watchman_root}
            ),
            "taint_model_paths": self.taint_models_path,
            "debug": self.debug,
            "strict": self.strict,
            "show_error_traces": self.show_error_traces,
            "critical_files": [
                critical_file.serialize() for critical_file in self.critical_files
            ],
            **(
                {}
                if self.saved_state_action is None
                else {"saved_state_action": self.saved_state_action.serialize()}
            ),
            "store_type_check_resolution": self.store_type_check_resolution,
            "parallel": self.parallel,
            "number_of_workers": self.number_of_workers,
        }


def run(
    configuration: configuration.Configuration,
    start_arguments: command_arguments.StartArguments,
) -> commands.ExitCode:
    LOG.warning("Not implemented yet")
    return commands.ExitCode.SUCCESS
