# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Optional


TEXT: str = "text"
JSON: str = "json"


@dataclass(frozen=True)
class CommandArguments:
    local_configuration: Optional[str] = None
    version: bool = False
    debug: bool = False
    sequential: bool = False
    strict: bool = False
    additional_checks: List[str] = field(default_factory=list)
    show_error_traces: bool = False
    output: str = TEXT
    enable_profiling: bool = False
    enable_memory_profiling: bool = False
    noninteractive: bool = False
    logging_sections: Optional[str] = None
    log_identifier: Optional[str] = None
    logger: Optional[str] = None
    formatter: Optional[str] = None
    targets: List[str] = field(default_factory=list)
    use_buck_builder: Optional[bool] = None
    use_buck_source_database: Optional[bool] = None
    source_directories: List[str] = field(default_factory=list)
    filter_directory: Optional[str] = None
    buck_mode: Optional[str] = None
    no_saved_state: bool = False
    search_path: List[str] = field(default_factory=list)
    binary: Optional[str] = None
    buck_builder_binary: Optional[str] = None
    exclude: List[str] = field(default_factory=list)
    typeshed: Optional[str] = None
    save_initial_state_to: Optional[str] = None
    load_initial_state_from: Optional[str] = None
    changed_files_path: Optional[str] = None
    saved_state_project: Optional[str] = None
    dot_pyre_directory: Optional[Path] = None
    features: Optional[str] = None
    use_command_v2: Optional[bool] = None
    isolation_prefix: Optional[str] = None
    python_version: Optional[str] = None


@dataclass(frozen=True)
class StartArguments:
    changed_files_path: Optional[str] = None
    debug: bool = False
    enable_memory_profiling: bool = False
    enable_profiling: bool = False
    load_initial_state_from: Optional[str] = None
    log_identifier: Optional[str] = None
    logging_sections: Optional[str] = None
    no_saved_state: bool = False
    no_watchman: bool = False
    noninteractive: bool = False
    save_initial_state_to: Optional[str] = None
    saved_state_project: Optional[str] = None
    sequential: bool = False
    show_error_traces: bool = False
    store_type_check_resolution: bool = False
    terminal: bool = False
    wait_on_initialization: bool = False


@dataclass(frozen=True)
class IncrementalArguments:
    output: str = TEXT
    no_start: bool = False
    start_arguments: StartArguments = field(default_factory=StartArguments)


@dataclass(frozen=True)
class RageArguments:
    output: Optional[Path] = None
    server_log_count: Optional[int] = None
