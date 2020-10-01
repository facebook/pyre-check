# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
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

    @staticmethod
    def from_arguments(arguments: argparse.Namespace) -> "CommandArguments":
        return CommandArguments(
            local_configuration=arguments.local_configuration,
            version=arguments.version,
            debug=arguments.debug,
            sequential=arguments.sequential,
            strict=arguments.strict,
            additional_checks=arguments.additional_check,
            show_error_traces=arguments.show_error_traces,
            output=arguments.output,
            enable_profiling=arguments.enable_profiling,
            enable_memory_profiling=arguments.enable_memory_profiling,
            noninteractive=arguments.noninteractive,
            logging_sections=arguments.logging_sections,
            log_identifier=arguments.log_identifier,
            logger=arguments.logger,
            formatter=arguments.formatter,
            targets=arguments.targets,
            use_buck_builder=arguments.use_buck_builder,
            use_buck_source_database=arguments.use_buck_source_database,
            source_directories=arguments.source_directories,
            filter_directory=arguments.filter_directory,
            buck_mode=arguments.buck_mode,
            no_saved_state=arguments.no_saved_state,
            search_path=arguments.search_path,
            binary=arguments.binary,
            buck_builder_binary=arguments.buck_builder_binary,
            exclude=arguments.exclude,
            typeshed=arguments.typeshed,
            save_initial_state_to=arguments.save_initial_state_to,
            load_initial_state_from=arguments.load_initial_state_from,
            changed_files_path=arguments.changed_files_path,
            saved_state_project=arguments.saved_state_project,
            dot_pyre_directory=arguments.dot_pyre_directory,
            features=arguments.features,
        )
