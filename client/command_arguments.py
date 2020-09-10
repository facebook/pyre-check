# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional


@dataclass(frozen=True)
class CommandArguments:
    local_configuration: Optional[str]
    version: bool
    debug: bool
    sequential: bool
    strict: bool
    additional_checks: List[str]
    show_error_traces: bool
    output: str
    enable_profiling: bool
    enable_memory_profiling: bool
    noninteractive: bool
    logging_sections: Optional[str]
    log_identifier: str
    logger: Optional[str]
    formatter: Optional[str]
    targets: List[str]
    use_buck_builder: Optional[bool]
    use_buck_source_database: Optional[bool]
    source_directories: List[str]
    filter_directory: Optional[str]
    buck_mode: Optional[str]
    no_saved_state: bool
    search_path: List[str]
    binary: str
    buck_builder_binary: Optional[str]
    exclude: List[str]
    typeshed: str
    save_initial_state_to: Optional[str]
    load_initial_state_from: Optional[str]
    changed_files_path: Optional[str]
    saved_state_project: Optional[str]
    dot_pyre_directory: Optional[Path]
    features: Optional[str]

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
