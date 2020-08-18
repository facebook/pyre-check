# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from pathlib import Path
from typing import Optional
from unittest.mock import MagicMock

from ..analysis_directory import AnalysisDirectory
from ..commands.command import TEXT, CommandArguments, IncrementalStyle
from ..commands.incremental import Incremental


def mock_arguments(
    debug=False,
    changed_files_path=None,
    enable_profiling=False,
    enable_memory_profiling=False,
    features=None,
    hide_parse_errors=False,
    load_initial_state_from=None,
    local_configuration=None,
    log_identifier="",
    no_saved_state=False,
    output=TEXT,
    save_initial_state_to=None,
    saved_state_project=None,
    sequential=False,
    source_directories=None,
    targets=None,
    dot_pyre_directory: Optional[Path] = None,
) -> CommandArguments:
    return CommandArguments(
        local_configuration=local_configuration,
        version=False,
        debug=debug,
        sequential=sequential,
        strict=False,
        additional_checks=[],
        show_error_traces=False,
        output=output,
        enable_profiling=enable_profiling,
        enable_memory_profiling=enable_memory_profiling,
        noninteractive=False,
        hide_parse_errors=hide_parse_errors,
        logging_sections=None,
        log_identifier=log_identifier,
        logger=None,
        formatter=[],
        targets=targets or [],
        use_buck_builder=False,
        use_buck_source_database=False,
        source_directories=source_directories or [],
        filter_directory=".",
        buck_mode=None,
        no_saved_state=no_saved_state,
        search_path=["some_path"],
        binary="/foo/binary.exe",
        buck_builder_binary=None,
        exclude=[],
        typeshed="/typeshed",
        save_initial_state_to=save_initial_state_to,
        load_initial_state_from=load_initial_state_from,
        changed_files_path=changed_files_path,
        saved_state_project=saved_state_project,
        dot_pyre_directory=dot_pyre_directory or Path(".pyre"),
        features=features,
    )


def mock_configuration(version_hash=None, file_hash=None) -> MagicMock:
    configuration = MagicMock()
    configuration.strict = False
    configuration.source_directories = ["."]
    configuration.logger = None
    configuration.number_of_workers = 5
    configuration.search_path = ["path1", "path2"]
    configuration.taint_models_path = []
    configuration.typeshed = "stub"
    configuration.version_hash = version_hash
    configuration.file_hash = file_hash
    configuration.local_root = None
    configuration.autocomplete = False
    configuration.log_directory = ".pyre"
    configuration.disabled = False
    return configuration


def mock_incremental_command() -> Incremental:
    arguments = mock_arguments()
    configuration = mock_configuration()
    analysis_directory = AnalysisDirectory(".")
    return Incremental(
        arguments,
        original_directory="/original/directory",
        configuration=configuration,
        analysis_directory=analysis_directory,
        nonblocking=False,
        incremental_style=IncrementalStyle.FINE_GRAINED,
        no_start_server=False,
        no_watchman=False,
    )
