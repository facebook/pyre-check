# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from pathlib import Path
from typing import Optional
from unittest.mock import MagicMock

from .. import command_arguments, configuration as configuration_module
from ..analysis_directory import AnalysisDirectory
from ..commands.command import IncrementalStyle
from ..commands.incremental import Incremental
from ..configuration import SharedMemory


def mock_arguments(
    debug: bool = False,
    changed_files_path=None,
    enable_profiling: bool = False,
    enable_memory_profiling: bool = False,
    features=None,
    load_initial_state_from=None,
    local_configuration=None,
    log_identifier: str = "",
    no_saved_state: bool = False,
    output: str = command_arguments.TEXT,
    save_initial_state_to=None,
    saved_state_project=None,
    sequential: bool = False,
    source_directories=None,
    targets=None,
    dot_pyre_directory: Optional[Path] = None,
) -> command_arguments.CommandArguments:
    return command_arguments.CommandArguments(
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
        noninteractive=True,
        logging_sections=None,
        log_identifier=log_identifier,
        logger=None,
        targets=targets or [],
        use_buck_builder=False,
        use_buck_source_database=False,
        source_directories=source_directories or [],
        filter_directory=None,
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
        python_version="3.6.0",
        shared_memory_heap_size=1024 * 1024 * 1024,
    )


def mock_configuration(version_hash=None, file_hash=None) -> MagicMock:
    configuration = MagicMock()
    configuration.project_root = "/root"
    configuration.local_root = None
    configuration.strict = False
    configuration.source_directories = ["."]
    configuration.logger = None
    configuration.get_number_of_workers = lambda: 5
    configuration.search_path = []
    configuration.taint_models_path = []
    configuration.get_typeshed_respecting_override = lambda: "stub"
    configuration.get_version_hash_respecting_override = lambda: version_hash
    configuration.file_hash = file_hash
    configuration.local_root = None
    configuration.autocomplete = False
    configuration.dot_pyre_directory = Path(".pyre")
    configuration.relative_local_root = None
    configuration.log_directory = ".pyre"
    configuration.disabled = False
    configuration.get_python_version = lambda: configuration_module.PythonVersion(
        major=3, minor=6, micro=0
    )
    configuration.shared_memory = SharedMemory(heap_size=1024 * 1024 * 1024)
    return configuration


def mock_incremental_command(cfg: configuration_module.Configuration) -> Incremental:
    arguments = mock_arguments()
    analysis_directory = AnalysisDirectory(
        configuration_module.SimpleSearchPathElement(".")
    )
    return Incremental(
        arguments,
        original_directory="/original/directory",
        configuration=cfg,
        analysis_directory=analysis_directory,
        nonblocking=False,
        incremental_style=IncrementalStyle.FINE_GRAINED,
        no_start_server=False,
        no_watchman=False,
    )
