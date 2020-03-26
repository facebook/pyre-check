# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from pathlib import Path
from typing import Optional
from unittest.mock import MagicMock

from ..commands.command import TEXT


def mock_arguments(
    build=False,
    changed_files_path=None,
    command=None,
    load_initial_state_from=None,
    no_saved_state=False,
    no_verify=False,
    no_watchman=False,
    output=TEXT,
    save_initial_state_to=None,
    saved_state_project=None,
    source_directories=None,
    store_type_check_resolution=False,
    targets=None,
    terminal=False,
    dot_pyre_directory: Optional[Path] = None,
) -> MagicMock:
    arguments = MagicMock()
    arguments.additional_check = []
    arguments.analysis = "taint"
    arguments.build = build
    arguments.changed_files_path = changed_files_path
    arguments.command = command
    arguments.debug = False
    arguments.enable_profiling = False
    arguments.enable_memory_profiling = False
    arguments.features = None
    arguments.filter_directory = ["."]
    arguments.hide_parse_errors = False
    arguments.load_initial_state_from = load_initial_state_from
    arguments.local = False
    arguments.local_configuration = None
    arguments.log_identifier = None
    arguments.logger = None
    arguments.logging_sections = None
    arguments.no_saved_state = no_saved_state
    arguments.no_start = False
    arguments.no_verify = no_verify
    arguments.no_watchman = no_watchman
    arguments.nonblocking = False
    arguments.output = output
    arguments.dot_pyre_directory = dot_pyre_directory or Path(".pyre")
    arguments.save_initial_state_to = save_initial_state_to
    arguments.save_results_to = None
    arguments.saved_state_project = saved_state_project
    arguments.sequential = False
    arguments.show_error_traces = False
    arguments.source_directories = source_directories
    arguments.store_type_check_resolution = store_type_check_resolution
    arguments.strict = False
    arguments.taint_models_path = []
    arguments.targets = targets
    arguments.terminal = terminal
    arguments.verbose = False
    arguments.repository_root = None
    arguments.rule = None
    return arguments


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
    configuration.local_configuration_root = None
    configuration.autocomplete = False
    configuration.log_directory = ".pyre"
    configuration.disabled = False
    return configuration
