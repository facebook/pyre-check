# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module provides a series of dataclasses and enums
that describe command-line arguments for invoking Pyre.

The `CommandArguments` dataclass represents top-level
arguments, whereas the other classes describe arguments
specific to subcommands.

For example, in the invocation
```
pyre --noninteractive start --terminal
```
the `--noninteractive` flag will determine the
`CommandArguments.noninteractive` field, whereas the `--terminal` flag will
determine the `StartArguments.terminal` field.
"""

from __future__ import annotations

import enum
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional, Set

from .identifiers import PyreFlavor

TEXT: str = "text"
JSON: str = "json"
SARIF: str = "sarif"


class ProfileOutput(enum.Enum):
    # pyre-fixme[35]: Target cannot be annotated.
    TRACE_EVENT: str = "trace_event"
    # pyre-fixme[35]: Target cannot be annotated.
    COLD_START_PHASES: str = "cold_start_phases"
    # pyre-fixme[35]: Target cannot be annotated.
    INCREMENTAL_UPDATES: str = "incremental_updates"
    # pyre-fixme[35]: Target cannot be annotated.
    TAINT: str = "taint"
    # pyre-fixme[35]: Target cannot be annotated.
    INDIVIDUAL_TABLE_SIZES: str = "individual_table_sizes"
    # pyre-fixme[35]: Target cannot be annotated.
    TOTAL_SHARED_MEMORY_SIZE_OVER_TIME: str = "total_shared_memory_size_over_time"
    # pyre-fixme[35]: Target cannot be annotated.
    TOTAL_SHARED_MEMORY_SIZE_OVER_TIME_GRAPH: str = (
        "total_shared_memory_size_over_time_graph"  # noqa B950
    )

    def __str__(self) -> str:
        return self.value


class VersionKind(str, enum.Enum):
    # pyre-fixme[35]: Target cannot be annotated.
    NONE: str = "none"
    # pyre-fixme[35]: Target cannot be annotated.
    CLIENT: str = "client"
    # pyre-fixme[35]: Target cannot be annotated.
    CLIENT_AND_BINARY: str = "client_and_binary"


class MissingFlowsKind(str, enum.Enum):
    # pyre-fixme[35]: Target cannot be annotated.
    OBSCURE: str = "obscure"
    # pyre-fixme[35]: Target cannot be annotated.
    TYPE: str = "type"


class TaintOutputFormat(str, enum.Enum):
    # pyre-fixme[35]: Target cannot be annotated.
    JSON: str = "json"
    # pyre-fixme[35]: Target cannot be annotated.
    SHARDED_JSON: str = "sharded-json"


@dataclass(frozen=True)
class CommandArguments:
    local_configuration: Optional[str] = None
    version: VersionKind = VersionKind.NONE
    debug: bool = False
    sequential: bool = False
    strict: bool = False
    show_error_traces: bool = False
    output: str = TEXT
    enable_profiling: bool = False
    enable_memory_profiling: bool = False
    noninteractive: bool = False
    logging_sections: Optional[str] = None
    log_identifier: Optional[str] = None
    logger: Optional[str] = None
    no_logger: bool = False
    targets: List[str] = field(default_factory=list)
    source_directories: List[str] = field(default_factory=list)
    only_check_paths: List[str] = field(default_factory=list)
    buck_mode: Optional[str] = None
    no_saved_state: bool = False
    search_path: List[str] = field(default_factory=list)
    optional_search_path: List[str] = field(default_factory=list)
    binary: Optional[str] = None
    exclude: List[str] = field(default_factory=list)
    typeshed: Optional[str] = None
    save_initial_state_to: Optional[str] = None
    load_initial_state_from: Optional[str] = None
    changed_files_path: Optional[str] = None
    dot_pyre_directory: Optional[Path] = None
    isolation_prefix: Optional[str] = None
    python_version: Optional[str] = None
    system_platform: Optional[str] = None
    shared_memory_heap_size: Optional[int] = None
    shared_memory_dependency_table_power: Optional[int] = None
    shared_memory_hash_table_power: Optional[int] = None
    number_of_workers: Optional[int] = None
    enable_unawaited_awaitable_analysis: Optional[bool] = None
    include_suppressed_errors: Optional[bool] = None


@dataclass(frozen=True)
class StartArguments:
    changed_files_path: Optional[str] = None
    debug: bool = False
    enable_memory_profiling: bool = False
    enable_profiling: bool = False
    flavor: PyreFlavor = PyreFlavor.CLASSIC
    load_initial_state_from: Optional[str] = None
    _log_identifier: Optional[str] = None
    logging_sections: Optional[str] = None
    no_saved_state: bool = False
    no_watchman: bool = False
    noninteractive: bool = False
    save_initial_state_to: Optional[str] = None
    sequential: bool = False
    show_error_traces: bool = False
    store_type_check_resolution: bool = False
    terminal: bool = False
    wait_on_initialization: bool = False
    skip_initial_type_check: bool = False
    use_lazy_module_tracking: bool = False
    analyze_external_sources: bool = False

    @staticmethod
    def create(
        command_argument: CommandArguments,
        flavor: PyreFlavor = PyreFlavor.CLASSIC,
        no_watchman: bool = False,
        store_type_check_resolution: bool = False,
        wait_on_initialization: bool = False,
        terminal: bool = False,
        skip_initial_type_check: bool = False,
        use_lazy_module_tracking: bool = False,
        analyze_external_sources: bool = False,
    ) -> StartArguments:
        return StartArguments(
            changed_files_path=command_argument.changed_files_path,
            debug=command_argument.debug,
            enable_memory_profiling=command_argument.enable_memory_profiling,
            enable_profiling=command_argument.enable_profiling,
            flavor=flavor,
            load_initial_state_from=command_argument.load_initial_state_from,
            _log_identifier=command_argument.log_identifier,
            logging_sections=command_argument.logging_sections,
            no_saved_state=command_argument.no_saved_state,
            no_watchman=no_watchman,
            noninteractive=command_argument.noninteractive,
            save_initial_state_to=command_argument.save_initial_state_to,
            sequential=command_argument.sequential,
            show_error_traces=command_argument.show_error_traces,
            store_type_check_resolution=store_type_check_resolution,
            terminal=terminal,
            wait_on_initialization=wait_on_initialization,
            skip_initial_type_check=skip_initial_type_check,
            use_lazy_module_tracking=use_lazy_module_tracking,
            analyze_external_sources=analyze_external_sources,
        )

    def get_log_identifier(self) -> str:
        """
        If a log identifier was manually set (this is usually done specifically
        to isolate telemetry, e.g. when running a performance experiment), we
        use that.

        Otherwise, we use the flavor. This keeps telemetry from various kinds
        of language servers separate so that our metrics can distinguish them.
        """
        if self._log_identifier is not None:
            return self._log_identifier
        else:
            return self.flavor.value


@dataclass(frozen=True)
class IncrementalArguments:
    output: str = TEXT
    no_start: bool = False
    start_arguments: StartArguments = field(default_factory=StartArguments)


@dataclass(frozen=True)
class CheckArguments:
    debug: bool = False
    enable_memory_profiling: bool = False
    enable_profiling: bool = False
    log_identifier: Optional[str] = None
    logging_sections: Optional[str] = None
    noninteractive: bool = False
    output: str = TEXT
    sequential: bool = False
    show_error_traces: bool = False

    @staticmethod
    def create(
        command_argument: CommandArguments,
    ) -> CheckArguments:
        return CheckArguments(
            debug=command_argument.debug,
            enable_memory_profiling=command_argument.enable_memory_profiling,
            enable_profiling=command_argument.enable_profiling,
            log_identifier=command_argument.log_identifier,
            logging_sections=command_argument.logging_sections,
            noninteractive=command_argument.noninteractive,
            output=command_argument.output,
            sequential=command_argument.sequential,
            show_error_traces=command_argument.show_error_traces,
        )


@dataclass(frozen=True)
class InferArguments:
    working_directory: Path
    annotate_attributes: bool = False
    annotate_from_existing_stubs: bool = False
    debug_infer: bool = False
    quote_annotations: bool = False
    dequalify: bool = False
    enable_memory_profiling: bool = False
    enable_profiling: bool = False
    log_identifier: Optional[str] = None
    logging_sections: Optional[str] = None
    use_future_annotations: bool = False
    in_place: bool = False
    simple_annotations: bool = False
    paths_to_modify: Optional[Set[Path]] = None
    print_only: bool = False
    read_stdin: bool = False
    sequential: bool = False


@dataclass(frozen=True)
class RageArguments:
    output: Optional[Path] = None
    server_log_count: Optional[int] = None


@dataclass(frozen=True)
class StatisticsArguments:
    paths: Optional[List[Path]] = None
    log_identifier: Optional[str] = None
    log_results: bool = False
    aggregate: bool = False
    print_summary: bool = False


@dataclass(frozen=True)
class CoverageArguments:
    working_directory: str
    paths: List[str] = field(default_factory=list)
    print_summary: bool = False


@dataclass(frozen=True)
class PysaSavedStateArguments:
    watchman_root: Optional[Path] = None
    project_name: Optional[str] = None
    preset: Optional[str] = None
    cache_critical_files: List[str] = field(default_factory=list)

    def serialize(self) -> Dict[str, Any]:
        return {
            "watchman_root": (
                str(self.watchman_root) if self.watchman_root is not None else None
            ),
            "project_name": self.project_name,
            "preset": self.preset,
            "cache_critical_files": self.cache_critical_files,
        }


@dataclass(frozen=True)
class AnalyzeArguments:
    debug: bool = False
    dump_call_graph: Optional[str] = None
    dump_model_query_results: Optional[str] = None
    enable_memory_profiling: bool = False
    enable_profiling: bool = False
    find_missing_flows: Optional[MissingFlowsKind] = None
    inline_decorators: bool = False
    infer_self_tito: bool = False
    infer_argument_tito: bool = False
    log_identifier: Optional[str] = None
    maximum_model_source_tree_width: Optional[int] = None
    maximum_model_sink_tree_width: Optional[int] = None
    maximum_model_tito_tree_width: Optional[int] = None
    maximum_tree_depth_after_widening: Optional[int] = None
    maximum_return_access_path_width: Optional[int] = None
    maximum_return_access_path_depth_after_widening: Optional[int] = None
    maximum_tito_collapse_depth: Optional[int] = None
    maximum_tito_positions: Optional[int] = None
    maximum_overrides_to_analyze: Optional[int] = None
    maximum_tito_depth: Optional[int] = None
    maximum_trace_length: Optional[int] = None
    no_verify: bool = False
    verify_dsl: bool = False
    verify_taint_config_only: bool = False
    output: str = TEXT
    repository_root: Optional[str] = None
    rule: List[int] = field(default_factory=list)
    source: List[str] = field(default_factory=list)
    sink: List[str] = field(default_factory=list)
    transform: List[str] = field(default_factory=list)
    save_results_to: Optional[str] = None
    output_format: Optional[TaintOutputFormat] = None
    sequential: bool = False
    taint_models_path: List[str] = field(default_factory=list)
    use_cache: bool = False
    build_cache_only: bool = False
    check_invariants: bool = False
    limit_entrypoints: bool = False
    compact_ocaml_heap: bool = False
    saved_state_arguments: PysaSavedStateArguments = field(
        default_factory=PysaSavedStateArguments
    )
    compute_coverage: bool = False
    scheduler_policies_path: Optional[Path] = None


@dataclass(frozen=True)
class QueryArguments:
    query: str
    no_daemon: bool
    no_validation_on_class_lookup_failure: bool
    check_arguments: CheckArguments = field(default_factory=CheckArguments)
