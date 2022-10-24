# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import contextlib
import dataclasses
import json
import logging
import os
import subprocess
from pathlib import Path
from typing import Any, Dict, Iterator, List, Optional, Sequence

from .. import (
    command_arguments,
    configuration as configuration_module,
    error as error_module,
    log,
)
from . import (
    backend_arguments,
    commands,
    frontend_configuration,
    start,
    validate_models,
)

LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class Arguments:
    """
    Data structure for configuration options the backend analyze command can recognize.
    Need to keep in sync with `source/command/analyzeCommand.ml`
    """

    base_arguments: backend_arguments.BaseArguments

    dump_call_graph: Optional[str] = None
    dump_model_query_results: Optional[str] = None
    find_missing_flows: Optional[str] = None
    inline_decorators: bool = False
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
    repository_root: Optional[str] = None
    rule_filter: Optional[Sequence[int]] = None
    source_filter: Optional[Sequence[str]] = None
    sink_filter: Optional[Sequence[str]] = None
    transform_filter: Optional[Sequence[str]] = None
    save_results_to: Optional[str] = None
    output_format: Optional[str] = None
    strict: bool = False
    taint_model_paths: Sequence[str] = dataclasses.field(default_factory=list)
    use_cache: bool = False
    check_invariants: bool = False

    def serialize(self) -> Dict[str, Any]:
        dump_call_graph = self.dump_call_graph
        dump_model_query_results = self.dump_model_query_results
        find_missing_flows = self.find_missing_flows
        maximum_model_source_tree_width = self.maximum_model_source_tree_width
        maximum_model_sink_tree_width = self.maximum_model_sink_tree_width
        maximum_model_tito_tree_width = self.maximum_model_tito_tree_width
        maximum_tree_depth_after_widening = self.maximum_tree_depth_after_widening
        maximum_return_access_path_width = self.maximum_return_access_path_width
        maximum_return_access_path_depth_after_widening = (
            self.maximum_return_access_path_depth_after_widening
        )
        maximum_tito_collapse_depth = self.maximum_tito_collapse_depth
        maximum_tito_positions = self.maximum_tito_positions
        maximum_overrides_to_analyze = self.maximum_overrides_to_analyze
        maximum_tito_depth = self.maximum_tito_depth
        maximum_trace_length = self.maximum_trace_length
        repository_root = self.repository_root
        rule_filter = self.rule_filter
        source_filter = self.source_filter
        sink_filter = self.sink_filter
        transform_filter = self.transform_filter
        save_results_to = self.save_results_to
        output_format = self.output_format
        return {
            **self.base_arguments.serialize(),
            **({} if dump_call_graph is None else {"dump_call_graph": dump_call_graph}),
            **(
                {}
                if dump_model_query_results is None
                else {"dump_model_query_results": dump_model_query_results}
            ),
            **(
                {}
                if find_missing_flows is None
                else {"find_missing_flows": find_missing_flows}
            ),
            "inline_decorators": self.inline_decorators,
            **(
                {}
                if maximum_model_source_tree_width is None
                else {
                    "maximum_model_source_tree_width": maximum_model_source_tree_width
                }
            ),
            **(
                {}
                if maximum_model_sink_tree_width is None
                else {"maximum_model_sink_tree_width": maximum_model_sink_tree_width}
            ),
            **(
                {}
                if maximum_model_tito_tree_width is None
                else {"maximum_model_tito_tree_width": maximum_model_tito_tree_width}
            ),
            **(
                {}
                if maximum_tree_depth_after_widening is None
                else {
                    "maximum_tree_depth_after_widening": maximum_tree_depth_after_widening
                }
            ),
            **(
                {}
                if maximum_return_access_path_width is None
                else {
                    "maximum_return_access_path_width": maximum_return_access_path_width
                }
            ),
            **(
                {}
                if maximum_return_access_path_depth_after_widening is None
                else {
                    "maximum_return_access_path_depth_after_widening": maximum_return_access_path_depth_after_widening
                }
            ),
            **(
                {}
                if maximum_tito_collapse_depth is None
                else {"maximum_tito_collapse_depth": maximum_tito_collapse_depth}
            ),
            **(
                {}
                if maximum_tito_positions is None
                else {"maximum_tito_positions": maximum_tito_positions}
            ),
            **(
                {}
                if maximum_overrides_to_analyze is None
                else {"maximum_overrides_to_analyze": maximum_overrides_to_analyze}
            ),
            **(
                {}
                if maximum_tito_depth is None
                else {"maximum_tito_depth": maximum_tito_depth}
            ),
            **(
                {}
                if maximum_trace_length is None
                else {"maximum_trace_length": maximum_trace_length}
            ),
            "no_verify": self.no_verify,
            "verify_dsl": self.verify_dsl,
            **({} if repository_root is None else {"repository_root": repository_root}),
            **({} if rule_filter is None else {"rule_filter": rule_filter}),
            **({} if source_filter is None else {"source_filter": source_filter}),
            **({} if sink_filter is None else {"sink_filter": sink_filter}),
            **(
                {}
                if transform_filter is None
                else {"transform_filter": transform_filter}
            ),
            **({} if save_results_to is None else {"save_results_to": save_results_to}),
            **({} if output_format is None else {"output_format": output_format}),
            "strict": self.strict,
            "taint_model_paths": self.taint_model_paths,
            "use_cache": self.use_cache,
            "check_invariants": self.check_invariants,
        }


def create_analyze_arguments(
    configuration: frontend_configuration.Base,
    analyze_arguments: command_arguments.AnalyzeArguments,
) -> Arguments:
    """
    Translate client configurations to backend analyze configurations.

    This API is not pure since it needs to access filesystem to filter out
    nonexistent directories. It is idempotent though, since it does not alter
    any filesystem state.
    """
    source_paths = backend_arguments.get_source_path_for_check(configuration)

    log_directory = configuration.get_log_directory()
    profiling_output = (
        backend_arguments.get_profiling_log_path(log_directory)
        if analyze_arguments.enable_profiling
        else None
    )
    memory_profiling_output = (
        backend_arguments.get_profiling_log_path(log_directory)
        if analyze_arguments.enable_memory_profiling
        else None
    )

    logger = configuration.get_remote_logger()
    remote_logging = (
        backend_arguments.RemoteLogging(
            logger=logger, identifier=analyze_arguments.log_identifier or ""
        )
        if logger is not None
        else None
    )

    find_missing_flows = analyze_arguments.find_missing_flows
    rule = analyze_arguments.rule
    source = analyze_arguments.source
    sink = analyze_arguments.sink
    transform = analyze_arguments.transform
    taint_models_path = analyze_arguments.taint_models_path
    output_format = analyze_arguments.output_format
    if len(taint_models_path) == 0:
        taint_models_path = configuration.get_taint_models_path()
    repository_root = analyze_arguments.repository_root
    if repository_root is not None:
        repository_root = str(Path(repository_root).resolve(strict=False))
    return Arguments(
        base_arguments=backend_arguments.BaseArguments(
            log_path=str(log_directory),
            global_root=str(configuration.get_global_root()),
            checked_directory_allowlist=backend_arguments.get_checked_directory_allowlist(
                configuration, source_paths
            ),
            checked_directory_blocklist=(configuration.get_ignore_all_errors()),
            debug=analyze_arguments.debug,
            excludes=configuration.get_excludes(),
            extensions=configuration.get_valid_extension_suffixes(),
            relative_local_root=configuration.get_relative_local_root(),
            memory_profiling_output=memory_profiling_output,
            number_of_workers=configuration.get_number_of_workers(),
            parallel=not analyze_arguments.sequential,
            profiling_output=profiling_output,
            python_version=configuration.get_python_version(),
            shared_memory=configuration.get_shared_memory(),
            remote_logging=remote_logging,
            search_paths=configuration.get_existent_search_paths(),
            source_paths=source_paths,
        ),
        dump_call_graph=analyze_arguments.dump_call_graph,
        dump_model_query_results=analyze_arguments.dump_model_query_results,
        find_missing_flows=str(find_missing_flows.value)
        if find_missing_flows is not None
        else None,
        inline_decorators=analyze_arguments.inline_decorators,
        maximum_model_source_tree_width=analyze_arguments.maximum_model_source_tree_width,
        maximum_model_sink_tree_width=analyze_arguments.maximum_model_sink_tree_width,
        maximum_model_tito_tree_width=analyze_arguments.maximum_model_tito_tree_width,
        maximum_tree_depth_after_widening=analyze_arguments.maximum_tree_depth_after_widening,
        maximum_return_access_path_width=analyze_arguments.maximum_return_access_path_width,
        maximum_return_access_path_depth_after_widening=analyze_arguments.maximum_return_access_path_depth_after_widening,
        maximum_tito_collapse_depth=analyze_arguments.maximum_tito_collapse_depth,
        maximum_tito_positions=analyze_arguments.maximum_tito_positions,
        maximum_overrides_to_analyze=analyze_arguments.maximum_overrides_to_analyze,
        maximum_tito_depth=analyze_arguments.maximum_tito_depth,
        maximum_trace_length=analyze_arguments.maximum_trace_length,
        no_verify=analyze_arguments.no_verify,
        verify_dsl=analyze_arguments.verify_dsl,
        repository_root=repository_root,
        rule_filter=None if len(rule) == 0 else rule,
        source_filter=None if len(source) == 0 else source,
        sink_filter=None if len(sink) == 0 else sink,
        transform_filter=None if len(transform) == 0 else transform,
        save_results_to=analyze_arguments.save_results_to,
        output_format=str(output_format.value) if output_format is not None else None,
        strict=configuration.is_strict(),
        taint_model_paths=taint_models_path,
        use_cache=analyze_arguments.use_cache,
        check_invariants=analyze_arguments.check_invariants,
    )


@contextlib.contextmanager
def create_analyze_arguments_and_cleanup(
    configuration: frontend_configuration.Base,
    analyze_arguments: command_arguments.AnalyzeArguments,
) -> Iterator[Arguments]:
    arguments = create_analyze_arguments(configuration, analyze_arguments)
    try:
        yield arguments
    finally:
        # It is safe to clean up source paths after analyze command since
        # any created artifact directory won't be reused by other commands.
        arguments.base_arguments.source_paths.cleanup()


def parse_taint_configuration_errors(
    response: str,
) -> List[error_module.TaintConfigurationError]:
    response_json = json.loads(response)
    errors = response_json.get("errors", [])
    return [error_module.TaintConfigurationError.from_json(error) for error in errors]


def parse_model_validation_errors(
    response: str,
) -> List[error_module.ModelVerificationError]:
    response_json = json.loads(response)
    return validate_models.parse_validation_errors(response_json)


def _run_analyze_command(
    command: Sequence[str],
    output: str,
    forward_stdout: bool,
    environment: Optional[Dict[str, str]],
) -> commands.ExitCode:
    with backend_arguments.backend_log_file(prefix="pyre_analyze") as log_file:
        with start.background_logging(Path(log_file.name)):
            # lint-ignore: NoUnsafeExecRule
            result = subprocess.run(
                command,
                stdout=subprocess.PIPE,
                stderr=log_file.file,
                universal_newlines=True,
                errors="replace",
                env=environment,
            )
            return_code = result.returncode

            # Interpretation of the return code needs to be kept in sync with
            # `command/analyzeCommand.ml`.
            if return_code == 0:
                if forward_stdout:
                    log.stdout.write(result.stdout)
                return commands.ExitCode.SUCCESS
            if return_code == 2:
                LOG.error("Pyre encountered a failure within buck.")
                return commands.ExitCode.BUCK_INTERNAL_ERROR
            if return_code == 3:
                LOG.error("Pyre encountered an error when building the buck targets.")
                return commands.ExitCode.BUCK_USER_ERROR
            if return_code == 10:
                error_module.print_errors(
                    parse_taint_configuration_errors(result.stdout),
                    output=output,
                    error_kind="taint configuration",
                )
                return commands.ExitCode.TAINT_CONFIGURATION_ERROR
            if return_code == 11:
                error_module.print_errors(
                    parse_model_validation_errors(result.stdout),
                    output=output,
                    error_kind="model verification",
                )
                return commands.ExitCode.MODEL_VERIFICATION_ERROR
            LOG.error(f"Pyre exited with non-zero return code: {return_code}.")
            return commands.ExitCode.FAILURE


def run_analyze(
    configuration: frontend_configuration.Base,
    analyze_arguments: command_arguments.AnalyzeArguments,
) -> commands.ExitCode:
    binary_location = configuration.get_binary_location(download_if_needed=True)
    if binary_location is None:
        raise configuration_module.InvalidConfiguration(
            "Cannot locate a Pyre binary to run."
        )

    with create_analyze_arguments_and_cleanup(
        configuration, analyze_arguments
    ) as arguments:
        with backend_arguments.temporary_argument_file(arguments) as argument_file_path:
            analyze_command = [
                str(binary_location),
                "newanalyze",
                str(argument_file_path),
            ]
            environment = None
            if analyze_arguments.check_invariants:
                # We need to pass this specific argument as an environment variable,
                # because it is needed during the global initialization.
                environment = dict(os.environ)
                environment["PYSA_CHECK_INVARIANTS"] = "1"
            return _run_analyze_command(
                command=analyze_command,
                environment=environment,
                output=analyze_arguments.output,
                forward_stdout=(analyze_arguments.save_results_to is None),
            )


def run(
    configuration: configuration_module.Configuration,
    analyze_arguments: command_arguments.AnalyzeArguments,
) -> commands.ExitCode:
    return run_analyze(
        frontend_configuration.OpenSource(configuration), analyze_arguments
    )
