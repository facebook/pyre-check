# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import contextlib
import dataclasses
import json
import logging
import subprocess
from pathlib import Path
from typing import Optional, Sequence, Dict, Any, Iterator, List

from .. import (
    command_arguments,
    configuration as configuration_module,
    error,
    log,
)
from . import backend_arguments, commands, remote_logging, start, validate_models

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
    maximum_tito_depth: Optional[int] = None
    maximum_trace_length: Optional[int] = None
    no_verify: bool = False
    repository_root: Optional[str] = None
    rule_filter: Optional[Sequence[int]] = None
    save_results_to: Optional[str] = None
    strict: bool = False
    taint_model_paths: Sequence[str] = dataclasses.field(default_factory=list)
    use_cache: bool = False

    def serialize(self) -> Dict[str, Any]:
        dump_call_graph = self.dump_call_graph
        dump_model_query_results = self.dump_model_query_results
        find_missing_flows = self.find_missing_flows
        maximum_tito_depth = self.maximum_tito_depth
        maximum_trace_length = self.maximum_trace_length
        repository_root = self.repository_root
        rule_filter = self.rule_filter
        save_results_to = self.save_results_to
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
                if maximum_tito_depth is None
                else {"maximum_tito_depth": maximum_tito_depth}
            ),
            **(
                {}
                if maximum_trace_length is None
                else {"maximum_trace_length": maximum_trace_length}
            ),
            "no_verify": self.no_verify,
            **({} if repository_root is None else {"repository_root": repository_root}),
            **({} if rule_filter is None else {"rule_filter": rule_filter}),
            **({} if save_results_to is None else {"save_results_to": save_results_to}),
            "strict": self.strict,
            "taint_model_paths": self.taint_model_paths,
            "use_cache": self.use_cache,
        }


def create_analyze_arguments(
    configuration: configuration_module.Configuration,
    analyze_arguments: command_arguments.AnalyzeArguments,
) -> Arguments:
    """
    Translate client configurations to backend analyze configurations.

    This API is not pure since it needs to access filesystem to filter out
    nonexistent directories. It is idempotent though, since it does not alter
    any filesystem state.
    """
    source_paths = backend_arguments.get_source_path_for_check(configuration)

    profiling_output = (
        backend_arguments.get_profiling_log_path(Path(configuration.log_directory))
        if analyze_arguments.enable_profiling
        else None
    )
    memory_profiling_output = (
        backend_arguments.get_profiling_log_path(Path(configuration.log_directory))
        if analyze_arguments.enable_memory_profiling
        else None
    )

    logger = configuration.logger
    remote_logging = (
        backend_arguments.RemoteLogging(
            logger=logger, identifier=analyze_arguments.log_identifier or ""
        )
        if logger is not None
        else None
    )

    find_missing_flows = analyze_arguments.find_missing_flows
    rule = analyze_arguments.rule
    taint_models_path = analyze_arguments.taint_models_path
    if len(taint_models_path) == 0:
        taint_models_path = configuration.taint_models_path
    repository_root = analyze_arguments.repository_root
    if repository_root is not None:
        repository_root = str(Path(repository_root).resolve(strict=False))
    return Arguments(
        base_arguments=backend_arguments.BaseArguments(
            log_path=configuration.log_directory,
            global_root=configuration.project_root,
            checked_directory_allowlist=backend_arguments.get_checked_directory_allowlist(
                configuration, source_paths
            ),
            checked_directory_blocklist=(
                configuration.get_existent_ignore_all_errors_paths()
            ),
            debug=analyze_arguments.debug,
            excludes=configuration.excludes,
            extensions=configuration.get_valid_extension_suffixes(),
            relative_local_root=configuration.relative_local_root,
            memory_profiling_output=memory_profiling_output,
            number_of_workers=configuration.get_number_of_workers(),
            parallel=not analyze_arguments.sequential,
            profiling_output=profiling_output,
            python_version=configuration.get_python_version(),
            shared_memory=configuration.shared_memory,
            remote_logging=remote_logging,
            search_paths=configuration.expand_and_get_existent_search_paths(),
            source_paths=source_paths,
        ),
        dump_call_graph=analyze_arguments.dump_call_graph,
        dump_model_query_results=analyze_arguments.dump_model_query_results,
        find_missing_flows=str(find_missing_flows.value)
        if find_missing_flows is not None
        else None,
        inline_decorators=analyze_arguments.inline_decorators,
        maximum_tito_depth=analyze_arguments.maximum_tito_depth,
        maximum_trace_length=analyze_arguments.maximum_trace_length,
        no_verify=analyze_arguments.no_verify,
        repository_root=repository_root,
        rule_filter=None if len(rule) == 0 else rule,
        save_results_to=analyze_arguments.save_results_to,
        strict=configuration.strict,
        taint_model_paths=taint_models_path,
        use_cache=analyze_arguments.use_cache,
    )


@contextlib.contextmanager
def create_analyze_arguments_and_cleanup(
    configuration: configuration_module.Configuration,
    analyze_arguments: command_arguments.AnalyzeArguments,
) -> Iterator[Arguments]:
    arguments = create_analyze_arguments(configuration, analyze_arguments)
    try:
        yield arguments
    finally:
        # It is safe to clean up source paths after analyze command since
        # any created artifact directory won't be reused by other commands.
        arguments.base_arguments.source_paths.cleanup()


def parse_model_validation_errors(response: str) -> List[error.ModelVerificationError]:
    response_json = json.loads(response)
    return validate_models.parse_validation_errors(response_json)


def _run_analyze_command(
    command: Sequence[str], output: str, forward_stdout: bool
) -> commands.ExitCode:
    with backend_arguments.backend_log_file(prefix="pyre_analyze") as log_file:
        with start.background_logging(Path(log_file.name)):
            result = subprocess.run(
                command,
                stdout=subprocess.PIPE,
                stderr=log_file.file,
                universal_newlines=True,
            )
            return_code = result.returncode

            # Interpretation of the return code needs to be kept in sync with
            # `command/newAnalyzeCommand.ml`.
            if return_code == 0:
                model_validation_errors = parse_model_validation_errors(result.stdout)
                if len(model_validation_errors) > 0:
                    error.print_errors(
                        model_validation_errors,
                        output=output,
                        error_kind="model verification",
                    )
                    return commands.ExitCode.FOUND_ERRORS

                if forward_stdout:
                    log.stdout.write(result.stdout)
                return commands.ExitCode.SUCCESS
            elif return_code == 2:
                LOG.error("Pyre encountered a failure within buck.")
                return commands.ExitCode.BUCK_INTERNAL_ERROR
            elif return_code == 3:
                LOG.error("Pyre encountered an error when building the buck targets.")
                return commands.ExitCode.BUCK_USER_ERROR
            else:
                LOG.error(
                    f"Check command exited with non-zero return code: {return_code}."
                )
                return commands.ExitCode.FAILURE


def run_analyze(
    configuration: configuration_module.Configuration,
    analyze_arguments: command_arguments.AnalyzeArguments,
) -> commands.ExitCode:
    binary_location = configuration.get_binary_respecting_override()
    if binary_location is None:
        raise configuration_module.InvalidConfiguration(
            "Cannot locate a Pyre binary to run."
        )

    with create_analyze_arguments_and_cleanup(
        configuration, analyze_arguments
    ) as arguments:
        with backend_arguments.temporary_argument_file(arguments) as argument_file_path:
            analyze_command = [binary_location, "newanalyze", str(argument_file_path)]
            return _run_analyze_command(
                command=analyze_command,
                output=analyze_arguments.output,
                forward_stdout=(analyze_arguments.save_results_to is None),
            )


@remote_logging.log_usage(command_name="analyze")
def run(
    configuration: configuration_module.Configuration,
    analyze_arguments: command_arguments.AnalyzeArguments,
) -> commands.ExitCode:
    try:
        return run_analyze(configuration, analyze_arguments)
    except Exception as error:
        raise commands.ClientException(
            f"Exception occurred during pyre analyze: {error}"
        ) from error
