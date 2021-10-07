# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import dataclasses
import json
import os
from pathlib import Path
from typing import List, Optional, Sequence, Dict, Any

from typing_extensions import Final

from .. import command_arguments, log
from ..analysis_directory import AnalysisDirectory, resolve_analysis_directory
from ..configuration import Configuration
from ..error import ModelVerificationError, print_errors
from .command import ClientException, ExitCode
from .reporting import Reporting


def _relativize_error(
    configuration: Configuration,
    relative_root: str,
    error: ModelVerificationError,
    original_directory: str,
) -> ModelVerificationError:
    if error.path is None:
        return error

    path = os.path.realpath(os.path.join(relative_root, error.path))
    # If relative paths don't make sense, keep the absolute path around.
    if not path.startswith(configuration.project_root) or not os.path.exists(path):
        return error

    # Relativize path to user's cwd.
    relative_path = os.path.relpath(path, original_directory)
    return dataclasses.replace(error, path=relative_path)


def parse_errors(
    json_result: Dict[str, Any],
    configuration: Configuration,
    analysis_directory: AnalysisDirectory,
    original_directory: str,
) -> Sequence[ModelVerificationError]:
    if "errors" not in json_result:
        return []
    analysis_root = os.path.realpath(analysis_directory.get_root())
    return sorted(
        (
            _relativize_error(
                configuration,
                analysis_root,
                ModelVerificationError.from_json(error_json),
                original_directory,
            )
            for error_json in json_result["errors"]
        ),
        key=lambda error: (error.path or Path(), error.line, error.code),
    )


class Analyze(Reporting):
    NAME = "analyze"

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        original_directory: str,
        *,
        configuration: Configuration,
        analysis_directory: Optional[AnalysisDirectory] = None,
        analysis: str,
        taint_models_path: List[str],
        no_verify: bool,
        save_results_to: Optional[str],
        dump_call_graph: Optional[str],
        repository_root: Optional[str],
        rules: Optional[List[int]],
        find_missing_flows: Optional[command_arguments.MissingFlowsKind] = None,
        dump_model_query_results: Optional[str] = None,
        use_cache: bool,
        inline_decorators: bool,
        maximum_trace_length: Optional[int],
        maximum_tito_depth: Optional[int],
    ) -> None:
        super(Analyze, self).__init__(
            command_arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
        )
        self._analysis: str = analysis
        self._taint_models_path: List[str] = taint_models_path or list(
            self._configuration.taint_models_path
        )
        self._no_verify: bool = no_verify
        self._save_results_to: Final[Optional[str]] = save_results_to
        self._dump_call_graph: Final[Optional[str]] = dump_call_graph
        self._repository_root: Final[Optional[str]] = repository_root
        self._rules: Final[Optional[List[int]]] = rules
        # pyre-ignore[11]: Annotation `MissingFlowsKind` is not defined as a type.
        self._find_missing_flows: Optional[
            command_arguments.MissingFlowsKind
        ] = find_missing_flows
        self._dump_model_query_results: Final[Optional[str]] = dump_model_query_results
        self._use_cache: bool = use_cache
        self._inline_decorators: bool = inline_decorators
        self._maximum_trace_length: Optional[int] = maximum_trace_length
        self._maximum_tito_depth: Optional[int] = maximum_tito_depth

    def generate_analysis_directory(self) -> AnalysisDirectory:
        return resolve_analysis_directory(
            self._command_arguments.source_directories,
            self._command_arguments.targets,
            self._configuration,
            self._original_directory,
            filter_directory=self._command_arguments.filter_directory,
            isolate=True,
        )

    def _flags(self) -> List[str]:
        flags = super()._flags()
        filter_directories = self._get_directories_to_analyze()
        filter_directories.update(
            set(self._configuration.get_existent_do_not_ignore_errors_in_paths())
        )
        if len(filter_directories):
            flags.extend(["-filter-directories", ";".join(sorted(filter_directories))])
        flags.extend(["-workers", str(self._configuration.get_number_of_workers())])

        search_path = [
            search_path.command_line_argument()
            for search_path in (
                self._configuration.expand_and_get_existent_search_paths()
            )
        ]

        ignore_all_errors_paths = (
            self._configuration.get_existent_ignore_all_errors_paths()
        )
        if len(ignore_all_errors_paths):
            flags.extend(
                ["-ignore-all-errors", ";".join(sorted(ignore_all_errors_paths))]
            )
        if search_path:
            flags.extend(["-search-path", ",".join(search_path)])
        excludes = self._configuration.excludes
        for exclude in excludes:
            flags.extend(["-exclude", exclude])
        extensions = [
            extension.command_line_argument()
            for extension in self._configuration.extensions
        ]
        for extension in extensions:
            flags.extend(["-extension", extension])
        flags.extend(["-analysis", self._analysis])
        if self._taint_models_path:
            for path in self._taint_models_path:
                flags.extend(["-taint-models", path])
        save_results_to = self._save_results_to
        if save_results_to:
            flags.extend(["-save-results-to", save_results_to])
        dump_call_graph = self._dump_call_graph
        if dump_call_graph:
            flags.extend(["-dump-call-graph", dump_call_graph])
        if self._no_verify:
            flags.append("-no-verify")
        repository_root = self._repository_root
        if repository_root:
            flags.extend(["-repository-root", repository_root])
        rules = self._rules
        if rules is not None:
            flags.extend(["-rules", ",".join(str(rule) for rule in rules)])
        find_missing_flows = self._find_missing_flows
        if find_missing_flows:
            flags.extend(["-find-missing-flows", find_missing_flows.value])
        dump_model_query_results = self._dump_model_query_results
        if dump_model_query_results:
            flags.extend(["-dump-model-query-results", dump_model_query_results])
        if self._use_cache:
            flags.append("-use-cache")
        if self._inline_decorators:
            flags.append("-inline-decorators")
        maximum_trace_length = self._maximum_trace_length
        if maximum_trace_length:
            flags.extend(["-maximum-trace-length", str(maximum_trace_length)])
        maximum_tito_depth = self._maximum_tito_depth
        if maximum_tito_depth:
            flags.extend(["-maximum-tito-depth", str(maximum_tito_depth)])

        return flags

    def _run(self, retries: int = 1) -> None:
        self._analysis_directory.prepare()
        result = self._call_client(command=self.NAME)
        result.check()

        try:
            errors = parse_errors(
                json.loads(result.output),
                self._configuration,
                self._analysis_directory,
                self._original_directory,
            )
        except json.JSONDecodeError:
            raise ClientException(f"Invalid JSON output: `{result.output}`.")

        if errors:
            print_errors(errors, output=self._output, error_kind="model verification")
            self._exit_code = ExitCode.FOUND_ERRORS
        elif self._save_results_to is None:
            log.stdout.write(result.output)
