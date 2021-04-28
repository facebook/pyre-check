# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import enum
import json
from typing import List, Optional

from typing_extensions import Final

from .. import command_arguments, log
from ..analysis_directory import AnalysisDirectory, resolve_analysis_directory
from ..configuration import Configuration
from ..error import print_errors
from .check import Check
from .command import ClientException, ExitCode
from .validate_models import ValidateModels


class MissingFlowsKind(str, enum.Enum):
    OBSCURE: str = "obscure"
    TYPE: str = "type"


class Analyze(Check):
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
        dump_call_graph: bool,
        repository_root: Optional[str],
        rules: Optional[List[int]],
        find_missing_flows: Optional[MissingFlowsKind] = None,
        dump_model_query_results: bool = False,
        use_cache: bool,
        inline_decorators: bool,
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
        self._dump_call_graph: bool = dump_call_graph
        self._repository_root: Final[Optional[str]] = repository_root
        self._rules: Final[Optional[List[int]]] = rules
        self._find_missing_flows: Optional[MissingFlowsKind] = find_missing_flows
        self._dump_model_query_results = dump_model_query_results
        self._use_cache: bool = use_cache
        self._inline_decorators: bool = inline_decorators

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
        flags.extend(["-analysis", self._analysis])
        if self._taint_models_path:
            for path in self._taint_models_path:
                flags.extend(["-taint-models", path])
        save_results_to = self._save_results_to
        if save_results_to:
            flags.extend(["-save-results-to", save_results_to])
        if self._dump_call_graph:
            flags.append("-dump-call-graph")
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
        if self._dump_model_query_results:
            flags.append("-dump-model-query-results")
        if self._use_cache:
            flags.append("-use-cache")
        if self._inline_decorators:
            flags.append("-inline-decorators")
        return flags

    def _run(self, retries: int = 1) -> None:
        self._analysis_directory.prepare()
        result = self._call_client(command=self.NAME)
        result.check()

        try:
            errors = ValidateModels.parse_errors(
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
