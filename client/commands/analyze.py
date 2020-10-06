# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


from typing import List, Optional

from typing_extensions import Final

from .. import command_arguments, log
from ..analysis_directory import AnalysisDirectory, resolve_analysis_directory
from ..configuration import Configuration
from .check import Check


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
        find_obscure_flows: bool = False,
        dump_model_query_results: bool = False,
    ) -> None:
        super(Analyze, self).__init__(
            command_arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
        )
        self._analysis: str = analysis
        self._taint_models_path: List[str] = (
            taint_models_path or list(self._configuration.taint_models_path)
        )
        self._no_verify: bool = no_verify
        self._save_results_to: Final[Optional[str]] = save_results_to
        self._dump_call_graph: bool = dump_call_graph
        self._repository_root: Final[Optional[str]] = repository_root
        self._rules: Final[Optional[List[int]]] = rules
        self._find_obscure_flows: bool = find_obscure_flows
        self._dump_model_query_results = dump_model_query_results

    def generate_analysis_directory(self) -> AnalysisDirectory:
        return resolve_analysis_directory(
            self._command_arguments.source_directories,
            self._command_arguments.targets,
            self._configuration,
            self._original_directory,
            self._configuration.project_root,
            filter_directory=self._command_arguments.filter_directory,
            buck_mode=self._command_arguments.buck_mode,
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
        if self._find_obscure_flows:
            flags.append("-find-obscure-flows")
        if self._dump_model_query_results:
            flags.append("-dump-model-query-results")
        return flags

    def _run(self, retries: int = 1) -> None:
        self._analysis_directory.prepare()
        result = self._call_client(command=self.NAME)
        result.check()
        if self._save_results_to is None:
            log.stdout.write(result.output)
