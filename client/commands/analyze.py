# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import argparse
from typing import List, Optional  # noqa

from .. import log
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from .check import Check


class Analyze(Check):
    NAME = "analyze"  # type: str

    def __init__(
        self,
        arguments: argparse.Namespace,
        configuration: Configuration,
        analysis_directory: AnalysisDirectory,
    ) -> None:
        super(Analyze, self).__init__(arguments, configuration, analysis_directory)
        self._analysis = arguments.analysis  # type: str
        self._no_verify = arguments.no_verify  # type: bool
        self._taint_models_path = (
            arguments.taint_models_path or configuration.taint_models_path
        )  # type: List[str]
        self._save_results_to = arguments.save_results_to  # type: Optional[str]
        self._dump_call_graph = arguments.dump_call_graph  # type: bool
        self._repository_root = arguments.repository_root  # type: Optional[str]
        self._rules = arguments.rule  # type: Optional[List[int]]

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
        return flags

    def _run(self, retries: int = 1) -> None:
        self._analysis_directory.prepare()
        result = self._call_client(command=self.NAME)
        result.check()
        log.stdout.write(result.output)
