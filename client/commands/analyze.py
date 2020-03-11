# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import argparse
import os
from typing import List, Optional

from typing_extensions import Final

from .. import assert_writable_directory, log, readable_directory
from ..analysis_directory import AnalysisDirectory, resolve_analysis_directory
from ..configuration import Configuration
from .check import Check


def writable_directory(path: str) -> str:
    # Create the directory if it does not exist.
    try:
        os.makedirs(path)
    except FileExistsError:
        pass
    path = os.path.abspath(path)
    assert_writable_directory(path)
    return path


class Analyze(Check):
    NAME = "analyze"

    def __init__(
        self,
        arguments: argparse.Namespace,
        original_directory: str,
        configuration: Optional[Configuration] = None,
        analysis_directory: Optional[AnalysisDirectory] = None,
    ) -> None:
        super(Analyze, self).__init__(
            arguments, original_directory, configuration, analysis_directory
        )
        self._analysis: str = arguments.analysis
        self._taint_models_path: List[str] = (
            arguments.taint_models_path or self._configuration.taint_models_path
        )
        self._no_verify: bool = arguments.no_verify
        self._save_results_to: Final[Optional[str]] = arguments.save_results_to
        self._dump_call_graph: bool = arguments.dump_call_graph
        self._repository_root: Final[Optional[str]] = arguments.repository_root
        self._rules: Final[Optional[List[int]]] = arguments.rule

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        analyze = parser.add_parser(cls.NAME)
        analyze.set_defaults(command=cls)
        analyze.add_argument(
            "analysis",
            nargs="?",
            default="taint",
            help="Type of analysis to run: {taint}",
        )
        analyze.add_argument(
            "--taint-models-path",
            action="append",
            default=[],
            type=readable_directory,
            help="Location of taint models",
        )
        analyze.add_argument(
            "--no-verify",
            action="store_true",
            help="Do not verify models for the taint analysis.",
        )
        analyze.add_argument(
            "--save-results-to",
            default=None,
            type=writable_directory,
            help="Directory to write analysis results to.",
        )
        analyze.add_argument("--dump-call-graph", action="store_true")
        analyze.add_argument("--repository-root", type=os.path.abspath)
        analyze.add_argument("--rule", action="append", type=int)

    def generate_analysis_directory(self) -> AnalysisDirectory:
        return resolve_analysis_directory(
            self._arguments,
            self._configuration,
            self._original_directory,
            self._current_directory,
            build=True,
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
        return flags

    def _run(self, retries: int = 1) -> None:
        self._analysis_directory.prepare()
        result = self._call_client(command=self.NAME)
        result.check()
        if self._save_results_to is None:
            log.stdout.write(result.output)
