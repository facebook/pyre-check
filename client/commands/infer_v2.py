# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import logging
from typing import Optional

from typing_extensions import Final

from .. import command_arguments, log
from ..analysis_directory import AnalysisDirectory, resolve_analysis_directory
from ..configuration import Configuration
from .check import Check

LOG: logging.Logger = logging.getLogger(__name__)


class Infer(Check):
    NAME = "analyze"
    ANALYSIS = "type_inference"

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        original_directory: str,
        *,
        configuration: Configuration,
        analysis_directory: Optional[AnalysisDirectory] = None,
        dump_call_graph: bool,
        repository_root: Optional[str],
        use_cache: bool,
    ) -> None:
        super(Infer, self).__init__(
            command_arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
        )
        self._dump_call_graph: bool = dump_call_graph
        self._repository_root: Final[Optional[str]] = repository_root
        self._use_cache: bool = use_cache

    def generate_analysis_directory(self) -> AnalysisDirectory:
        return resolve_analysis_directory(
            self._command_arguments.source_directories,
            self._command_arguments.targets,
            self._configuration,
            self._original_directory,
            filter_directory=self._command_arguments.filter_directory,
            isolate=True,
        )

    def _flags(self) -> list[str]:
        flags = super()._flags()
        flags.extend(["-analysis", self.ANALYSIS])
        if self._dump_call_graph:
            flags.append("-dump-call-graph")
        repository_root = self._repository_root
        if repository_root:
            flags.extend(["-repository-root", repository_root])
        if self._use_cache:
            flags.append("-use-cache")
        return flags

    def _run(self, retries: int = 1) -> None:
        self._analysis_directory.prepare()
        result = self._call_client(command=self.NAME)
        result.check()
        LOG.log(log.SUCCESS, result.output)
        LOG.log(log.SUCCESS, "Successfully ran interprocedural infer")
