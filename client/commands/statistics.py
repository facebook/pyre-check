# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import argparse
from pathlib import Path
from typing import List

from ..configuration import Configuration
from ..count_annotations import TypeCollector, run
from ..filesystem import AnalysisDirectory
from .command import Command


def _run_count_annotations(paths: List[Path]) -> TypeCollector:
    collector = TypeCollector()
    for path in paths:
        run(path, collector)
    return collector


def _find_paths(local_configuration: str, paths: List[str]) -> List[Path]:
    if local_configuration:
        pyre_configuration_directory = Path(
            local_configuration.replace(".pyre_configuration.local", "")
        )
    else:
        pyre_configuration_directory = Path.cwd()

    if paths:
        return [pyre_configuration_directory / path for path in paths]
    return [pyre_configuration_directory]


class Statistics(Command):
    NAME = "statistics"

    def __init__(
        self,
        arguments: argparse.Namespace,
        configuration: Configuration,
        analysis_directory: AnalysisDirectory,
    ) -> None:
        super(Statistics, self).__init__(arguments, configuration, analysis_directory)
        self._local_configuration: str = arguments.local_configuration
        self._filter_paths: List[str] = arguments.filter_paths

    def _run(self) -> None:
        self._analysis_directory.prepare()
        annotations = _run_count_annotations(
            _find_paths(self._local_configuration, self._filter_paths)
        )
        annotations.print_results()
