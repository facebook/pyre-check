# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import json
import logging
import time
from pathlib import Path
from typing import Any, Dict, List, Mapping, Optional, Set, Type, Union

import libcst as cst
from libcst._exceptions import ParserSyntaxError
from libcst.metadata import MetadataWrapper

from .. import command_arguments, log, statistics
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from ..statistics_collectors import (
    AnnotationCountCollector,
    FixmeCountCollector,
    IgnoreCountCollector,
    StatisticsCollector,
    StrictCountCollector,
)
from .command import Command

LOG: logging.Logger = logging.getLogger(__name__)


def _get_paths(target_directory: Path) -> List[Path]:
    return [
        path
        for path in target_directory.glob("**/*.py")
        if not path.name.startswith("__") and not path.name.startswith(".")
    ]


def parse_path_to_module(path: Path) -> Optional[cst.Module]:
    try:
        return cst.parse_module(path.read_text())
    except (ParserSyntaxError, FileNotFoundError):
        return None


def parse_path_to_metadata_module(path: Path) -> Optional[MetadataWrapper]:
    module = parse_path_to_module(path)
    if module is not None:
        return MetadataWrapper(module)


def _parse_paths(paths: List[Path]) -> List[Path]:
    parsed_paths = []
    for path in paths:
        if path.is_dir():
            parsed_directory_paths = _get_paths(path)
            for path in parsed_directory_paths:
                parsed_paths.append(path)
        else:
            parsed_paths.append(path)
    return parsed_paths


def _path_wise_counts(
    paths: Mapping[str, Union[cst.Module, cst.MetadataWrapper]],
    collector_class: Type[StatisticsCollector],
    strict_by_default: bool = False,
) -> Dict[str, StatisticsCollector]:
    collected_counts = {}
    for path, module in paths.items():
        collector = (
            StrictCountCollector(strict_by_default)
            if collector_class == StrictCountCollector
            else collector_class()
        )
        try:
            module.visit(collector)
            collected_counts[str(path)] = collector
        except RecursionError:
            LOG.warning(f"LibCST encountered recursion error in `{path}`")
    return collected_counts


def _pyre_configuration_directory(local_configuration: Optional[str]) -> Path:
    if local_configuration:
        return Path(local_configuration.replace(".pyre_configuration.local", ""))
    return Path.cwd()


def _find_paths(local_configuration: Optional[str], paths: Set[str]) -> List[Path]:
    pyre_configuration_directory = _pyre_configuration_directory(local_configuration)
    if paths:
        return [Path(path).absolute() for path in paths]
    return [pyre_configuration_directory]


class Statistics(Command):
    NAME = "statistics"

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        original_directory: str,
        *,
        configuration: Configuration,
        analysis_directory: Optional[AnalysisDirectory] = None,
        filter_paths: List[str],
        log_results: bool,
    ) -> None:
        super(Statistics, self).__init__(
            command_arguments, original_directory, configuration, analysis_directory
        )
        self._filter_paths: Set[str] = set(filter_paths)
        self._log_results: bool = log_results

    def _collect_statistics(self, modules: Mapping[str, cst.Module]) -> Dict[str, Any]:
        modules_with_metadata: Mapping[str, cst.MetadataWrapper] = {
            path: MetadataWrapper(module) for path, module in modules.items()
        }
        annotations = _path_wise_counts(modules_with_metadata, AnnotationCountCollector)
        fixmes = _path_wise_counts(modules, FixmeCountCollector)
        ignores = _path_wise_counts(modules, IgnoreCountCollector)
        strict_files = _path_wise_counts(
            modules,
            StrictCountCollector,
            self._configuration.strict,
        )
        return {
            "annotations": {
                path: counts.build_json() for path, counts in annotations.items()
            },
            "fixmes": {path: counts.build_json() for path, counts in fixmes.items()},
            "ignores": {path: counts.build_json() for path, counts in ignores.items()},
            "strict": {
                path: counts.build_json() for path, counts in strict_files.items()
            },
        }

    def _run(self) -> None:
        log_identifier = self._command_arguments.log_identifier
        run_id = log_identifier if log_identifier is not None else str(time.time_ns())
        paths = self._find_paths()
        modules = {}
        for path in _parse_paths(paths):
            module = parse_path_to_module(path)
            if module is not None:
                modules[path] = module
        data = self._collect_statistics(modules)
        log.stdout.write(json.dumps(data, indent=4))
        if self._log_results:
            self._log_to_scuba(run_id, data)

    def _log_to_scuba(self, run_id: str, data: Dict[str, Any]) -> None:
        if self._configuration and self._configuration.logger:
            for path, counts in data["annotations"].items():
                statistics.log_with_configuration(
                    statistics.LoggerCategory.ANNOTATION_COUNTS,
                    configuration=self._configuration,
                    integers=counts,
                    normals={"run_id": run_id, "path": path},
                )
            for path, counts in data["fixmes"].items():
                self._log_fixmes(run_id, "fixme", counts, path)
            for path, counts in data["ignores"].items():
                self._log_fixmes(run_id, "ignore", counts, path)
            for path, counts in data["strict"].items():
                statistics.log_with_configuration(
                    statistics.LoggerCategory.STRICT_ADOPTION,
                    configuration=self._configuration,
                    integers=counts,
                    normals={"run_id": run_id, "path": path},
                )

    def _log_fixmes(
        self, run_id: str, fixme_type: str, data: Dict[str, int], path: str
    ) -> None:
        for error_code, count in data.items():
            statistics.log_with_configuration(
                statistics.LoggerCategory.FIXME_COUNTS,
                configuration=self._configuration,
                integers={"count": count},
                normals={
                    "run_id": run_id,
                    "code": error_code,
                    "type": fixme_type,
                    "path": path,
                },
            )

    def _find_paths(self) -> List[Path]:
        return _find_paths(self._configuration.local_root, self._filter_paths)
