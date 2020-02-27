# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import argparse
import enum
import json
import os
from pathlib import Path
from typing import Any, Dict, List, Optional, Set

import libcst as cst
from libcst._exceptions import ParserSyntaxError
from libcst.metadata import MetadataWrapper

from .. import log, log_statistics
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from ..statistics_collectors import (
    AnnotationCountCollector,
    FixmeCountCollector,
    FunctionsCollector,
    IgnoreCountCollector,
    StatisticsCollector,
    StrictCountCollector,
    StrictIssueCollector,
)
from .command import Command


def _get_paths(target_directory: Path) -> List[Path]:
    return [
        path
        for path in target_directory.glob("**/*.py")
        if not path.name.startswith("__") and not path.name.startswith(".")
    ]


def parse_path_to_module(path: Path) -> Optional[cst.Module]:
    try:
        return cst.parse_module(path.read_text())
    except ParserSyntaxError:
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


def _count(
    paths: List[cst.Module], collector: StatisticsCollector
) -> StatisticsCollector:
    for path in paths:
        path.visit(collector)
    return collector


def _pyre_configuration_directory(local_configuration: Optional[str]) -> Path:
    if local_configuration:
        return Path(local_configuration.replace(".pyre_configuration.local", ""))
    return Path.cwd()


def _find_paths(local_configuration: Optional[str], paths: Set[str]) -> List[Path]:
    pyre_configuration_directory = _pyre_configuration_directory(local_configuration)

    if paths:
        return [pyre_configuration_directory / path for path in paths]
    return [pyre_configuration_directory]


def file_exists(path: str) -> str:
    if not os.path.exists(path):
        raise argparse.ArgumentTypeError("ERROR: " + str(path) + " does not exist")
    return path


def is_strict(configuration: Path) -> bool:
    path = Path(configuration, ".pyre_configuration.local")
    json_configuration = json.loads(path.read_text())
    return json_configuration.get("strict", False)


class QualityType(enum.Enum):
    STRICT = "unstrict_files"
    MISSING_ANNOTATIONS = "missing_annotations"

    def __str__(self) -> str:
        return self.name.lower()

    def __repr__(self) -> str:
        return str(self)


class Statistics(Command):
    NAME = "statistics"

    def __init__(
        self,
        arguments: argparse.Namespace,
        original_directory: str,
        configuration: Optional[Configuration] = None,
        analysis_directory: Optional[AnalysisDirectory] = None,
    ) -> None:
        super(Statistics, self).__init__(
            arguments, original_directory, configuration, analysis_directory
        )
        self._filter_paths: Set[str] = set(arguments.filter_paths)
        self._strict: bool = self._configuration.strict
        self._collect: QualityType = arguments.collect
        self._log_results: bool = arguments.log_results

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        statistics = parser.add_parser(
            cls.NAME, epilog="Collect various syntactic metrics on type coverage."
        )
        statistics.set_defaults(command=cls)
        # TODO[T60916205]: Rename this argument, it doesn't make sense anymore
        statistics.add_argument(
            "filter_paths", nargs="*", type=file_exists, help=argparse.SUPPRESS
        )
        statistics.add_argument(
            "--collect",
            type=QualityType,
            choices=list(QualityType),
            default=None,
            help="Which code quality issue you want to generate.",
        )
        statistics.add_argument(
            "--log-results",
            type=bool,
            default=False,
            help="Log the statistics results to external tables.",
        )

    def _run(self) -> None:
        if self._collect is None:
            self._analysis_directory.prepare()
            paths = self._find_paths()
            modules = []
            for path in _parse_paths(paths):
                module = parse_path_to_module(path)
                if module is not None:
                    modules.append(module)
            annotations = _count(modules, AnnotationCountCollector())
            fixmes = _count(modules, FixmeCountCollector())
            ignores = _count(modules, IgnoreCountCollector())
            strict_files = _count(modules, StrictCountCollector(self._strict))
            data = {
                "annotations": annotations.build_json(),
                "fixmes": fixmes.build_json(),
                "ignores": ignores.build_json(),
                "strict": strict_files.build_json(),
            }
            log.stdout.write(json.dumps(data))
            if self._log_results:
                self._log_to_scuba(data)
        elif self._collect == QualityType.MISSING_ANNOTATIONS:
            self._get_missing_annotation_issues()
        elif self._collect == QualityType.STRICT:
            self._get_strict_issues()

    def _get_missing_annotation_issues(self) -> None:
        collector = FunctionsCollector()
        paths = _find_paths(self._local_configuration, self._filter_paths)
        paths = _parse_paths(paths)
        modules = {
            str(path): parse_path_to_metadata_module(path)
            for path in _parse_paths(paths)
        }
        for path, module in modules.items():
            if module is not None:
                collector.path = path
                module.visit(collector)
        issues = [issue.build_json() for issue in collector.issues]
        log.stdout.write(json.dumps(issues))

    def _get_strict_issues(self) -> None:
        collector = StrictIssueCollector(strict_by_default=False)
        paths = _find_paths(self._local_configuration, self._filter_paths)
        for configuration in paths:
            is_default_strict = is_strict(configuration)
            paths = _parse_paths([Path(configuration)])
            modules = {
                str(path): parse_path_to_metadata_module(path)
                for path in _parse_paths(paths)
            }
            for path, module in modules.items():
                if module is not None:
                    collector.path = path
                    collector.is_strict = is_default_strict
                    module.visit(collector)
        issues = [issue.build_json() for issue in collector.issues]
        log.stdout.write(json.dumps(issues))

    def _log_to_scuba(self, data: Dict[str, Any]) -> None:
        if self._configuration and self._configuration.logger:
            root = str(_pyre_configuration_directory(self._local_configuration))
            log_statistics(
                "perfpipe_pyre_annotation_counts",
                configuration=self._configuration,
                integers=data["annotations"],
                normals={"root": root},
            )
            self._log_fixmes("fixme", data["fixmes"], root)
            self._log_fixmes("ignore", data["ignores"], root)
            log_statistics(
                "perfpipe_pyre_strict_adoption",
                configuration=self._configuration,
                integers=data["strict"],
                normals={"root": root},
            )

    def _log_fixmes(self, fixme_type: str, data: Dict[str, int], root: str) -> None:
        for error_code, count in data.items():
            log_statistics(
                "perfpipe_pyre_fixme_counts",
                configuration=self._configuration,
                integers={"count": count},
                normals={"root": root, "code": error_code, "type": fixme_type},
            )

    def _find_paths(self) -> List[Path]:
        return _find_paths(self._local_configuration, self._filter_paths)
