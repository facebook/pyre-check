# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import argparse
import json
from collections import defaultdict
from pathlib import Path
from re import compile
from typing import Dict, List, Pattern, Sequence

import libcst as cst

from .. import log
from ..configuration import Configuration
from ..filesystem import AnalysisDirectory
from .command import Command


class StatisticsCollector(cst.CSTVisitor):
    def build_json(self) -> Dict[str, int]:
        return {}


class AnnotationCountCollector(StatisticsCollector):
    def __init__(
        self,
        return_count: int = 0,
        annotated_return_count: int = 0,
        globals_count: int = 0,
        annotated_globals_count: int = 0,
        parameter_count: int = 0,
        annotated_parameter_count: int = 0,
        attribute_count: int = 0,
        annotated_attribute_count: int = 0,
    ) -> None:
        self.return_count = return_count
        self.annotated_return_count = annotated_return_count
        self.globals_count = globals_count
        self.annotated_globals_count = annotated_globals_count
        self.parameter_count = parameter_count
        self.annotated_parameter_count = annotated_parameter_count
        self.attribute_count = attribute_count
        self.annotated_attribute_count = annotated_attribute_count
        self.in_class_definition = False
        self.in_function_definition = False

    def build_json(self) -> Dict[str, int]:
        return {
            "return_count": self.return_count,
            "annotated_return_count": self.annotated_return_count,
            "globals_count": self.globals_count,
            "annotated_globals_count": self.annotated_globals_count,
            "parameter_count": self.parameter_count,
            "annotated_parameter_count": self.annotated_parameter_count,
            "attribute_count": self.attribute_count,
            "annotated_attribute_count": self.annotated_attribute_count,
        }

    def _check_parameter_annotations(self, parameters: Sequence[cst.Param]) -> None:
        for parameter in list(parameters):
            self.parameter_count += 1
            annotation = parameter.annotation
            if annotation is not None or parameter.name.value == "self":
                self.annotated_parameter_count += 1

    def visit_FunctionDef(self, node: cst.FunctionDef) -> None:
        self.in_function_definition = True
        self.return_count += 1
        if node.returns is not None:
            self.annotated_return_count += 1

        self._check_parameter_annotations(node.params.default_params)
        self._check_parameter_annotations(node.params.params)

    def leave_FunctionDef(self, node: cst.FunctionDef) -> None:
        self.in_function_definition = False

    def visit_Assign(self, node: cst.Assign) -> None:
        if self.in_function_definition:
            return
        if self.in_class_definition:
            self.attribute_count += 1
        else:
            self.globals_count += 1

    def visit_AnnAssign(self, node: cst.AnnAssign) -> None:
        if self.in_function_definition:
            return
        if self.in_class_definition:
            self.attribute_count += 1
            self.annotated_attribute_count += 1
        else:
            self.globals_count += 1
            self.annotated_globals_count += 1

    def visit_ClassDef(self, node: cst.ClassDef) -> None:
        self.in_class_definition = True

    def leave_ClassDef(self, node: cst.ClassDef) -> None:
        self.in_class_definition = False


class CountCollector(StatisticsCollector):
    def __init__(self, regex: str) -> None:
        self.counts: Dict[str, int] = defaultdict(int)
        self.regex: Pattern[str] = compile(regex)

    def visit_Comment(self, node: cst.Comment) -> None:
        match = self.regex.match(node.value)
        if match:
            self.counts[match.group(1)] += 1

    def build_json(self) -> Dict[str, int]:
        return dict(self.counts)


class FixmeCountCollector(CountCollector):
    def __init__(self) -> None:
        super().__init__(r"# pyre-fixme\[(\d*)\]:")


class IgnoreCountCollector(CountCollector):
    def __init__(self) -> None:
        super().__init__(r"# pyre-ignore\[(\d*)\]:")


def _get_paths(target_directory: Path) -> List[Path]:
    return [
        path
        for path in target_directory.glob("**/*.py")
        if not path.name.startswith("__") and not path.name.startswith(".")
    ]


def _parse_directory(directory: Path) -> List[cst.Module]:
    files = _get_paths(directory)
    new_files = []
    for file in files:
        new_files.append(_parse_file(file))
    return new_files


def _parse_file(path: Path) -> cst.Module:
    return cst.parse_module(path.read_text())


def _parse_paths(paths: List[Path]) -> List[cst.Module]:
    parsed_paths = []
    for path in paths:
        if path.is_dir():
            parsed_directory_paths = _parse_directory(path)
            for path in parsed_directory_paths:
                parsed_paths.append(path)
        else:
            parsed_paths.append(_parse_file(path))
    return parsed_paths


def _count(
    paths: List[cst.Module], collector: StatisticsCollector
) -> StatisticsCollector:
    for path in paths:
        path.visit(collector)
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
        paths = _find_paths(self._local_configuration, self._filter_paths)
        parsed_paths = _parse_paths(paths)
        annotations = _count(parsed_paths, AnnotationCountCollector())
        fixmes = _count(parsed_paths, FixmeCountCollector())
        ignores = _count(parsed_paths, IgnoreCountCollector())
        as_json = {
            "annotations": annotations.build_json(),
            "fixmes": fixmes.build_json(),
            "ignores": ignores.build_json(),
        }
        log.stdout.write(json.dumps(as_json))
