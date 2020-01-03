# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import argparse
import json
import os
from collections import defaultdict
from pathlib import Path
from re import compile
from typing import Dict, List, Optional, Pattern, Sequence

import libcst as cst

from .. import log, log_statistics
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
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
        self.is_static_function = False

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

    def _is_self(self, parameter: cst.Param) -> bool:
        return (
            parameter.name.value == "self"
            and self.in_class_definition
            and not self.is_static_function
        )

    def _check_parameter_annotations(self, parameters: Sequence[cst.Param]) -> None:
        for parameter in list(parameters):
            self.parameter_count += 1
            annotation = parameter.annotation
            if annotation is not None or self._is_self(parameter):
                self.annotated_parameter_count += 1

    def visit_FunctionDef(self, node: cst.FunctionDef) -> None:
        for decorator in node.decorators:
            decorator_node = decorator.decorator
            if isinstance(decorator_node, cst.Name):
                if decorator_node.value == "staticmethod":
                    self.is_static_function = True
        self.in_function_definition = True
        self.return_count += 1
        if node.returns is not None:
            self.annotated_return_count += 1

        self._check_parameter_annotations(node.params.default_params)
        self._check_parameter_annotations(node.params.params)

    def leave_FunctionDef(self, original_node: cst.FunctionDef) -> None:
        self.in_function_definition = False
        self.is_static_function = False

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

    def leave_ClassDef(self, original_node: cst.ClassDef) -> None:
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


class StrictCountCollector(StatisticsCollector):
    def __init__(self, strict_by_default: bool) -> None:
        self.is_strict: bool = False
        self.is_unsafe: bool = False
        self.strict_count: int = 0
        self.unsafe_count: int = 0
        self.strict_by_default: bool = strict_by_default
        self.unsafe_regex: Pattern[str] = compile(r"# pyre-unsafe")
        self.strict_regex: Pattern[str] = compile(r"# pyre-strict")

    def visit_Module(self, node: cst.Module) -> None:
        self.is_strict = False
        self.is_unsafe = False

    def visit_Comment(self, node: cst.Comment) -> None:
        strict_match = self.strict_regex.match(node.value)
        if strict_match:
            self.is_strict = True
        unsafe_match = self.unsafe_regex.match(node.value)
        if unsafe_match:
            self.is_unsafe = True

    def leave_Module(self, original_node: cst.Module) -> None:
        if self.is_unsafe:
            self.unsafe_count += 1
        elif self.is_strict or self.strict_by_default:
            self.strict_count += 1
        else:
            self.unsafe_count += 1

    def build_json(self) -> Dict[str, int]:
        return {"unsafe_count": self.unsafe_count, "strict_count": self.strict_count}


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


def _pyre_configuration_directory(local_configuration: Optional[str]) -> Path:
    if local_configuration:
        return Path(local_configuration.replace(".pyre_configuration.local", ""))
    return Path.cwd()


def _find_paths(local_configuration: Optional[str], paths: List[str]) -> List[Path]:
    pyre_configuration_directory = _pyre_configuration_directory(local_configuration)

    if paths:
        return [pyre_configuration_directory / path for path in paths]
    return [pyre_configuration_directory]


def file_exists(path: str) -> str:
    if not os.path.exists(path):
        raise argparse.ArgumentTypeError("ERROR: " + str(path) + " does not exist")
    return path


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
        self._filter_paths: List[str] = arguments.filter_paths
        self._strict: bool = self._configuration.strict

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        statistics = parser.add_parser(cls.NAME)
        statistics.set_defaults(command=cls)
        statistics.add_argument(
            "filter_paths",
            nargs="*",
            type=file_exists,
            help="Source path(s) to gather metrics for.",
        )

    def _run(self) -> None:
        self._analysis_directory.prepare()
        paths = _find_paths(self._local_configuration, self._filter_paths)
        parsed_paths = _parse_paths(paths)
        annotations = _count(parsed_paths, AnnotationCountCollector())
        fixmes = _count(parsed_paths, FixmeCountCollector())
        ignores = _count(parsed_paths, IgnoreCountCollector())
        strict_files = _count(parsed_paths, StrictCountCollector(self._strict))
        data = {
            "annotations": annotations.build_json(),
            "fixmes": fixmes.build_json(),
            "ignores": ignores.build_json(),
            "strict": strict_files.build_json(),
        }
        log.stdout.write(json.dumps(data))

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
