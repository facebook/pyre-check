# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import json
from enum import Enum
from typing import Any, Dict, List, Optional, Set

from .. import log
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from .command import Command


# this needs a better name
class TypeAnnotation:
    def __init__(
        self,
        coverage: str,
        start_line: int,
        stop_line: int,
        start_column: int,
        stop_column: int,
    ) -> None:
        self.coverage = coverage
        self.start_line = start_line
        self.stop_line = stop_line
        self.start_column = start_column
        self.stop_column = stop_column

    def __hash__(self) -> int:
        return hash(
            (self.start_line, self.stop_line, self.start_column, self.stop_column)
        )

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, TypeAnnotation):
            return False
        return (
            self.start_line == other.start_line
            and self.stop_line == other.stop_line
            and self.start_column == other.start_column
            and self.stop_column == other.stop_column
        )

    @staticmethod
    def create_from_json(data: Dict[str, Any]) -> "TypeAnnotation":
        start = data["location"]["start"]
        stop = data["location"]["stop"]
        type = data["coverage"][0]

        if type == CoverageLevel.TYPED.value:
            coverage = CoverageLevel.TYPED
        elif type == CoverageLevel.PARTIAL.value:
            coverage = CoverageLevel.PARTIAL
        else:
            coverage = CoverageLevel.UNTYPED

        return TypeAnnotation(
            # pyre-fixme[6]: Expected `str` for 1st param but got `CoverageLevel`.
            coverage,
            start["line"],
            stop["line"],
            start["column"],
            stop["column"],
        )


class CoverageLevel(Enum):
    TYPED = "Typed"
    PARTIAL = "Partial"
    UNTYPED = "Untyped"


class PrintColor:
    def __init__(self, type_annotations: Set[TypeAnnotation], path: str) -> None:
        self.type_annotations = type_annotations
        self.path = path

    def _add_color(self, line: str, type: CoverageLevel) -> str:
        coverage_to_color = {
            CoverageLevel.TYPED: log.Color.GREEN,
            CoverageLevel.PARTIAL: log.Color.YELLOW,
            CoverageLevel.UNTYPED: log.Color.RED,
        }
        return coverage_to_color[type] + line + log.Format.CLEAR

    def _find_types_at_line(self, line_number: int) -> List[TypeAnnotation]:
        types_at_line = []
        for type in self.type_annotations:
            if type.start_line == line_number:
                types_at_line.append(type)
        return types_at_line

    def print_results(self) -> None:
        with open(self.path) as file:
            lines = file.read().split("\n")
            for line_number, line in enumerate(lines, start=1):
                type_annotations = self._find_types_at_line(line_number)
                type_annotations.sort(key=lambda annotation: annotation.start_column)
                for annotation in reversed(type_annotations):
                    prefix = line[: annotation.start_column]
                    string = line[annotation.start_column : annotation.stop_column]
                    suffix = line[annotation.stop_column :]
                    string_with_color = self._add_color(string, annotation.coverage)
                    line = prefix + string_with_color + suffix
                log.stdout.write(line + "\n")


class Color(Command):
    NAME = "color"

    def __init__(
        self,
        arguments: argparse.Namespace,
        original_directory: str,
        configuration: Optional[Configuration] = None,
        analysis_directory: Optional[AnalysisDirectory] = None,
    ) -> None:
        super(Color, self).__init__(
            arguments, original_directory, configuration, analysis_directory
        )
        self.path: str = arguments.path

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        color = parser.add_parser(cls.NAME)
        color.set_defaults(command=cls)
        color.add_argument("path")

    def _flags(self) -> List[str]:
        return ["coverage_in_file('" + self.path + "')"]

    def _run(self) -> None:
        result = self._call_client(command="query")
        result.check()
        if "error" in result.output:
            log.stdout.write(result.output)
            return

        # convert result to TypeAnnotation
        coverage = json.loads(result.output)
        type_annotations = [
            TypeAnnotation.create_from_json(type)
            for type in coverage["response"]["types"]
        ]
        PrintColor(set(type_annotations), self.path).print_results()
