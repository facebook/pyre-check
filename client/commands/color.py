# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
import json
from enum import Enum
from typing import List

from .. import log
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
    ):
        self.coverage = coverage
        self.start_line = start_line
        self.stop_line = stop_line
        self.start_column = start_column
        self.stop_column = stop_column

    def __hash__(self):
        return hash(
            (self.start_line, self.stop_line, self.start_column, self.stop_column)
        )

    def __eq__(self, other: object):
        if not isinstance(other, TypeAnnotation):
            return False
        return (
            self.start_line == other.start_line
            and self.stop_line == other.stop_line
            and self.start_column == other.start_column
            and self.stop_column == other.stop_column
        )

    @staticmethod
    def create_from_json(data):
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
            coverage, start["line"], stop["line"], start["column"], stop["column"]
        )


class CoverageLevel(Enum):
    TYPED = "Typed"
    PARTIAL = "Partial"
    UNTYPED = "Untyped"


class PrintColor:
    def __init__(self, type_annotations, path):
        self.type_annotations = type_annotations  # List[TypeAnnotation]
        self.path = path

    def _add_color(self, line: str, type) -> str:
        coverage_to_color = {
            CoverageLevel.TYPED: log.Color.GREEN,
            CoverageLevel.PARTIAL: log.Color.YELLOW,
            CoverageLevel.UNTYPED: log.Color.RED,
        }
        return coverage_to_color[type] + line + log.Format.CLEAR

    def _find_types_at_line(self, line_number: int):  # List
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

    def __init__(self, arguments, configuration, analysis_directory) -> None:
        self.path = arguments.path
        super(Color, self).__init__(arguments, configuration, analysis_directory)

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
