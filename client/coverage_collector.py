# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import logging
from dataclasses import dataclass
from typing import List, Set, Tuple

import libcst as cst
from libcst.metadata import CodeRange

from .statistics_collectors import (
    AnnotationCollector,
    FunctionAnnotationInfo,
    StrictCountCollector,
)

LOG: logging.Logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class CoveredAndUncoveredRanges:
    covered_ranges: List[CodeRange]
    uncovered_ranges: List[CodeRange]


@dataclass(frozen=True)
class CoveredAndUncoveredLines:
    covered_lines: Set[int]
    uncovered_lines: Set[int]


@dataclass(frozen=True)
class FileCoverage:
    filepath: str
    covered_lines: List[int]
    uncovered_lines: List[int]


class CoverageCollector(AnnotationCollector):
    def __init__(self, is_strict: bool) -> None:
        super().__init__()
        self.is_strict = is_strict

    def covered_functions(self) -> List[FunctionAnnotationInfo]:
        if self.is_strict:
            return self.functions
        else:
            return [f for f in self.functions if f.is_annotated]

    def uncovered_functions(self) -> List[FunctionAnnotationInfo]:
        if self.is_strict:
            return []
        else:
            return [f for f in self.functions if not f.is_annotated]

    def covered_and_uncovered_lines(self) -> CoveredAndUncoveredLines:
        def num_lines(code_range_and_is_covered: Tuple[CodeRange, bool]) -> int:
            code_range, _ = code_range_and_is_covered
            return code_range.end.line - code_range.start.line + 1

        # When the code ranges are nested, we want to respect the innermost
        # one. By processing in descending order of number of lines we can
        # ensure that.
        uncovered_lines = set()
        for code_range, is_covered in sorted(
            [
                *((f.code_range, False) for f in self.uncovered_functions()),
                *((f.code_range, True) for f in self.covered_functions()),
            ],
            key=num_lines,
            reverse=True,
        ):
            if is_covered:
                uncovered_lines -= _code_range_to_lines(code_range)
            else:
                uncovered_lines |= _code_range_to_lines(code_range)
        covered_lines = set(range(0, self.line_count)) - uncovered_lines
        return CoveredAndUncoveredLines(covered_lines, uncovered_lines)


def coverage_collector_for_module(
    relative_path: str, module: cst.Module, strict_default: bool
) -> CoverageCollector:
    module_with_metadata = cst.MetadataWrapper(module)
    strict_count_collector = StrictCountCollector(strict_default)
    try:
        module_with_metadata.visit(strict_count_collector)
    except RecursionError:
        LOG.warning(f"LibCST encountered recursion error in `{relative_path}`")
    coverage_collector = CoverageCollector(strict_count_collector.is_strict_module())
    try:
        module_with_metadata.visit(coverage_collector)
    except RecursionError:
        LOG.warning(f"LibCST encountered recursion error in `{relative_path}`")
    return coverage_collector


def collect_coverage_for_module(
    relative_path: str, module: cst.Module, strict_default: bool
) -> FileCoverage:
    coverage_collector = coverage_collector_for_module(
        relative_path, module, strict_default
    )
    covered_and_uncovered_lines = coverage_collector.covered_and_uncovered_lines()
    return FileCoverage(
        filepath=relative_path,
        covered_lines=sorted(covered_and_uncovered_lines.covered_lines),
        uncovered_lines=sorted(covered_and_uncovered_lines.uncovered_lines),
    )


def _code_range_to_lines(code_range: CodeRange) -> Set[int]:
    return set(range(code_range.start.line - 1, code_range.end.line))
