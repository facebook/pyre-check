# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import itertools
import logging
from dataclasses import dataclass
from typing import Set, Iterable, List

import libcst as cst
from libcst.metadata import CodeRange

from .statistics_collectors import (
    AnnotationCollector,
    FunctionAnnotationInfo,
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


def collect_coverage_for_module(relative_path: str, module: cst.Module) -> FileCoverage:
    module_with_metadata = cst.MetadataWrapper(module)
    coverage_collector = CoverageCollector()
    try:
        module_with_metadata.visit(coverage_collector)
    except RecursionError:
        LOG.warning(f"LibCST encountered recursion error in `{relative_path}`")
    covered_and_uncovered_ranges = coverage_collector.covered_and_uncovered_ranges()
    covered_and_uncovered_lines = _covered_and_uncovered_ranges_to_lines(
        covered_and_uncovered_ranges
    )
    return FileCoverage(
        filepath=relative_path,
        covered_lines=sorted(covered_and_uncovered_lines.covered_lines),
        uncovered_lines=sorted(covered_and_uncovered_lines.uncovered_lines),
    )


class CoverageCollector(AnnotationCollector):
    def covered_functions(self) -> List[FunctionAnnotationInfo]:
        return [f for f in self.functions if f.is_annotated]

    def uncovered_functions(self) -> List[FunctionAnnotationInfo]:
        return [f for f in self.functions if not f.is_annotated]

    def covered_and_uncovered_ranges(self) -> CoveredAndUncoveredRanges:
        covered_ranges = []
        uncovered_ranges = []
        for info in itertools.chain(
            self.globals, self.attributes, self.parameters(), self.returns()
        ):
            if info.is_annotated:
                covered_ranges.append(info.code_range)
            else:
                uncovered_ranges.append(info.code_range)
        return CoveredAndUncoveredRanges(covered_ranges, uncovered_ranges)


def _code_ranges_to_lines(code_ranges: Iterable[CodeRange]) -> Set[int]:
    lines: Set[int] = set()
    for code_range in code_ranges:
        lines |= set(range(code_range.start.line - 1, code_range.end.line))
    return lines


def _covered_and_uncovered_ranges_to_lines(
    covered_and_uncovered_ranges: CoveredAndUncoveredRanges,
) -> CoveredAndUncoveredLines:
    covered_lines = _code_ranges_to_lines(covered_and_uncovered_ranges.covered_ranges)
    uncovered_lines = _code_ranges_to_lines(
        covered_and_uncovered_ranges.uncovered_ranges
    )
    # We show partially covered lines as covered
    return CoveredAndUncoveredLines(covered_lines, uncovered_lines - covered_lines)
