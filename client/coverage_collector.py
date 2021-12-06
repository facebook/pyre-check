# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import logging
from dataclasses import dataclass
from enum import Enum
from typing import Set, Iterable, List

import libcst as cst
from libcst.metadata import CodeRange

from .statistics_collectors import (
    AnnotationCollector,
    FunctionAnnotationInfo,
)

LOG: logging.Logger = logging.getLogger(__name__)


class AnnotationKind(Enum):
    RETURN = 0
    PARAMETER = 1
    GLOBAL = 2
    ATTRIBUTE = 3


@dataclass(frozen=True)
class CodeRangeAndAnnotationKind:
    code_range: CodeRange
    kind: AnnotationKind


@dataclass(frozen=True)
class CoveredAndUncovered:
    covered: List[CodeRangeAndAnnotationKind]
    uncovered: List[CodeRangeAndAnnotationKind]


@dataclass(frozen=True)
class CoveredAndUncoveredLines:
    covered_lines: Set[int]
    uncovered_lines: Set[int]


@dataclass(frozen=True)
class FileCoverage:
    filepath: str
    covered_lines: List[int]
    uncovered_lines: List[int]


def coverage_for_module(relative_path: str, module: cst.Module) -> CoveredAndUncovered:
    module_with_metadata = cst.MetadataWrapper(module)
    coverage_collector = CoverageCollector()
    try:
        module_with_metadata.visit(coverage_collector)
    except RecursionError:
        LOG.warning(f"LibCST encountered recursion error in `{relative_path}`")
    return coverage_collector.covered_and_uncovered()


def collect_coverage_for_module(relative_path: str, module: cst.Module) -> FileCoverage:
    covered_and_uncovered_lines = _to_covered_and_uncovered_lines(
        coverage_for_module(relative_path, module)
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

    def covered_and_uncovered(self) -> CoveredAndUncovered:
        covered = []
        uncovered = []
        for info in self.globals:
            (covered if info.is_annotated else uncovered).append(
                CodeRangeAndAnnotationKind(info.code_range, AnnotationKind.GLOBAL)
            )
        for info in self.attributes:
            (covered if info.is_annotated else uncovered).append(
                CodeRangeAndAnnotationKind(info.code_range, AnnotationKind.ATTRIBUTE)
            )
        for info in self.parameters():
            (covered if info.is_annotated else uncovered).append(
                CodeRangeAndAnnotationKind(info.code_range, AnnotationKind.PARAMETER)
            )
        for info in self.returns():
            (covered if info.is_annotated else uncovered).append(
                CodeRangeAndAnnotationKind(info.code_range, AnnotationKind.RETURN)
            )
        return CoveredAndUncovered(covered, uncovered)


def _code_ranges_to_lines(code_ranges: Iterable[CodeRange]) -> Set[int]:
    lines: Set[int] = set()
    for code_range in code_ranges:
        lines |= set(range(code_range.start.line - 1, code_range.end.line))
    return lines


def _to_covered_and_uncovered_lines(
    covered_and_uncovered: CoveredAndUncovered,
) -> CoveredAndUncoveredLines:
    covered_lines = _code_ranges_to_lines(
        c.code_range for c in covered_and_uncovered.covered
    )
    uncovered_lines = _code_ranges_to_lines(
        u.code_range for u in covered_and_uncovered.uncovered
    )
    # We show partially covered lines as covered
    return CoveredAndUncoveredLines(covered_lines, uncovered_lines - covered_lines)
