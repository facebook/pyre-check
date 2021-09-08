# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import dataclasses
import itertools
import logging
from dataclasses import dataclass
from typing import Set, Iterable, Dict, List, Sequence

import libcst as cst
from libcst.metadata import CodeRange, PositionProvider

from .statistics_collectors import FunctionAnnotationKind

LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class AnnotationInfo:
    node: cst.CSTNode
    is_annotated: bool
    code_range: CodeRange


@dataclasses.dataclass(frozen=True)
class FunctionAnnotationInfo:
    node: cst.CSTNode
    annotation_kind: FunctionAnnotationKind
    code_range: CodeRange

    @property
    def is_annotated(self) -> bool:
        return self.annotation_kind != FunctionAnnotationKind.NOT_ANNOTATED

    @property
    def is_partially_annotated(self) -> bool:
        return self.annotation_kind == FunctionAnnotationKind.PARTIALLY_ANNOTATED

    @property
    def is_fully_annotated(self) -> bool:
        return self.annotation_kind == FunctionAnnotationKind.FULLY_ANNOTATED


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


class CoverageCollector(cst.CSTVisitor):
    METADATA_DEPENDENCIES = (PositionProvider,)
    path: str = ""

    def __init__(self) -> None:
        self.returns: List[AnnotationInfo] = []
        self.globals: List[AnnotationInfo] = []
        self.parameters: List[AnnotationInfo] = []
        self.attributes: List[AnnotationInfo] = []
        self.functions: List[FunctionAnnotationInfo] = []
        self.class_definition_depth = 0
        self.function_definition_depth = 0
        self.static_function_definition_depth = 0
        self.line_count = 0

    def annotated_returns(self) -> List[AnnotationInfo]:
        return [r for r in self.returns if r.is_annotated]

    def annotated_globals(self) -> List[AnnotationInfo]:
        return [g for g in self.globals if g.is_annotated]

    def annotated_parameters(self) -> List[AnnotationInfo]:
        return [p for p in self.parameters if p.is_annotated]

    def annotated_attributes(self) -> List[AnnotationInfo]:
        return [a for a in self.attributes if a.is_annotated]

    def partially_annotated_functions(self) -> List[FunctionAnnotationInfo]:
        return [f for f in self.functions if f.is_partially_annotated]

    def fully_annotated_functions(self) -> List[FunctionAnnotationInfo]:
        return [f for f in self.functions if f.is_fully_annotated]

    def covered_functions(self) -> List[FunctionAnnotationInfo]:
        return [f for f in self.functions if f.is_annotated]

    def uncovered_functions(self) -> List[FunctionAnnotationInfo]:
        return [f for f in self.functions if not f.is_annotated]

    def build_json(self) -> Dict[str, int]:
        return {
            "return_count": len(self.returns),
            "annotated_return_count": len(self.annotated_returns()),
            "globals_count": len(self.globals),
            "annotated_globals_count": len(self.annotated_globals()),
            "parameter_count": len(self.parameters),
            "annotated_parameter_count": len(self.annotated_parameters()),
            "attribute_count": len(self.attributes),
            "annotated_attribute_count": len(self.annotated_attributes()),
            "partially_annotated_function_count": (
                len(self.partially_annotated_functions())
            ),
            "fully_annotated_function_count": len(self.fully_annotated_functions()),
            "line_count": self.line_count,
        }

    def _code_range(self, node: cst.CSTNode) -> CodeRange:
        return self.get_metadata(PositionProvider, node)

    def in_class_definition(self) -> bool:
        return self.class_definition_depth > 0

    def in_function_definition(self) -> bool:
        return self.function_definition_depth > 0

    def in_static_function_definition(self) -> bool:
        return self.static_function_definition_depth > 0

    def _is_method_or_classmethod(self) -> bool:
        return self.in_class_definition() and not self.in_static_function_definition()

    def _is_self_or_cls(self, index: int) -> bool:
        return index == 0 and self._is_method_or_classmethod()

    def _check_parameter_annotations(self, parameters: Sequence[cst.Param]) -> int:
        annotated_parameter_count = 0
        for index, parameter in enumerate(parameters):
            is_annotated = parameter.annotation is not None or self._is_self_or_cls(
                index
            )
            self.parameters.append(
                AnnotationInfo(parameter, is_annotated, self._code_range(parameter))
            )
            if is_annotated:
                annotated_parameter_count += 1
        return annotated_parameter_count

    def visit_FunctionDef(self, node: cst.FunctionDef) -> None:
        for decorator in node.decorators:
            decorator_node = decorator.decorator
            if isinstance(decorator_node, cst.Name):
                if decorator_node.value == "staticmethod":
                    self.static_function_definition_depth += 1
                    break
        self.function_definition_depth += 1

        if node.returns is None:
            code_range = self._code_range(node.whitespace_before_colon)
            return_is_annotated = False
        else:
            code_range = self._code_range(node.returns)
            return_is_annotated = True
        self.returns.append(AnnotationInfo(node, return_is_annotated, code_range))
        annotated_parameter_count = self._check_parameter_annotations(
            node.params.params
        )

        annotation_kind = FunctionAnnotationKind.from_function_data(
            return_is_annotated,
            annotated_parameter_count,
            self._is_method_or_classmethod(),
            parameters=node.params.params,
        )
        code_range = self._code_range(node.body)
        self.functions.append(FunctionAnnotationInfo(node, annotation_kind, code_range))

    def leave_FunctionDef(self, original_node: cst.FunctionDef) -> None:
        self.function_definition_depth -= 1
        for decorator in original_node.decorators:
            decorator_node = decorator.decorator
            if isinstance(decorator_node, cst.Name):
                if decorator_node.value == "staticmethod":
                    self.static_function_definition_depth -= 1
                    break

    def visit_Assign(self, node: cst.Assign) -> None:
        if self.in_function_definition():
            return
        implicitly_annotated_literal = False
        if isinstance(node.value, cst.BaseNumber) or isinstance(
            node.value, cst.BaseString
        ):
            implicitly_annotated_literal = True
        implicitly_annotated_value = False
        if isinstance(node.value, cst.Name) or isinstance(node.value, cst.Call):
            # An over-approximation of global values that do not need an explicit
            # annotation. Erring on the side of reporting these as annotated to
            # avoid showing false positives to users.
            implicitly_annotated_value = True
        code_range = self._code_range(node)
        if self.in_class_definition():
            is_annotated = implicitly_annotated_literal or implicitly_annotated_value
            self.attributes.append(AnnotationInfo(node, is_annotated, code_range))
        else:
            is_annotated = implicitly_annotated_literal or implicitly_annotated_value
            self.globals.append(AnnotationInfo(node, is_annotated, code_range))

    def visit_AnnAssign(self, node: cst.AnnAssign) -> None:
        if self.in_function_definition():
            return
        code_range = self._code_range(node)
        if self.in_class_definition():
            self.attributes.append(AnnotationInfo(node, True, code_range))
        else:
            self.globals.append(AnnotationInfo(node, True, code_range))

    def visit_ClassDef(self, node: cst.ClassDef) -> None:
        self.class_definition_depth += 1

    def leave_ClassDef(self, original_node: cst.ClassDef) -> None:
        self.class_definition_depth -= 1

    def leave_Module(self, original_node: cst.Module) -> None:
        file_range = self.get_metadata(PositionProvider, original_node)
        self.line_count = file_range.end.line

    def covered_and_uncovered_ranges(self) -> CoveredAndUncoveredRanges:
        covered_ranges = []
        uncovered_ranges = []
        for info in itertools.chain(
            self.globals, self.attributes, self.parameters, self.returns
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
