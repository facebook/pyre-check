# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module defines a suite of libcst visitors that discover annotation
information and typing-related comments in code, and can thereby produce
various code statistics (counts of annotations, annotated functions, fixmes,
etc).
"""


import dataclasses
import logging
from enum import Enum
from re import compile
from typing import Dict, Iterable, List, Optional, Pattern, Sequence, Set, Tuple

import libcst as cst
from libcst.metadata import CodeRange, PositionProvider
from typing_extensions import TypeAlias

LOG: logging.Logger = logging.getLogger(__name__)

ErrorCode: TypeAlias = int
LineNumber: TypeAlias = int


@dataclasses.dataclass(frozen=True)
class AnnotationInfo:
    node: cst.CSTNode
    is_annotated: bool
    code_range: CodeRange


@dataclasses.dataclass(frozen=True)
class ModuleAnnotationData:
    line_count: int
    total_functions: List[CodeRange]
    partially_annotated_functions: List[CodeRange]
    fully_annotated_functions: List[CodeRange]
    total_parameters: List[CodeRange]
    annotated_parameters: List[CodeRange]
    total_returns: List[CodeRange]
    annotated_returns: List[CodeRange]
    total_globals: List[CodeRange]
    annotated_globals: List[CodeRange]
    total_attributes: List[CodeRange]
    annotated_attributes: List[CodeRange]


@dataclasses.dataclass(frozen=True)
class ModuleSuppressionData:
    code: Dict[ErrorCode, List[LineNumber]]
    no_code: List[LineNumber]


class ModuleMode(str, Enum):
    UNSAFE = "UNSAFE"
    STRICT = "STRICT"
    IGNORE_ALL = "IGNORE_ALL"


@dataclasses.dataclass(frozen=True)
class ModuleStrictData:
    mode: ModuleMode
    explicit_comment_line: Optional[LineNumber]


class FunctionAnnotationKind(Enum):
    NOT_ANNOTATED = 0
    PARTIALLY_ANNOTATED = 1
    FULLY_ANNOTATED = 2

    @staticmethod
    def from_function_data(
        is_return_annotated: bool,
        annotated_parameter_count: int,
        is_method_or_classmethod: bool,
        parameters: Sequence[cst.Param],
    ) -> "FunctionAnnotationKind":
        if is_return_annotated and annotated_parameter_count == len(parameters):
            return FunctionAnnotationKind.FULLY_ANNOTATED

        if is_return_annotated:
            return FunctionAnnotationKind.PARTIALLY_ANNOTATED

        has_untyped_self_parameter = is_method_or_classmethod and (
            len(parameters) > 0 and parameters[0].annotation is None
        )

        # Note: Untyped self parameters don't count towards making the function
        # partially-annotated. This is because, if there is no return type, we
        # will skip typechecking that function. So, even though `self` is
        # considered an implicitly-annotated parameter, we expect at least one
        # explicitly-annotated parameter for the function to be typechecked.
        threshold_for_partial_annotation = 1 if has_untyped_self_parameter else 0

        if annotated_parameter_count > threshold_for_partial_annotation:
            return FunctionAnnotationKind.PARTIALLY_ANNOTATED

        return FunctionAnnotationKind.NOT_ANNOTATED


@dataclasses.dataclass(frozen=True)
class FunctionAnnotationInfo:
    node: cst.CSTNode
    annotation_kind: FunctionAnnotationKind
    code_range: CodeRange

    returns: AnnotationInfo
    parameters: List[AnnotationInfo]
    is_method_or_classmethod: bool

    def non_self_cls_parameters(self) -> Iterable[AnnotationInfo]:
        if self.is_method_or_classmethod:
            yield from self.parameters[1:]
        else:
            yield from self.parameters

    @property
    def is_annotated(self) -> bool:
        return self.annotation_kind != FunctionAnnotationKind.NOT_ANNOTATED

    @property
    def is_partially_annotated(self) -> bool:
        return self.annotation_kind == FunctionAnnotationKind.PARTIALLY_ANNOTATED

    @property
    def is_fully_annotated(self) -> bool:
        return self.annotation_kind == FunctionAnnotationKind.FULLY_ANNOTATED


class AnnotationCollector(cst.CSTVisitor):
    METADATA_DEPENDENCIES = (PositionProvider,)
    path: str = ""

    def __init__(self) -> None:
        self.globals: List[AnnotationInfo] = []
        self.attributes: List[AnnotationInfo] = []
        self.functions: List[FunctionAnnotationInfo] = []
        self.class_definition_depth = 0
        self.function_definition_depth = 0
        self.static_function_definition_depth = 0
        self.line_count = 0

    def returns(self) -> Iterable[AnnotationInfo]:
        for function in self.functions:
            yield function.returns

    def parameters(self) -> Iterable[AnnotationInfo]:
        for function in self.functions:
            yield from function.non_self_cls_parameters()

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

    def _code_range(self, node: cst.CSTNode) -> CodeRange:
        return self.get_metadata(PositionProvider, node)

    def _parameter_annotations(
        self, parameters: Sequence[cst.Param]
    ) -> Iterable[AnnotationInfo]:
        for index, parameter in enumerate(parameters):
            is_annotated = parameter.annotation is not None or self._is_self_or_cls(
                index
            )
            yield AnnotationInfo(parameter, is_annotated, self._code_range(parameter))

    def visit_FunctionDef(self, node: cst.FunctionDef) -> None:
        for decorator in node.decorators:
            decorator_node = decorator.decorator
            if isinstance(decorator_node, cst.Name):
                if decorator_node.value == "staticmethod":
                    self.static_function_definition_depth += 1
                    break
        self.function_definition_depth += 1

        returns = AnnotationInfo(
            node,
            is_annotated=node.returns is not None,
            code_range=self._code_range(node.name),
        )

        parameters = []
        annotated_parameter_count = 0
        for parameter_info in self._parameter_annotations(node.params.params):
            if parameter_info.is_annotated:
                annotated_parameter_count += 1
            parameters.append(parameter_info)

        annotation_kind = FunctionAnnotationKind.from_function_data(
            returns.is_annotated,
            annotated_parameter_count,
            self._is_method_or_classmethod(),
            parameters=node.params.params,
        )
        self.functions.append(
            FunctionAnnotationInfo(
                node,
                annotation_kind,
                self._code_range(node),
                returns,
                parameters,
                self._is_method_or_classmethod(),
            )
        )

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
        if original_node.has_trailing_newline:
            self.line_count = file_range.end.line
        else:
            # Seems to be a quirk in LibCST, the module CodeRange still goes 1 over
            # even when there is no trailing new line in the file.
            self.line_count = file_range.end.line - 1


class StatisticsCollector(cst.CSTVisitor):
    pass


class AnnotationCountCollector(StatisticsCollector, AnnotationCollector):
    def partially_annotated_functions(self) -> List[FunctionAnnotationInfo]:
        return [f for f in self.functions if f.is_partially_annotated]

    def fully_annotated_functions(self) -> List[FunctionAnnotationInfo]:
        return [f for f in self.functions if f.is_fully_annotated]

    def annotated_parameters(self) -> List[AnnotationInfo]:
        return [p for p in self.parameters() if p.is_annotated]

    def annotated_returns(self) -> List[AnnotationInfo]:
        return [r for r in self.returns() if r.is_annotated]

    def annotated_globals(self) -> List[AnnotationInfo]:
        return [g for g in self.globals if g.is_annotated]

    def annotated_attributes(self) -> List[AnnotationInfo]:
        return [a for a in self.attributes if a.is_annotated]

    def build_result(self) -> ModuleAnnotationData:
        return ModuleAnnotationData(
            line_count=self.line_count,
            total_functions=[function.code_range for function in self.functions],
            partially_annotated_functions=[
                function.code_range for function in self.partially_annotated_functions()
            ],
            fully_annotated_functions=[
                function.code_range for function in self.fully_annotated_functions()
            ],
            total_parameters=[p.code_range for p in list(self.parameters())],
            annotated_parameters=[
                p.code_range for p in self.parameters() if p.is_annotated
            ],
            total_returns=[r.code_range for r in list(self.returns())],
            annotated_returns=[r.code_range for r in self.annotated_returns()],
            total_globals=[g.code_range for g in self.globals],
            annotated_globals=[g.code_range for g in self.annotated_globals()],
            total_attributes=[a.code_range for a in self.attributes],
            annotated_attributes=[a.code_range for a in self.annotated_attributes()],
        )

    @staticmethod
    def get_result_counts(result: ModuleAnnotationData) -> Dict[str, int]:
        return {
            "return_count": len(result.total_returns),
            "annotated_return_count": len(result.annotated_returns),
            "globals_count": len(result.total_globals),
            "annotated_globals_count": len(result.annotated_globals),
            "parameter_count": len(result.total_parameters),
            "annotated_parameter_count": len(result.annotated_parameters),
            "attribute_count": len(result.total_attributes),
            "annotated_attribute_count": len(result.annotated_attributes),
            "function_count": len(result.total_functions),
            "partially_annotated_function_count": len(
                result.partially_annotated_functions
            ),
            "fully_annotated_function_count": len(result.fully_annotated_functions),
            "line_count": result.line_count,
        }


class SuppressionCountCollector(StatisticsCollector):
    METADATA_DEPENDENCIES = (PositionProvider,)

    def __init__(self, regex: str) -> None:
        self.no_code: List[int] = []
        self.codes: Dict[int, List[int]] = {}
        self.regex: Pattern[str] = compile(regex)

    def error_codes(self, line: str) -> Optional[List[ErrorCode]]:
        match = self.regex.match(line)
        if match is None:
            # No suppression on line
            return None
        code_group = match.group(1)
        if code_group is None:
            # Code-less error suppression
            return []
        code_strings = code_group.strip("[] ").split(",")
        try:
            codes = [int(code) for code in code_strings]
            return codes
        except ValueError:
            LOG.warning("Invalid error suppression code: %s", line)
            return []

    def visit_Comment(self, node: cst.Comment) -> None:
        error_codes = self.error_codes(node.value)
        if error_codes is None:
            return
        suppression_line = self.get_metadata(PositionProvider, node).start.line
        if len(error_codes) == 0:
            self.no_code.append(suppression_line)
            return
        for code in error_codes:
            if code in self.codes:
                self.codes[code].append(suppression_line)
            else:
                self.codes[code] = [suppression_line]

    def build_result(self) -> ModuleSuppressionData:
        return ModuleSuppressionData(code=self.codes, no_code=self.no_code)


class FixmeCountCollector(SuppressionCountCollector):
    def __init__(self) -> None:
        super().__init__(r".*# *pyre-fixme(\[(\d* *,? *)*\])?")


class IgnoreCountCollector(SuppressionCountCollector):
    def __init__(self) -> None:
        super().__init__(r".*# *pyre-ignore(\[(\d* *,? *)*\])?")


class TypeIgnoreCountCollector(SuppressionCountCollector):
    def __init__(self) -> None:
        super().__init__(r".*# *type: ignore")


class StrictCountCollector(StatisticsCollector):
    METADATA_DEPENDENCIES = (PositionProvider,)
    unsafe_regex: Pattern[str] = compile(r" ?#+ *pyre-unsafe")
    strict_regex: Pattern[str] = compile(r" ?#+ *pyre-strict")
    ignore_all_regex: Pattern[str] = compile(r" ?#+ *pyre-ignore-all-errors")
    ignore_all_by_code_regex: Pattern[str] = compile(
        r" ?#+ *pyre-ignore-all-errors\[[0-9]+[0-9, ]*\]"
    )

    def __init__(self, strict_by_default: bool) -> None:
        self.strict_by_default: bool = strict_by_default
        self.explicit_strict_comment_line: Optional[int] = None
        self.explicit_unsafe_comment_line: Optional[int] = None
        self.strict_count: int = 0
        self.unsafe_count: int = 0

    def is_unsafe_module(self) -> bool:
        if self.explicit_unsafe_comment_line is not None:
            return True
        elif self.explicit_strict_comment_line is not None or self.strict_by_default:
            return False
        return True

    def is_strict_module(self) -> bool:
        return not self.is_unsafe_module()

    def visit_Comment(self, node: cst.Comment) -> None:
        if self.strict_regex.match(node.value):
            self.explicit_strict_comment_line = self.get_metadata(
                PositionProvider, node
            ).start.line
            return
        if self.unsafe_regex.match(node.value):
            self.explicit_unsafe_comment_line = self.get_metadata(
                PositionProvider, node
            ).start.line
            return
        if self.ignore_all_regex.match(
            node.value
        ) and not self.ignore_all_by_code_regex.match(node.value):
            self.explicit_unsafe_comment_line = self.get_metadata(
                PositionProvider, node
            ).start.line

    def leave_Module(self, original_node: cst.Module) -> None:
        if self.is_unsafe_module():
            self.unsafe_count += 1
        else:
            self.strict_count += 1

    def build_result(self) -> ModuleStrictData:
        return ModuleStrictData(
            mode=ModuleMode.UNSAFE if self.is_unsafe_module() else ModuleMode.STRICT,
            explicit_comment_line=self.explicit_unsafe_comment_line
            if self.is_unsafe_module()
            else self.explicit_strict_comment_line,
        )


@dataclasses.dataclass(frozen=True)
class CoveredAndUncoveredRanges:
    covered_ranges: List[CodeRange]
    uncovered_ranges: List[CodeRange]


@dataclasses.dataclass(frozen=True)
class CoveredAndUncoveredLines:
    covered_lines: Set[int]
    uncovered_lines: Set[int]


@dataclasses.dataclass(frozen=True)
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
