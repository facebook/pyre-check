# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import dataclasses
from collections import defaultdict
from enum import Enum
from re import compile
from typing import Iterable, Any, Dict, List, Pattern, Sequence

import libcst as cst
from libcst.metadata import CodeRange, PositionProvider


@dataclasses.dataclass(frozen=True)
class AnnotationInfo:
    node: cst.CSTNode
    is_annotated: bool
    code_range: CodeRange


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
    def build_json(self) -> Dict[str, int]:
        return {}


class AnnotationCountCollector(StatisticsCollector, AnnotationCollector):
    def return_count(self) -> int:
        return len(list(self.returns()))

    def annotated_return_count(self) -> int:
        return len([r for r in self.returns() if r.is_annotated])

    def globals_count(self) -> int:
        return len(self.globals)

    def annotated_globals_count(self) -> int:
        return len([g for g in self.globals if g.is_annotated])

    def parameters_count(self) -> int:
        return len(list(self.parameters()))

    def annotated_parameters_count(self) -> int:
        return len([p for p in self.parameters() if p.is_annotated])

    def attributes_count(self) -> int:
        return len(self.attributes)

    def annotated_attributes_count(self) -> int:
        return len([a for a in self.attributes if a.is_annotated])

    def function_count(self) -> int:
        return len(self.functions)

    def partially_annotated_functions_count(self) -> int:
        return len([f for f in self.functions if f.is_partially_annotated])

    def fully_annotated_functions_count(self) -> int:
        return len([f for f in self.functions if f.is_fully_annotated])

    def build_json(self) -> Dict[str, int]:
        return {
            "return_count": self.return_count(),
            "annotated_return_count": self.annotated_return_count(),
            "globals_count": self.globals_count(),
            "annotated_globals_count": self.annotated_globals_count(),
            "parameter_count": self.parameters_count(),
            "annotated_parameter_count": self.annotated_parameters_count(),
            "attribute_count": self.attributes_count(),
            "annotated_attribute_count": self.annotated_attributes_count(),
            "function_count": self.function_count(),
            "partially_annotated_function_count": (
                self.partially_annotated_functions_count()
            ),
            "fully_annotated_function_count": self.fully_annotated_functions_count(),
            "line_count": self.line_count,
        }


class CountCollector(StatisticsCollector):
    def __init__(self, regex: str) -> None:
        self.counts: Dict[str, int] = defaultdict(int)
        self.regex: Pattern[str] = compile(regex)

    def visit_Comment(self, node: cst.Comment) -> None:
        match = self.regex.match(node.value)
        if match:
            code_group = match.group(1)
            if code_group:
                codes = code_group.strip("[] ").split(",")
            else:
                codes = ["No Code"]
            for code in codes:
                self.counts[code.strip()] += 1

    def build_json(self) -> Dict[str, int]:
        return dict(self.counts)


class FixmeCountCollector(CountCollector):
    def __init__(self) -> None:
        super().__init__(r".*# *pyre-fixme(\[(\d* *,? *)*\])?")


class IgnoreCountCollector(CountCollector):
    def __init__(self) -> None:
        super().__init__(r".*# *pyre-ignore(\[(\d* *,? *)*\])?")


class StrictCountCollector(StatisticsCollector):
    def __init__(self, strict_by_default: bool) -> None:
        self.is_strict: bool = False
        self.is_unsafe: bool = False
        self.strict_count: int = 0
        self.unsafe_count: int = 0
        self.strict_by_default: bool = strict_by_default
        self.unsafe_regex: Pattern[str] = compile(r" ?#+ *pyre-unsafe")
        self.strict_regex: Pattern[str] = compile(r" ?#+ *pyre-strict")
        self.ignore_all_regex: Pattern[str] = compile(r" ?#+ *pyre-ignore-all-errors")
        self.ignore_all_by_code_regex: Pattern[str] = compile(
            r" ?#+ *pyre-ignore-all-errors\[[0-9]+[0-9, ]*\]"
        )

    def is_unsafe_module(self) -> bool:
        if self.is_unsafe:
            return True
        elif self.is_strict or self.strict_by_default:
            return False
        return True

    def is_strict_module(self) -> bool:
        return not self.is_unsafe_module()

    def visit_Module(self, node: cst.Module) -> None:
        self.is_strict = False
        self.is_unsafe = False

    def visit_Comment(self, node: cst.Comment) -> None:
        if self.strict_regex.match(node.value):
            self.is_strict = True
            return
        if self.unsafe_regex.match(node.value):
            self.is_unsafe = True
            return
        if self.ignore_all_regex.match(
            node.value
        ) and not self.ignore_all_by_code_regex.match(node.value):
            self.is_unsafe = True

    def leave_Module(self, original_node: cst.Module) -> None:
        if self.is_unsafe_module():
            self.unsafe_count += 1
        else:
            self.strict_count += 1

    def build_json(self) -> Dict[str, int]:
        return {"unsafe_count": self.unsafe_count, "strict_count": self.strict_count}


class CodeQualityIssue:
    def __init__(
        self, code_range: CodeRange, path: str, category: str, message: str
    ) -> None:
        self.category: str = category
        self.detail_message: str = message
        self.line: int = code_range.start.line
        self.line_end: int = code_range.end.line
        self.column: int = code_range.start.column
        self.column_end: int = code_range.end.column
        self.path: str = path

    def build_json(self) -> Dict[str, Any]:
        return {
            "category": self.category,
            "detail_message": self.detail_message,
            "line": self.line,
            "line_end": self.line_end,
            "column": self.column,
            "column_end": self.column_end,
            "path": self.path,
        }


class FunctionsCollector(cst.CSTVisitor):
    METADATA_DEPENDENCIES = (PositionProvider,)
    path: str = ""

    def __init__(self) -> None:
        self.issues: List[CodeQualityIssue] = []

    def visit_FunctionDef(self, node: cst.FunctionDef) -> None:
        return_is_annotated = node.returns is not None
        if not return_is_annotated:
            code_range = self.get_metadata(PositionProvider, node)
            issue = CodeQualityIssue(
                code_range,
                self.path,
                "PYRE_MISSING_ANNOTATIONS",
                "This function is missing a return annotation. \
                Bodies of unannotated functions are not typechecked by Pyre.",
            )
            self.issues.append(issue)


class StrictIssueCollector(StrictCountCollector):
    METADATA_DEPENDENCIES = (PositionProvider,)
    path: str = ""
    issues: List[CodeQualityIssue] = []

    def _create_issue(self, node: cst.Module) -> None:
        file_range = self.get_metadata(PositionProvider, node)
        code_range = CodeRange(start=file_range.start, end=file_range.start)
        issue = CodeQualityIssue(
            code_range, self.path, "PYRE_STRICT", "Unsafe Pyre file."
        )
        self.issues.append(issue)

    def leave_Module(self, original_node: cst.Module) -> None:
        if self.is_unsafe_module():
            self._create_issue(original_node)
