# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


from collections import defaultdict
from re import compile
from typing import Any, Dict, List, Pattern, Sequence

import libcst as cst
from libcst.metadata import CodeRange, PositionProvider


class StatisticsCollector(cst.CSTVisitor):
    def build_json(self) -> Dict[str, int]:
        return {}


class AnnotationCountCollector(StatisticsCollector):
    METADATA_DEPENDENCIES = (PositionProvider,)

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
        partially_annotated_function_count: int = 0,
        fully_annotated_function_count: int = 0,
    ) -> None:
        self.return_count = return_count
        self.annotated_return_count = annotated_return_count
        self.globals_count = globals_count
        self.annotated_globals_count = annotated_globals_count
        self.parameter_count = parameter_count
        self.annotated_parameter_count = annotated_parameter_count
        self.attribute_count = attribute_count
        self.annotated_attribute_count = annotated_attribute_count
        self.partially_annotated_function_count = partially_annotated_function_count
        self.fully_annotated_function_count = fully_annotated_function_count
        self.class_definition_depth = 0
        self.function_definition_depth = 0
        self.static_function_definition_depth = 0
        self.line_count = 0

    def in_class_definition(self) -> bool:
        return self.class_definition_depth > 0

    def in_function_definition(self) -> bool:
        return self.function_definition_depth > 0

    def in_static_function_definition(self) -> bool:
        return self.static_function_definition_depth > 0

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
            "partially_annotated_function_count": (
                self.partially_annotated_function_count
            ),
            "fully_annotated_function_count": self.fully_annotated_function_count,
            "line_count": self.line_count,
        }

    def _is_self_or_cls(self, index: int) -> bool:
        return (
            index == 0
            and self.in_class_definition()
            and not self.in_static_function_definition()
        )

    def _check_parameter_annotations(self, parameters: Sequence[cst.Param]) -> int:
        annotated_parameter_count = 0
        for index, parameter in enumerate(parameters):
            self.parameter_count += 1
            annotation = parameter.annotation
            if annotation is not None or self._is_self_or_cls(index):
                annotated_parameter_count += 1
        self.annotated_parameter_count += annotated_parameter_count
        return annotated_parameter_count

    def visit_FunctionDef(self, node: cst.FunctionDef) -> None:
        for decorator in node.decorators:
            decorator_node = decorator.decorator
            if isinstance(decorator_node, cst.Name):
                if decorator_node.value == "staticmethod":
                    self.static_function_definition_depth += 1
                    break
        self.function_definition_depth += 1

        self.return_count += 1
        return_is_annotated = node.returns is not None
        if return_is_annotated:
            self.annotated_return_count += 1

        annotated_parameters = self._check_parameter_annotations(node.params.params)

        if return_is_annotated and (annotated_parameters == len(node.params.params)):
            self.fully_annotated_function_count += 1
        elif return_is_annotated or annotated_parameters > 0:
            self.partially_annotated_function_count += 1

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
        if self.in_class_definition():
            self.attribute_count += 1
        else:
            self.globals_count += 1

    def visit_AnnAssign(self, node: cst.AnnAssign) -> None:
        if self.in_function_definition():
            return
        if self.in_class_definition():
            self.attribute_count += 1
            self.annotated_attribute_count += 1
        else:
            self.globals_count += 1
            self.annotated_globals_count += 1

    def visit_ClassDef(self, node: cst.ClassDef) -> None:
        self.class_definition_depth += 1

    def leave_ClassDef(self, original_node: cst.ClassDef) -> None:
        self.class_definition_depth -= 1

    def leave_Module(self, original_node: cst.Module) -> None:
        file_range = self.get_metadata(PositionProvider, original_node)
        self.line_count = file_range.end.line


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
