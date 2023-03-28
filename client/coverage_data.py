# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module defines shared logic used by Pyre coverage tooling, including
- LibCST visitors to collect coverage information, and dataclasses
  representing the resulting data.
- Helpers for determining which files correspond to modules where Pyre
  should collect coverage information.
- Helpers for parsing code into LibCST modules with position metadata
"""

import dataclasses
import itertools
import logging
import re
from enum import Enum
from pathlib import Path
from re import compile
from typing import Dict, Iterable, List, Optional, Pattern, Sequence

import libcst
from libcst.metadata import CodeRange, PositionProvider
from typing_extensions import TypeAlias

LOG: logging.Logger = logging.getLogger(__name__)

ErrorCode: TypeAlias = int
LineNumber: TypeAlias = int


@dataclasses.dataclass(frozen=True)
class AnnotationInfo:
    node: libcst.CSTNode
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
        is_non_static_method: bool,
        is_return_annotated: bool,
        parameters: Sequence[libcst.Param],
    ) -> "FunctionAnnotationKind":
        if is_return_annotated:
            parameters_requiring_annotation = (
                parameters[1:] if is_non_static_method else parameters
            )
            all_parameters_annotated = all(
                parameter.annotation is not None
                for parameter in parameters_requiring_annotation
            )
            if all_parameters_annotated:
                return FunctionAnnotationKind.FULLY_ANNOTATED
            else:
                return FunctionAnnotationKind.PARTIALLY_ANNOTATED
        else:
            any_parameter_annotated = any(
                parameter.annotation is not None for parameter in parameters
            )
            if any_parameter_annotated:
                return FunctionAnnotationKind.PARTIALLY_ANNOTATED
            else:
                return FunctionAnnotationKind.NOT_ANNOTATED


@dataclasses.dataclass(frozen=True)
class FunctionAnnotationInfo:
    node: libcst.CSTNode
    code_range: CodeRange
    annotation_kind: FunctionAnnotationKind
    returns: AnnotationInfo
    parameters: Sequence[AnnotationInfo]
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


class VisitorWithPositionData(libcst.CSTVisitor):
    """
    Mixin to use for libcst visitors that need position data.
    """

    METADATA_DEPENDENCIES = (PositionProvider,)

    def code_range(self, node: libcst.CSTNode) -> CodeRange:
        return self.get_metadata(PositionProvider, node)


class AnnotationContext:
    class_name_stack: List[str]
    define_depth: int
    static_define_depth: int

    def __init__(self) -> None:
        self.class_name_stack = []
        self.define_depth = 0
        self.static_define_depth = 0

    # Mutators to maintain context

    @staticmethod
    def _define_includes_staticmethod(define: libcst.FunctionDef) -> bool:
        for decorator in define.decorators:
            decorator_node = decorator.decorator
            if isinstance(decorator_node, libcst.Name):
                if decorator_node.value == "staticmethod":
                    return True
        return False

    def update_for_enter_define(self, define: libcst.FunctionDef) -> None:
        self.define_depth += 1
        if self._define_includes_staticmethod(define):
            self.static_define_depth += 1

    def update_for_exit_define(self, define: libcst.FunctionDef) -> None:
        self.define_depth -= 1
        if self._define_includes_staticmethod(define):
            self.static_define_depth -= 1

    def update_for_enter_class(self, classdef: libcst.ClassDef) -> None:
        self.class_name_stack.append(classdef.name.value)

    def update_for_exit_class(self) -> None:
        self.class_name_stack.pop()

    # Queries of the context

    def assignments_are_function_local(self) -> bool:
        return self.define_depth > 0

    def assignments_are_class_level(self) -> bool:
        return len(self.class_name_stack) > 0

    def is_non_static_method(self) -> bool:
        """
        Is a parameter implicitly typed? This happens in non-static methods for
        the initial parameter (conventionally `self` or `cls`).
        """
        return len(self.class_name_stack) > 0 and not self.static_define_depth > 0


class AnnotationCollector(VisitorWithPositionData):
    path: str = ""

    def __init__(self) -> None:
        self.context: AnnotationContext = AnnotationContext()
        self.globals: List[AnnotationInfo] = []
        self.attributes: List[AnnotationInfo] = []
        self.functions: List[FunctionAnnotationInfo] = []
        self.line_count = 0

    def returns(self) -> Iterable[AnnotationInfo]:
        for function in self.functions:
            yield function.returns

    def parameters(self) -> Iterable[AnnotationInfo]:
        for function in self.functions:
            yield from function.non_self_cls_parameters()

    def _parameter_annotations(
        self, parameters: Sequence[libcst.Param]
    ) -> Sequence[AnnotationInfo]:
        return [
            AnnotationInfo(
                parameter,
                parameter.annotation is not None,
                self.code_range(parameter),
            )
            for parameter in parameters
        ]

    def visit_ClassDef(self, node: libcst.ClassDef) -> None:
        self.context.update_for_enter_class(node)

    def leave_ClassDef(self, original_node: libcst.ClassDef) -> None:
        self.context.update_for_exit_class()

    def visit_FunctionDef(self, node: libcst.FunctionDef) -> None:
        self.context.update_for_enter_define(node)

        returns = AnnotationInfo(
            node,
            is_annotated=node.returns is not None,
            code_range=self.code_range(node.name),
        )

        parameters = self._parameter_annotations(node.params.params)

        annotation_kind = FunctionAnnotationKind.from_function_data(
            is_non_static_method=self.context.is_non_static_method(),
            is_return_annotated=returns.is_annotated,
            parameters=node.params.params,
        )
        self.functions.append(
            FunctionAnnotationInfo(
                node,
                self.code_range(node),
                annotation_kind,
                returns,
                parameters,
                self.context.is_non_static_method(),
            )
        )

    def leave_FunctionDef(self, original_node: libcst.FunctionDef) -> None:
        self.context.update_for_exit_define(original_node)

    def visit_Assign(self, node: libcst.Assign) -> None:
        if self.context.assignments_are_function_local():
            return
        implicitly_annotated_literal = False
        if isinstance(node.value, libcst.BaseNumber) or isinstance(
            node.value, libcst.BaseString
        ):
            implicitly_annotated_literal = True
        implicitly_annotated_value = False
        if isinstance(node.value, libcst.Name) or isinstance(node.value, libcst.Call):
            # An over-approximation of global values that do not need an explicit
            # annotation. Erring on the side of reporting these as annotated to
            # avoid showing false positives to users.
            implicitly_annotated_value = True
        code_range = self.code_range(node)
        if self.context.assignments_are_class_level():
            is_annotated = implicitly_annotated_literal or implicitly_annotated_value
            self.attributes.append(AnnotationInfo(node, is_annotated, code_range))
        else:
            is_annotated = implicitly_annotated_literal or implicitly_annotated_value
            self.globals.append(AnnotationInfo(node, is_annotated, code_range))

    def visit_AnnAssign(self, node: libcst.AnnAssign) -> None:
        node.annotation
        if self.context.assignments_are_function_local():
            return
        code_range = self.code_range(node)
        if self.context.assignments_are_class_level():
            self.attributes.append(AnnotationInfo(node, True, code_range))
        else:
            self.globals.append(AnnotationInfo(node, True, code_range))

    def leave_Module(self, original_node: libcst.Module) -> None:
        file_range = self.get_metadata(PositionProvider, original_node)
        if original_node.has_trailing_newline:
            self.line_count = file_range.end.line
        else:
            # Seems to be a quirk in LibCST, the module CodeRange still goes 1 over
            # even when there is no trailing new line in the file.
            self.line_count = file_range.end.line - 1


class StatisticsCollector(libcst.CSTVisitor):
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

    def collect(
        self,
        module: libcst.MetadataWrapper,
    ) -> ModuleAnnotationData:
        module.visit(self)
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

    def visit_Comment(self, node: libcst.Comment) -> None:
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

    def collect(
        self,
        module: libcst.MetadataWrapper,
    ) -> ModuleSuppressionData:
        module.visit(self)
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

    def visit_Comment(self, node: libcst.Comment) -> None:
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

    def leave_Module(self, original_node: libcst.Module) -> None:
        if self.is_unsafe_module():
            self.unsafe_count += 1
        else:
            self.strict_count += 1

    def collect(
        self,
        module: libcst.MetadataWrapper,
    ) -> ModuleStrictData:
        module.visit(self)
        return ModuleStrictData(
            mode=ModuleMode.UNSAFE if self.is_unsafe_module() else ModuleMode.STRICT,
            explicit_comment_line=self.explicit_unsafe_comment_line
            if self.is_unsafe_module()
            else self.explicit_strict_comment_line,
        )


def module_from_code(code: str) -> Optional[libcst.MetadataWrapper]:
    try:
        raw_module = libcst.parse_module(code)
        return libcst.MetadataWrapper(raw_module)
    except libcst.ParserSyntaxError:
        LOG.exception("Parsing failure")
        return None


def module_from_path(path: Path) -> Optional[libcst.MetadataWrapper]:
    try:
        return module_from_code(path.read_text())
    except FileNotFoundError:
        return None


def get_paths_to_collect(
    paths: Optional[Sequence[Path]],
    root: Path,
) -> Iterable[Path]:
    """
    If `paths` is None, return the project root in a singleton list.

    Otherwise, verify that every path in `paths` is a valid subpath
    of the project, and return a deduplicated list of these paths (which
    can be either directory or file paths).
    """
    if paths is None:
        return [root]
    else:
        absolute_paths = set()
        for path in paths:
            absolute_path = path if path.is_absolute() else Path.cwd() / path
            if root not in absolute_path.parents:
                raise ValueError(
                    f"`{path}` is not nested under the project at `{root}`",
                )
            absolute_paths.add(absolute_path)
        return absolute_paths


def _is_excluded(path: Path, excludes: Sequence[str]) -> bool:
    try:
        return any(
            [re.match(exclude_pattern, str(path)) for exclude_pattern in excludes]
        )
    except re.error:
        LOG.warning("Could not parse `excludes`: %s", excludes)
        return False


def _should_ignore(path: Path, excludes: Sequence[str]) -> bool:
    return (
        path.suffix != ".py"
        or path.name.startswith("__")
        or path.name.startswith(".")
        or _is_excluded(path, excludes)
    )


def find_module_paths(paths: Iterable[Path], excludes: Sequence[str]) -> Iterable[Path]:
    """
    Given a set of paths (which can be file paths or directory paths)
    where we want to collect data, return an iterable of all the module
    paths after recursively expanding directories, and ignoring directory
    exclusions specified in `excludes`.
    """

    def _get_paths_for_file(target_file: Path) -> Iterable[Path]:
        return [target_file] if not _should_ignore(target_file, excludes) else []

    def _get_paths_in_directory(target_directory: Path) -> Iterable[Path]:
        return (
            path
            for path in target_directory.glob("**/*.py")
            if not _should_ignore(path, excludes)
        )

    return itertools.chain.from_iterable(
        _get_paths_for_file(path)
        if not path.is_dir()
        else _get_paths_in_directory(path)
        for path in paths
    )
