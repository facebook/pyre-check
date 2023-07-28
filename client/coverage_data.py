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

from __future__ import annotations

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

from . import dataclasses_json_extensions as json_mixins

LOG: logging.Logger = logging.getLogger(__name__)

ErrorCode: TypeAlias = int
LineNumber: TypeAlias = int


@dataclasses.dataclass(frozen=True)
class Location(json_mixins.SnakeCaseAndExcludeJsonMixin):
    start_line: int
    start_column: int
    end_line: int
    end_column: int

    @staticmethod
    def from_code_range(code_range: CodeRange) -> Location:
        return Location(
            start_line=code_range.start.line,
            start_column=code_range.start.column,
            end_line=code_range.end.line,
            end_column=code_range.end.column,
        )


@dataclasses.dataclass(frozen=True)
class AnnotationInfo:
    node: libcst.CSTNode
    is_annotated: bool
    location: Location


@dataclasses.dataclass(frozen=True)
class FunctionIdentifier(json_mixins.SnakeCaseAndExcludeJsonMixin):
    parent: Optional[str]
    name: str


@dataclasses.dataclass(frozen=True)
class ParameterAnnotationInfo(json_mixins.SnakeCaseAndExcludeJsonMixin):
    name: str
    is_annotated: bool
    location: Location


@dataclasses.dataclass(frozen=True)
class ReturnAnnotationInfo(json_mixins.SnakeCaseAndExcludeJsonMixin):
    is_annotated: bool
    location: Location


class ModuleMode(str, Enum):
    UNSAFE = "UNSAFE"
    STRICT = "STRICT"
    IGNORE_ALL = "IGNORE_ALL"


@dataclasses.dataclass(frozen=True)
class ModuleModeInfo(json_mixins.SnakeCaseAndExcludeJsonMixin):
    mode: ModuleMode
    explicit_comment_line: Optional[LineNumber]


class FunctionAnnotationStatus(str, Enum):
    NOT_ANNOTATED = "NOT_ANNOTATED"
    PARTIALLY_ANNOTATED = "PARTIALLY_ANNOTATED"
    FULLY_ANNOTATED = "FULLY_ANNOTATED"

    @staticmethod
    def from_function_data(
        is_non_static_method: bool,
        is_return_annotated: bool,
        parameters: Sequence[libcst.Param],
    ) -> "FunctionAnnotationStatus":
        if is_return_annotated:
            parameters_requiring_annotation = (
                parameters[1:] if is_non_static_method else parameters
            )
            all_parameters_annotated = all(
                parameter.annotation is not None
                for parameter in parameters_requiring_annotation
            )
            if all_parameters_annotated:
                return FunctionAnnotationStatus.FULLY_ANNOTATED
            else:
                return FunctionAnnotationStatus.PARTIALLY_ANNOTATED
        else:
            any_parameter_annotated = any(
                parameter.annotation is not None for parameter in parameters
            )
            if any_parameter_annotated:
                return FunctionAnnotationStatus.PARTIALLY_ANNOTATED
            else:
                return FunctionAnnotationStatus.NOT_ANNOTATED


@dataclasses.dataclass(frozen=True)
class FunctionAnnotationInfo(json_mixins.SnakeCaseAndExcludeJsonMixin):
    identifier: FunctionIdentifier
    location: Location
    annotation_status: FunctionAnnotationStatus
    returns: ReturnAnnotationInfo
    parameters: Sequence[ParameterAnnotationInfo]
    is_method_or_classmethod: bool

    def non_self_cls_parameters(self) -> Iterable[ParameterAnnotationInfo]:
        if self.is_method_or_classmethod:
            yield from self.parameters[1:]
        else:
            yield from self.parameters

    @property
    def is_annotated(self) -> bool:
        return self.annotation_status != FunctionAnnotationStatus.NOT_ANNOTATED

    @property
    def is_partially_annotated(self) -> bool:
        return self.annotation_status == FunctionAnnotationStatus.PARTIALLY_ANNOTATED

    @property
    def is_fully_annotated(self) -> bool:
        return self.annotation_status == FunctionAnnotationStatus.FULLY_ANNOTATED


class VisitorWithPositionData(libcst.CSTVisitor):
    """
    Mixin to use for libcst visitors that need position data.
    """

    METADATA_DEPENDENCIES = (PositionProvider,)

    def location(self, node: libcst.CSTNode) -> Location:
        return Location.from_code_range(self.get_metadata(PositionProvider, node))


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

    def get_function_identifier(self, node: libcst.FunctionDef) -> FunctionIdentifier:
        return FunctionIdentifier(
            parent=".".join(self.class_name_stack)
            if len(self.class_name_stack) > 0
            else None,
            name=node.name.value,
        )

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

    def returns(self) -> Iterable[ReturnAnnotationInfo]:
        for function in self.functions:
            yield function.returns

    def parameters(self) -> Iterable[ParameterAnnotationInfo]:
        for function in self.functions:
            yield from function.non_self_cls_parameters()

    def get_parameter_annotation_info(
        self,
        params: Sequence[libcst.Param],
    ) -> List[ParameterAnnotationInfo]:
        return [
            ParameterAnnotationInfo(
                name=node.name.value,
                is_annotated=node.annotation is not None,
                location=self.location(node),
            )
            for node in params
        ]

    def visit_ClassDef(self, node: libcst.ClassDef) -> None:
        self.context.update_for_enter_class(node)

    def leave_ClassDef(self, original_node: libcst.ClassDef) -> None:
        self.context.update_for_exit_class()

    def visit_FunctionDef(self, node: libcst.FunctionDef) -> None:
        identifier = self.context.get_function_identifier(node)
        self.context.update_for_enter_define(node)

        returns = ReturnAnnotationInfo(
            is_annotated=node.returns is not None,
            location=self.location(node.name),
        )

        parameters = self.get_parameter_annotation_info(
            params=node.params.params,
        )

        annotation_status = FunctionAnnotationStatus.from_function_data(
            is_non_static_method=self.context.is_non_static_method(),
            is_return_annotated=returns.is_annotated,
            parameters=node.params.params,
        )
        self.functions.append(
            FunctionAnnotationInfo(
                identifier,
                self.location(node),
                annotation_status,
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
        location = self.location(node)
        if self.context.assignments_are_class_level():
            is_annotated = implicitly_annotated_literal or implicitly_annotated_value
            self.attributes.append(AnnotationInfo(node, is_annotated, location))
        else:
            is_annotated = implicitly_annotated_literal or implicitly_annotated_value
            self.globals.append(AnnotationInfo(node, is_annotated, location))

    def visit_AnnAssign(self, node: libcst.AnnAssign) -> None:
        node.annotation
        if self.context.assignments_are_function_local():
            return
        location = self.location(node)
        if self.context.assignments_are_class_level():
            self.attributes.append(AnnotationInfo(node, True, location))
        else:
            self.globals.append(AnnotationInfo(node, True, location))

    def leave_Module(self, original_node: libcst.Module) -> None:
        file_range = self.get_metadata(PositionProvider, original_node)
        if original_node.has_trailing_newline:
            self.line_count = file_range.end.line
        else:
            # Seems to be a quirk in LibCST, the module CodeRange still goes 1 over
            # even when there is no trailing new line in the file.
            self.line_count = file_range.end.line - 1


class SuppressionKind(str, Enum):
    PYRE_FIXME = "PYRE_FIXME"
    PYRE_IGNORE = "PYRE_IGNORE"
    TYPE_IGNORE = "TYPE_IGNORE"


@dataclasses.dataclass(frozen=True)
class TypeErrorSuppression(json_mixins.SnakeCaseAndExcludeJsonMixin):
    kind: SuppressionKind
    location: Location
    error_codes: Optional[Sequence[ErrorCode]]


class SuppressionCollector(VisitorWithPositionData):

    suppression_regexes: Dict[SuppressionKind, str] = {
        SuppressionKind.PYRE_FIXME: r".*# *pyre-fixme(\[(\d* *,? *)*\])?",
        SuppressionKind.PYRE_IGNORE: r".*# *pyre-ignore(\[(\d* *,? *)*\])?",
        SuppressionKind.TYPE_IGNORE: r".*# *type: ignore",
    }

    def __init__(self) -> None:
        self.suppressions: List[TypeErrorSuppression] = []

    @staticmethod
    def _error_codes_from_re_group(
        match: re.Match[str],
        line: int,
    ) -> Optional[List[int]]:
        if len(match.groups()) < 1:
            code_group = None
        else:
            code_group = match.group(1)
        if code_group is None:
            return None
        code_strings = code_group.strip("[] ").split(",")
        try:
            codes = [int(code) for code in code_strings]
            return codes
        except ValueError:
            LOG.warning("Invalid error suppression code: %s", line)
            return []

    def suppression_from_comment(
        self,
        node: libcst.Comment,
    ) -> Iterable[TypeErrorSuppression]:
        location = self.location(node)
        for suppression_kind, regex in self.suppression_regexes.items():
            match = re.match(regex, node.value)
            if match is not None:
                yield TypeErrorSuppression(
                    kind=suppression_kind,
                    location=location,
                    error_codes=self._error_codes_from_re_group(
                        match=match,
                        line=location.start_line,
                    ),
                )

    def visit_Comment(self, node: libcst.Comment) -> None:
        for suppression in self.suppression_from_comment(node):
            self.suppressions.append(suppression)


class ModuleModeCollector(VisitorWithPositionData):
    unsafe_regex: Pattern[str] = compile(r" ?#+ *pyre-unsafe")
    strict_regex: Pattern[str] = compile(r" ?#+ *pyre-strict")
    ignore_all_regex: Pattern[str] = compile(r" ?#+ *pyre-ignore-all-errors")
    ignore_all_by_code_regex: Pattern[str] = compile(
        r" ?#+ *pyre-ignore-all-errors\[[0-9]+[0-9, ]*\]"
    )

    def __init__(self, strict_by_default: bool) -> None:
        self.strict_by_default: bool = strict_by_default
        # Note: the last comment will win here if there are multiple. This doesn't
        # matter for practical purposes because multiple modes produce a type error,
        # so it should be very rare to see them.
        self.mode: ModuleMode = (
            ModuleMode.STRICT if strict_by_default else ModuleMode.UNSAFE
        )
        self.explicit_comment_line: Optional[int] = None

    def is_strict_module(self) -> bool:
        return self.mode == ModuleMode.STRICT

    def visit_Comment(self, node: libcst.Comment) -> None:
        if self.strict_regex.match(node.value):
            self.mode = ModuleMode.STRICT
            self.explicit_comment_line = self.location(node).start_line
        elif self.unsafe_regex.match(node.value):
            self.mode = ModuleMode.UNSAFE
            self.explicit_comment_line = self.location(node).start_line
        elif self.ignore_all_regex.match(
            node.value
        ) and not self.ignore_all_by_code_regex.match(node.value):
            self.mode = ModuleMode.IGNORE_ALL
            self.explicit_comment_line = self.location(node).start_line


def collect_mode(
    module: libcst.MetadataWrapper,
    strict_by_default: bool,
    ignored: bool = False,  # means the module was ignored in the pyre configuration
) -> ModuleModeInfo:
    visitor = ModuleModeCollector(strict_by_default)
    module.visit(visitor)
    mode = ModuleMode.IGNORE_ALL if ignored else visitor.mode
    return ModuleModeInfo(
        mode=mode,
        explicit_comment_line=visitor.explicit_comment_line,
    )


def collect_functions(
    module: libcst.MetadataWrapper,
) -> Sequence[FunctionAnnotationInfo]:
    visitor = AnnotationCollector()
    module.visit(visitor)
    return visitor.functions


def collect_suppressions(
    module: libcst.MetadataWrapper,
) -> Sequence[TypeErrorSuppression]:
    visitor = SuppressionCollector()
    module.visit(visitor)
    return visitor.suppressions


def module_from_code(code: str) -> Optional[libcst.MetadataWrapper]:
    try:
        raw_module = libcst.parse_module(code)
        return libcst.MetadataWrapper(raw_module)
    except Exception:
        LOG.exception("Error reading code at path %s.", code)
        return None


def module_from_path(path: Path) -> Optional[libcst.MetadataWrapper]:
    try:
        return module_from_code(path.read_text())
    except FileNotFoundError:
        return None


def _is_excluded(
    path: Path,
    excludes: Sequence[str],
) -> bool:
    try:
        return any(
            [re.match(exclude_pattern, str(path)) for exclude_pattern in excludes]
        )
    except re.error:
        LOG.warning("Could not parse `excludes`: %s", excludes)
        return False


def _should_ignore(
    path: Path,
    excludes: Sequence[str],
) -> bool:
    return (
        path.suffix != ".py"
        or path.name.startswith("__")
        or path.name.startswith(".")
        or _is_excluded(path, excludes)
    )


def find_module_paths(
    paths: Iterable[Path],
    excludes: Sequence[str],
) -> List[Path]:
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

    return sorted(
        set(
            itertools.chain.from_iterable(
                _get_paths_for_file(path)
                if not path.is_dir()
                else _get_paths_in_directory(path)
                for path in paths
            )
        )
    )
