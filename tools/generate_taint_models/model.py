# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import _ast
import abc
import ast
import logging
from typing import Callable, Iterable, List, Optional, Set, Union

from ...api import query
from .generator_specifications import (
    AnnotationSpecification,
    ParameterAnnotation,
    WhitelistSpecification,
)
from .inspect_parser import extract_parameters, extract_qualified_name
from .parameter import Parameter


FunctionDefinition = Union[_ast.FunctionDef, _ast.AsyncFunctionDef]

LOG: logging.Logger = logging.getLogger(__name__)


class Model(abc.ABC):
    def __lt__(self, other: "Model") -> bool:
        return str(self) < str(other)

    @abc.abstractmethod
    def __eq__(self) -> int:
        ...

    @abc.abstractmethod
    def __hash__(self) -> int:
        ...


class RawCallableModel(Model):
    callable_name: str
    parameters: List[Parameter]
    annotations: AnnotationSpecification
    whitelist: WhitelistSpecification
    returns: Optional[str] = None

    def __init__(
        self,
        parameter_annotation: Optional[ParameterAnnotation] = None,
        returns: Optional[str] = None,
        parameter_type_whitelist: Optional[Iterable[str]] = None,
        parameter_name_whitelist: Optional[Set[str]] = None,
        annotations: Optional[AnnotationSpecification] = None,
        whitelist: Optional[WhitelistSpecification] = None,
    ) -> None:
        if annotations:
            self.annotations = annotations
        else:
            self.annotations = AnnotationSpecification(
                parameter_annotation=parameter_annotation, returns=returns
            )

        if whitelist:
            self.whitelist = whitelist
        else:
            self.whitelist = WhitelistSpecification(
                parameter_type=set(parameter_type_whitelist)
                if parameter_type_whitelist
                else None,
                parameter_name=parameter_name_whitelist,
            )

        callable_name = self._get_fully_qualified_callable_name()
        # Object construction should fail if any child class passes in a None.
        if not callable_name or "-" in callable_name:
            raise ValueError("The callable is not supported")

        self.callable_name = callable_name
        self.parameters = self._generate_parameters()

    @abc.abstractmethod
    def _generate_parameters(self) -> List["Parameter"]:
        ...

    @abc.abstractmethod
    def _get_fully_qualified_callable_name(self) -> Optional[str]:
        ...

    def __str__(self) -> str:
        serialized_parameters = []

        name_whitelist = self.whitelist.parameter_name
        type_whitelist = self.whitelist.parameter_type
        for parameter in self.parameters:
            should_annotate = True
            if name_whitelist is not None and parameter.name in name_whitelist:
                should_annotate = False

            if type_whitelist is not None and parameter.annotation in type_whitelist:
                should_annotate = False

            if should_annotate:
                parameter_annotation = self.annotations.parameter_annotation
                if parameter_annotation is not None:
                    taint = parameter_annotation.get(parameter)
                else:
                    taint = None
            else:
                taint = None

            # * parameters indicate kwargs after the parameter position, and can't be
            # tainted. Example: `def foo(x, *, y): ...`
            if parameter.name != "*" and taint:
                serialized_parameters.append(f"{parameter.name}: {taint}")
            else:
                serialized_parameters.append(parameter.name)

        returns = self.annotations.returns
        if returns:
            return_annotation = f" -> {returns}"
        else:
            return_annotation = ""

        return (
            f"def {self.callable_name}({', '.join(serialized_parameters)})"
            f"{return_annotation}: ..."
        )

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, RawCallableModel):
            return False
        return (
            self.callable_name == other.callable_name
            and self.parameters == other.parameters
        )

    # Need to explicitly define this(despite baseclass) as we are overriding eq
    def __hash__(self) -> int:
        parameter_names_string = ",".join(
            map(
                lambda parameter: f"{parameter.name}:{parameter.annotation}"
                if parameter.annotation
                else f"{parameter.name}:_empty",
                self.parameters,
            )
        )
        return hash((self.callable_name, parameter_names_string))


class CallableModel(RawCallableModel):
    callable_object: Callable[..., object]

    def __init__(
        self,
        callable_object: Callable[..., object],
        parameter_annotation: Optional[ParameterAnnotation] = None,
        returns: Optional[str] = None,
        parameter_type_whitelist: Optional[Iterable[str]] = None,
        parameter_name_whitelist: Optional[Set[str]] = None,
        annotations: Optional[AnnotationSpecification] = None,
        whitelist: Optional[WhitelistSpecification] = None,
    ) -> None:
        self.callable_object = callable_object
        super().__init__(
            parameter_annotation=parameter_annotation,
            returns=returns,
            parameter_type_whitelist=parameter_type_whitelist,
            parameter_name_whitelist=parameter_name_whitelist,
            annotations=annotations,
            whitelist=whitelist,
        )

    def _generate_parameters(self) -> List[Parameter]:
        return extract_parameters(self.callable_object)

    def _get_fully_qualified_callable_name(self) -> Optional[str]:
        return extract_qualified_name(self.callable_object)


class FunctionDefinitionModel(RawCallableModel):
    definition: FunctionDefinition
    qualifier: Optional[str] = None

    def __init__(
        self,
        definition: FunctionDefinition,
        qualifier: Optional[str] = None,
        parameter_annotation: Optional[ParameterAnnotation] = None,
        returns: Optional[str] = None,
        parameter_type_whitelist: Optional[Iterable[str]] = None,
        parameter_name_whitelist: Optional[Set[str]] = None,
        annotations: Optional[AnnotationSpecification] = None,
        whitelist: Optional[WhitelistSpecification] = None,
    ) -> None:
        self.definition = definition
        self.qualifier = qualifier
        super().__init__(
            parameter_annotation=parameter_annotation,
            returns=returns,
            parameter_type_whitelist=parameter_type_whitelist,
            parameter_name_whitelist=parameter_name_whitelist,
            annotations=annotations,
            whitelist=whitelist,
        )

    @staticmethod
    def _get_annotation(ast_arg: ast.arg) -> Optional[str]:
        annotation = ast_arg.annotation
        if annotation and isinstance(annotation, _ast.Name):
            return annotation.id
        else:
            return None

    def _generate_parameters(self) -> List[Parameter]:
        parameters: List[Parameter] = []
        function_arguments = self.definition.args

        for ast_arg in function_arguments.args:
            parameters.append(
                Parameter(
                    ast_arg.arg,
                    FunctionDefinitionModel._get_annotation(ast_arg),
                    Parameter.Kind.ARG,
                )
            )

        keyword_only_parameters = function_arguments.kwonlyargs
        if len(keyword_only_parameters) > 0:
            parameters.append(
                Parameter(name="*", annotation=None, kind=Parameter.Kind.ARG)
            )
            for parameter in keyword_only_parameters:
                parameters.append(
                    Parameter(
                        parameter.arg,
                        FunctionDefinitionModel._get_annotation(parameter),
                        Parameter.Kind.ARG,
                    )
                )

        vararg_parameters = function_arguments.vararg
        if isinstance(vararg_parameters, ast.arg):
            parameters.append(
                Parameter(
                    f"*{vararg_parameters.arg}",
                    FunctionDefinitionModel._get_annotation(vararg_parameters),
                    Parameter.Kind.VARARG,
                )
            )

        kwarg_parameters = function_arguments.kwarg
        if isinstance(kwarg_parameters, ast.arg):
            parameters.append(
                Parameter(
                    f"**{kwarg_parameters.arg}",
                    FunctionDefinitionModel._get_annotation(kwarg_parameters),
                    Parameter.Kind.KWARG,
                )
            )

        return parameters

    def _get_fully_qualified_callable_name(self) -> Optional[str]:
        qualifier = f"{self.qualifier}." if self.qualifier else ""
        fn_name = self.definition.name
        return qualifier + fn_name


class PyreFunctionDefinitionModel(RawCallableModel):
    definition: query.Define

    def __init__(
        self,
        definition: query.Define,
        parameter_annotation: Optional[ParameterAnnotation] = None,
        returns: Optional[str] = None,
        parameter_type_whitelist: Optional[Iterable[str]] = None,
        parameter_name_whitelist: Optional[Set[str]] = None,
        annotations: Optional[AnnotationSpecification] = None,
        whitelist: Optional[WhitelistSpecification] = None,
    ) -> None:
        self.definition = definition
        super().__init__(
            parameter_annotation=parameter_annotation,
            returns=returns,
            parameter_type_whitelist=parameter_type_whitelist,
            parameter_name_whitelist=parameter_name_whitelist,
            annotations=annotations,
            whitelist=whitelist,
        )

    def _generate_parameters(self) -> List[Parameter]:
        parameters: List[Parameter] = []

        for parameter in self.definition.parameters:
            if "**" in parameter.name:
                kind = Parameter.Kind.KWARG
            elif "*" in parameter.name:
                kind = Parameter.Kind.VARARG
            else:
                kind = Parameter.Kind.ARG
            parameters.append(
                Parameter(
                    name=parameter.name, annotation=parameter.annotation, kind=kind
                )
            )

        return parameters

    def _get_fully_qualified_callable_name(self) -> Optional[str]:
        return self.definition.name


class AssignmentModel(Model):
    annotation: str
    target: str

    def __init__(self, annotation: str, target: str) -> None:
        if "-" in target:
            raise ValueError("The target is not supported")
        self.annotation = annotation
        self.target = target

    def __str__(self) -> str:
        return f"{self.target}: {self.annotation} = ..."

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, AssignmentModel):
            return False
        return self.target == other.target

    def __hash__(self) -> int:
        return hash(self.target)


class ClassModel(Model):
    class_name: str
    annotation: str

    def __init__(self, class_name: str, annotation: str) -> None:
        self.class_name = class_name
        self.annotation = annotation

    def __str__(self) -> str:
        return f"class {self.class_name}({self.annotation}): ..."

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, ClassModel):
            return False
        return self.class_name == other.class_name

    def __hash__(self) -> int:
        return hash(self.class_name)


class PropertyModel(Model):
    def __init__(self, class_name: str, attribute_name: str, annotation: str) -> None:
        self.class_name = class_name
        self.attribute_name = attribute_name
        self.annotation = annotation

    def __str__(self) -> str:
        return f"@property\ndef {self.class_name}.{self.attribute_name}(self) -> {self.annotation}: ..."  # noqa B950

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, PropertyModel):
            return False
        return (
            self.class_name == other.class_name
            and self.attribute_name == other.attribute_name
        )

    def __hash__(self) -> int:
        return hash((self.class_name, self.attribute_name))
