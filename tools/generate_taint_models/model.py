# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
import inspect
import logging
import types
from typing import Callable, Iterable, List, Mapping, NamedTuple, Optional, Set, Union

import _ast

from .inspect_parser import extract_annotation, extract_name, extract_qualified_name


FunctionDefinition = Union[_ast.FunctionDef, _ast.AsyncFunctionDef]

LOG: logging.Logger = logging.getLogger(__name__)


class Model:
    def __lt__(self, other: "Model") -> bool:
        return str(self) < str(other)

    def __hash__(self) -> int:
        return hash(str(self))


class RawCallableModel(Model):
    class Parameter(NamedTuple):
        name: str
        taint: Optional[str]

        def __eq__(self, other: "RawCallableModel.Parameter") -> bool:
            if not isinstance(other, self.__class__):
                return False
            return self.name == other.name

    callable_name: str
    parameters: List[Parameter]
    returns: Optional[str] = None

    def __init__(
        self,
        callable_name: Optional[str],
        parameters: List[Parameter],
        returns: Optional[str] = None,
    ) -> None:
        # Object construction should fail if any child class passes in a None.
        if not callable_name or "-" in callable_name:
            raise ValueError("The callable is not supported")
        self.callable_name = callable_name
        self.parameters = parameters
        self.returns = returns

    def __str__(self) -> str:
        serialized_parameters = []
        for parameter_name, taint in self.parameters:
            if taint:
                serialized_parameters.append(f"{parameter_name}: {taint}")
            else:
                serialized_parameters.append(parameter_name)
        returns = self.returns
        if returns:
            return_annotation = f" -> {returns}"
        else:
            return_annotation = ""

        return (
            f"def {self.callable_name}({', '.join(serialized_parameters)})"
            f"{return_annotation}: ..."
        )

    def __eq__(self, other: "RawCallableModel") -> bool:
        if not isinstance(other, RawCallableModel):
            return False
        return (
            self.callable_name == other.callable_name
            and self.returns == other.returns
            and self.parameters == other.parameters
        )

    # Need to explicitly define this(despite baseclass) as we are overriding eq
    def __hash__(self) -> int:
        parameter_names_string = ",".join(
            map(lambda parameter: parameter.name, self.parameters)
        )
        return hash((self.callable_name, self.returns, parameter_names_string))


class CallableModel(RawCallableModel):
    callable_object: Callable[..., object]
    whitelisted_parameters: Optional[Iterable[str]]
    parameter_name_whitelist: Optional[Set[str]]

    def __init__(
        self,
        callable_object: Callable[..., object],
        arg: Optional[str] = None,
        vararg: Optional[str] = None,
        kwarg: Optional[str] = None,
        returns: Optional[str] = None,
        whitelisted_parameters: Optional[Iterable[str]] = None,
        parameter_name_whitelist: Optional[Set[str]] = None,
    ) -> None:
        self.callable_object = callable_object
        self.arg = arg
        self.vararg = vararg
        self.kwarg = kwarg
        self.whitelisted_parameters = whitelisted_parameters
        self.parameter_name_whitelist = parameter_name_whitelist
        view_parameters = CallableModel._get_view_parameters(callable_object)
        super().__init__(
            callable_name=extract_qualified_name(callable_object),
            parameters=self._generate_parameters(view_parameters),
            returns=returns,
        )

    @staticmethod
    def _get_view_parameters(
        callable_object: Callable[..., object]
    ) -> Mapping[str, inspect.Parameter]:
        view_parameters: Mapping[str, inspect.Parameter] = {}
        if isinstance(callable_object, types.FunctionType):
            view_parameters = inspect.signature(callable_object).parameters
        elif isinstance(callable_object, types.MethodType):
            # pyre-ignore: Too dynamic
            view_parameters = inspect.signature(callable_object.__func__).parameters
        return view_parameters

    def _generate_parameters(
        self, view_parameters: Mapping[str, inspect.Parameter]
    ) -> List[RawCallableModel.Parameter]:
        parameters: List[RawCallableModel.Parameter] = []
        name_whitelist = self.parameter_name_whitelist

        for parameter_name in view_parameters:
            parameter = view_parameters[parameter_name]
            whitelist = self.whitelisted_parameters
            should_annotate = True
            if name_whitelist is not None and parameter_name in name_whitelist:
                should_annotate = False
            if whitelist is not None and extract_annotation(parameter) in whitelist:
                should_annotate = False
            if should_annotate:
                if parameter.kind == inspect.Parameter.VAR_KEYWORD:
                    taint = self.kwarg
                elif parameter.kind == inspect.Parameter.VAR_POSITIONAL:
                    taint = self.vararg
                else:
                    taint = self.arg
            else:
                taint = None
            parameters.append(
                RawCallableModel.Parameter(extract_name(parameter), taint)
            )
        return parameters


class FunctionDefinitionModel(RawCallableModel):
    definition: FunctionDefinition
    qualifier: Optional[str] = None

    def __init__(
        self,
        definition: FunctionDefinition,
        arg: Optional[str] = None,
        vararg: Optional[str] = None,
        kwarg: Optional[str] = None,
        returns: Optional[str] = None,
        qualifier: Optional[str] = None,
    ) -> None:
        self.definition = definition
        self.qualifier = qualifier
        self.arg = arg
        self.vararg = vararg
        self.kwarg = kwarg
        self.returns = returns
        super().__init__(
            callable_name=self._get_fully_qualified_callable_name(),
            parameters=self._generate_parameters(),
            returns=returns,
        )

    def _generate_parameters(self) -> List[RawCallableModel.Parameter]:
        parameters: List[RawCallableModel.Parameter] = []
        function_arguments = self.definition.args

        for ast_arg in function_arguments.args:
            parameters.append(RawCallableModel.Parameter(ast_arg.arg, self.arg))

        vararg_parameters = function_arguments.vararg
        if isinstance(vararg_parameters, ast.arg):
            parameters.append(
                RawCallableModel.Parameter(f"*{vararg_parameters.arg}", self.vararg)
            )

        kwarg_parameters = function_arguments.kwarg
        if isinstance(kwarg_parameters, ast.arg):
            parameters.append(
                RawCallableModel.Parameter(f"**{kwarg_parameters.arg}", self.kwarg)
            )

        return parameters

    def _get_fully_qualified_callable_name(self) -> str:
        qualifier = f"{self.qualifier}." if self.qualifier else ""
        fn_name = self.definition.name
        return qualifier + fn_name


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


class ClassModel(Model):
    class_name: str
    annotation: str

    def __init__(self, class_name: str, annotation: str) -> None:
        self.class_name = class_name
        self.annotation = annotation

    def __str__(self) -> str:
        return f"class {self.class_name}({self.annotation}): ..."
