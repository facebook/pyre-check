# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
import inspect
import types
from typing import Callable, Iterable, List, NamedTuple, Optional, Set, Tuple, Union

import _ast

from .inspect_parser import extract_annotation, extract_name, extract_view_name


FunctionDefinition = Union[_ast.FunctionDef, _ast.AsyncFunctionDef]


class RawCallableModel(NamedTuple):
    callable_name: str
    # The tuple's second element is the taint of the parameter.
    parameters: List[Tuple[str, Optional[str]]]
    returns: Optional[str] = None

    def generate(self) -> str:
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


class CallableModel(NamedTuple):
    callable: Callable[..., object]
    arg: Optional[str] = None
    vararg: Optional[str] = None
    kwarg: Optional[str] = None
    returns: Optional[str] = None
    whitelisted_parameters: Optional[Iterable[str]] = None
    parameter_name_whitelist: Optional[Set[str]] = None

    def generate(self) -> Optional[str]:
        modeled_object = self.callable
        view_name = extract_view_name(modeled_object)
        # Don't attempt to generate models for local functions that our static analysis
        # can't handle.
        if view_name is None:
            return None
        parameters = []
        if isinstance(modeled_object, types.FunctionType):
            view_parameters = inspect.signature(modeled_object).parameters
        elif isinstance(modeled_object, types.MethodType):
            # pyre-fixme
            view_parameters = inspect.signature(modeled_object.__func__).parameters
        else:
            return
        name_whitelist = self.parameter_name_whitelist
        for parameter_name in view_parameters:
            parameter = view_parameters[parameter_name]
            whitelist = self.whitelisted_parameters
            should_annotate = True
            if name_whitelist is not None and parameter_name not in name_whitelist:
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
            parameters.append((extract_name(parameter), taint))
        return RawCallableModel(
            callable_name=view_name, parameters=parameters, returns=self.returns
        ).generate()


def _annotate(argument: str, annotation: Optional[str]) -> str:
    if annotation:
        return f"{argument}: {annotation}"
    else:
        return argument


class FunctionDefinitionModel(NamedTuple):
    definition: FunctionDefinition
    arg: Optional[str] = None
    vararg: Optional[str] = None
    kwarg: Optional[str] = None
    returns: Optional[str] = None
    qualifier: Optional[str] = None

    def generate(self) -> str:
        annotated_params: List[str] = []
        parameters = self.definition.args

        for ast_arg in self.definition.args.args:
            annotated_params.append(_annotate(ast_arg.arg, self.arg))

        vararg_parameters = parameters.vararg
        if isinstance(vararg_parameters, ast.arg):
            annotated_params.append(_annotate(f"*{vararg_parameters.arg}", self.vararg))

        kwarg_parameters = parameters.kwarg
        if isinstance(kwarg_parameters, ast.arg):
            annotated_params.append(_annotate(f"**{kwarg_parameters.arg}", self.kwarg))

        combined_params = ", ".join(annotated_params) if annotated_params else ""

        returns = f" -> {self.returns}" if self.returns else ""
        qualifier = f"{self.qualifier}." if self.qualifier else ""

        fn_name = self.definition.name

        return f"def {qualifier}{fn_name}({combined_params}){returns}: ..."


class AssignmentModel(NamedTuple):
    annotation: str
    target: str

    def generate(self) -> str:
        return f"{self.target}: {self.annotation} = ..."
