# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
import inspect
import types
from typing import Callable, Iterable, List, NamedTuple, Optional, Set, Union

import _ast

from .inspect_parser import extract_annotation, extract_name, extract_view_name


FunctionDefinition = Union[_ast.FunctionDef, _ast.AsyncFunctionDef]


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
            annotation = ""
            whitelist = self.whitelisted_parameters
            should_annotate = True
            if name_whitelist is not None and parameter_name not in name_whitelist:
                should_annotate = False
            if whitelist is not None and extract_annotation(parameter) in whitelist:
                should_annotate = False
            if should_annotate:
                if parameter.kind == inspect.Parameter.VAR_KEYWORD:
                    keywords = self.kwarg
                    if keywords is not None:
                        annotation = f": {keywords}"
                    else:
                        annotation = ""
                elif parameter.kind == inspect.Parameter.VAR_POSITIONAL:
                    variable = self.vararg
                    if variable is not None:
                        annotation = f": {variable}"
                    else:
                        annotation = ""
                else:
                    argument = self.arg
                    if argument is not None:
                        annotation = f": {argument}"
                    else:
                        annotation = ""

            parameters.append(f"{extract_name(parameter)}{annotation}")

        parameters = ", ".join(parameters) if len(parameters) > 0 else ""
        returns = self.returns
        if returns is not None:
            returns = f" -> {returns}"
        else:
            returns = ""
        return f"def {view_name}({parameters}){returns}: ..."


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
