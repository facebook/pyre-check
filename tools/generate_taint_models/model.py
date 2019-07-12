import ast
import inspect
import types
from typing import Callable, Iterable, List, NamedTuple, Optional, Union

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
        for parameter_name in view_parameters:
            parameter = view_parameters[parameter_name]
            annotation = ""
            whitelist = self.whitelisted_parameters
            if whitelist is None or extract_annotation(parameter) not in whitelist:
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


class AssignmentModel(NamedTuple):
    annotation: str
    target: str

    def generate(self):
        return f"{self.target}: {self.annotation} = ..."
