import ast
import inspect
import types
from typing import Callable, Iterable, List, NamedTuple, Optional, Union

import _ast

from .inspect_parser import extract_annotation, extract_name, extract_view_name


FunctionDefinition = Union[_ast.FunctionDef, _ast.AsyncFunctionDef]


class Model(NamedTuple):
    arg: Optional[str] = None
    vararg: Optional[str] = None
    kwarg: Optional[str] = None
    returns: Optional[str] = None

    def generate(
        self,
        view_function: Callable[..., object],
        whitelisted_parameters: Optional[Iterable[str]] = None,
    ) -> Optional[str]:
        view_name = extract_view_name(view_function)
        # Don't attempt to generate models for local functions that our static analysis
        # can't handle.
        if view_name is None:
            return None
        parameters = []
        if isinstance(view_function, types.FunctionType):
            view_parameters = inspect.signature(view_function).parameters
        elif isinstance(view_function, types.MethodType):
            # pyre-fixme
            view_parameters = inspect.signature(view_function.__func__).parameters
        else:
            return
        for parameter_name in view_parameters:
            parameter = view_parameters[parameter_name]
            annotation = ""
            if (
                whitelisted_parameters is None
                or extract_annotation(parameter) not in whitelisted_parameters
            ):
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


def annotate_function(
    function: str,
    definition: FunctionDefinition,
    model: Model,
    whitelist: Optional[List[str]] = None,
) -> str:
    def annotated_argument(argument: _ast.arg) -> str:
        annotation = argument.annotation

        whitelisted = (
            annotation
            and whitelist
            and any(element in ast.dump(annotation) for element in whitelist)
        )
        arg = model.arg
        if whitelisted or arg is None:
            return argument.arg
        else:
            return f"{argument.arg}: {arg}"

    call_arguments = definition.args

    arguments = call_arguments.args
    modified_arguments = ", ".join(
        [annotated_argument(argument) for argument in arguments]
    )

    variable = call_arguments.vararg
    if variable:
        annotation = model.vararg
        if annotation is not None:
            modified_arguments += f", *{variable.arg}: {annotation}"
        else:
            modified_arguments += f", *{variable.arg}"

    keywords = call_arguments.kwarg
    if keywords:
        annotation = model.kwarg
        if annotation is not None:
            modified_arguments += f", **{keywords.arg}: {annotation}"
        else:
            modified_arguments += f", **{keywords.arg}"

    returns = model.returns
    if returns is not None:
        returns = f" -> {returns}"
    else:
        returns = ""
    return f"def {function}({modified_arguments}){returns}: ..."
