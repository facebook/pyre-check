import ast
import inspect
import types
from typing import Callable, Iterable, List, NamedTuple, Optional, Union

import _ast

from .inspect_parser import extract_annotation, extract_name, extract_view_name


FunctionDefinition = Union[_ast.FunctionDef, _ast.AsyncFunctionDef]


class Model(NamedTuple):
    arg: str = ""
    vararg: str = ""
    kwarg: str = ""
    returns: str = ""

    def generate(
        self,
        view_function: Callable[..., object],
        whitelisted_parameters: Optional[Iterable[str]] = None,
    ) -> Optional[str]:
        view_name = extract_view_name(view_function)
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
            if (
                whitelisted_parameters is None
                or extract_annotation(parameter) not in whitelisted_parameters
            ):
                if parameter.kind == inspect.Parameter.VAR_KEYWORD:
                    parameters.append(f"{extract_name(parameter)}{self.kwarg}")
                elif parameter.kind == inspect.Parameter.VAR_POSITIONAL:
                    parameters.append(f"{extract_name(parameter)}{self.vararg}")
                else:
                    parameters.append(f"{extract_name(parameter)}{self.arg}")
            else:
                parameters.append(extract_name(parameter))

        parameters = ", ".join(parameters) if len(parameters) > 0 else ""
        return f"def {view_name}({parameters}){self.returns}: ..."


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
        if whitelisted:
            return argument.arg
        else:
            return argument.arg + model.arg

    call_arguments = definition.args

    arguments = call_arguments.args
    modified_arguments = ", ".join(
        [annotated_argument(argument) for argument in arguments]
    )

    variable = call_arguments.vararg
    if variable:
        modified_arguments += f", *{variable.arg}{model.vararg}"

    keywords = call_arguments.kwarg
    if keywords:
        modified_arguments += f", **{keywords.arg}{model.kwarg}"

    return f"def {function}({modified_arguments}){model.returns}: ..."
