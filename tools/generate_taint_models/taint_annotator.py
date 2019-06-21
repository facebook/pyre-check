import ast
from typing import List, NamedTuple, Optional, Union

import _ast


FunctionDefinition = Union[_ast.FunctionDef, _ast.AsyncFunctionDef]


class Model(NamedTuple):
    arg: str = ""
    vararg: str = ""
    kwarg: str = ""
    returns: str = ""


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
