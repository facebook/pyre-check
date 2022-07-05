# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import inspect
import re
import types
from typing import Callable, List, Mapping, Optional

from .parameter import Parameter


def extract_qualified_name(callable_object: Callable[..., object]) -> Optional[str]:
    if inspect.ismethod(callable_object):
        # pyre-fixme[6]: Expected `(...) -> object` for 1st param but got
        #  `_StaticFunctionType`.
        return extract_qualified_name(callable_object.__func__)
    else:
        module_name = getattr(callable_object, "__module__", None)
        # Try and fallback to objclass
        if module_name is None and (
            objclass := getattr(callable_object, "__objclass__", None)
        ):
            module_name = getattr(objclass, "__module__", None)
        view_name = getattr(
            callable_object, "__qualname__", callable_object.__class__.__qualname__
        )
    if "<locals>" in view_name:
        return None
    return ".".join(filter(None, [module_name, view_name]))


def extract_parameters(
    callable_object: Callable[..., object], strip_annotated: bool = True
) -> List[Parameter]:
    callable_parameters: Mapping[str, inspect.Parameter] = {}
    if isinstance(callable_object, types.FunctionType):
        callable_parameters = inspect.signature(callable_object).parameters
    elif isinstance(callable_object, types.MethodType):
        # pyre-ignore: Too dynamic
        callable_parameters = inspect.signature(callable_object.__func__).parameters

    parameters: List[Parameter] = []
    for parameter in callable_parameters.values():
        if parameter.kind == inspect.Parameter.VAR_KEYWORD:
            kind = Parameter.Kind.KWARG
        elif parameter.kind == inspect.Parameter.VAR_POSITIONAL:
            kind = Parameter.Kind.VARARG
        else:
            kind = Parameter.Kind.ARG

        parameters.append(
            Parameter(
                _extract_parameter_name(parameter),
                _extract_parameter_annotation(parameter, strip_annotated),
                kind,
            )
        )

    return parameters


def _strip_annotated(annotation: str) -> str:
    if matched_annotation := re.search("^Annotated\\[([^,]*),", annotation):
        return matched_annotation.group(1)
    else:
        return annotation


def _extract_parameter_annotation(
    parameter: inspect.Parameter, strip_annotated: bool
) -> Optional[str]:
    annotation = parameter.annotation
    if isinstance(annotation, str):
        if strip_annotated:
            return _strip_annotated(annotation)
        else:
            return annotation
    elif isinstance(annotation, type):
        return annotation.__name__
    else:
        return None


def _extract_parameter_name(parameter: inspect.Parameter) -> str:
    kind = parameter.kind
    if kind == inspect.Parameter.VAR_KEYWORD:
        return f"**{parameter.name}"
    elif kind == inspect.Parameter.VAR_POSITIONAL:
        return f"*{parameter.name}"
    else:
        return f"{parameter.name}"
