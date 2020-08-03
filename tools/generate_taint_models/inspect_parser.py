# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import inspect
import types
from typing import Callable, List, Mapping, Optional

from .parameter import Parameter


def extract_qualified_name(callable_object: Callable[..., object]) -> Optional[str]:
    if inspect.ismethod(callable_object):
        # pyre-fixme[16]: Anonymous callable has no attribute `__func__`.
        return extract_qualified_name(callable_object.__func__)
    else:
        module_name = getattr(callable_object, "__module__", None)
        view_name = getattr(
            callable_object, "__qualname__", callable_object.__class__.__qualname__
        )
    if "<locals>" in view_name:
        return None
    return ".".join(filter(None, [module_name, view_name]))


def extract_parameters(callable_object: Callable[..., object]) -> List[Parameter]:
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
                _extract_parameter_annotation(parameter),
                kind,
            )
        )

    return parameters


def _extract_parameter_annotation(parameter: inspect.Parameter) -> Optional[str]:
    annotation = parameter.annotation
    if isinstance(annotation, str):
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
