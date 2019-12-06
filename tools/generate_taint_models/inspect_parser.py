# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import inspect
from typing import Callable, Optional


def extract_qualified_name(view_function: Callable[..., object]) -> Optional[str]:
    if inspect.ismethod(view_function):
        return extract_qualified_name(view_function.__func__)
    else:
        module_name = getattr(view_function, "__module__", None)
        view_name = getattr(
            view_function, "__qualname__", view_function.__class__.__qualname__
        )
    if "<locals>" in view_name:
        return None
    return ".".join(filter(None, [module_name, view_name]))


def extract_annotation(parameter: inspect.Parameter) -> Optional[str]:
    annotation = parameter.annotation
    if isinstance(annotation, str):
        return annotation
    elif isinstance(annotation, type):
        return annotation.__name__
    else:
        return None


def extract_name(parameter: inspect.Parameter) -> str:
    kind = parameter.kind
    if kind == inspect.Parameter.VAR_KEYWORD:
        return f"**{parameter.name}"
    elif kind == inspect.Parameter.VAR_POSITIONAL:
        return f"*{parameter.name}"
    else:
        return f"{parameter.name}"
