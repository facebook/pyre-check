# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import inspect
from typing import Callable, Optional


def extract_view_name(view_func: Callable[..., object]) -> Optional[str]:
    method_name = None
    self_class = getattr(view_func, "__self__", None)
    if self_class and inspect.ismethod(view_func):
        method_name = view_func.__name__
        view_func = self_class
        module_name = getattr(view_func, "__module__", None)
        view_name = getattr(view_func, "__name__", view_func.__class__.__name__)
    else:
        module_name = getattr(view_func, "__module__", None)
        view_name = getattr(view_func, "__qualname__", view_func.__class__.__qualname__)
    if "<locals>" in view_name:
        return None
    return ".".join(filter(None, [module_name, view_name, method_name]))


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
