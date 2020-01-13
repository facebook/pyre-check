# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import Callable, Iterable, List, Optional, Set

from .inspect_parser import extract_qualified_name
from .model import CallableModel, Model


def taint_functions(
    functions_to_taint: Iterable[Callable[..., object]],
    taint_annotation: str = "TaintSource[UserControlled]",
    whitelisted_views: Optional[List[str]] = None,
    whitelisted_classes: Optional[List[str]] = None,
    parameter_name_whitelist: Optional[Set[str]] = None,
) -> List[Model]:
    whitelisted_views = whitelisted_views or []
    whitelisted_classes = whitelisted_classes or []
    parameter_name_whitelist = parameter_name_whitelist or set()
    entry_points = set()
    for function in functions_to_taint:
        qualified_name = extract_qualified_name(function)
        if qualified_name in whitelisted_views:
            continue
        try:
            model = CallableModel(
                callable_object=function,
                arg=taint_annotation,
                vararg=taint_annotation,
                kwarg=taint_annotation,
                whitelisted_parameters=whitelisted_classes,
                parameter_name_whitelist=parameter_name_whitelist,
            )
            entry_points.add(model)
        except ValueError:
            pass

    return sorted(entry_points)
