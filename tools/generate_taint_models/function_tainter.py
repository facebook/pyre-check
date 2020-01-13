# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import Callable, Iterable, List, Optional, Set

from .inspect_parser import extract_qualified_name
from .model import CallableModel, Model


class FunctionTainter:
    parameter_name_whitelist: Optional[Set[str]] = None

    def __init__(
        self,
        whitelisted_classes: Optional[List[str]] = None,
        whitelisted_views: Optional[List[str]] = None,
        arg: str = "TaintSource[UserControlled]",
        vararg: str = "TaintSource[UserControlled]",
        kwarg: str = "TaintSource[UserControlled]",
    ) -> None:
        self.arg = arg
        self.vararg = vararg
        self.kwarg = kwarg
        self.whitelisted_classes: List[str] = whitelisted_classes or []
        self.whitelisted_views: List[str] = whitelisted_views or []

    def taint_functions(
        self, functions_to_taint: Iterable[Callable[..., object]]
    ) -> List[Model]:
        entry_points = set()
        for function in functions_to_taint:
            qualified_name = extract_qualified_name(function)
            if qualified_name in self.whitelisted_views:
                continue
            try:
                model = CallableModel(
                    callable_object=function,
                    arg=self.arg,
                    vararg=self.vararg,
                    kwarg=self.kwarg,
                    whitelisted_parameters=self.whitelisted_classes,
                    parameter_name_whitelist=self.parameter_name_whitelist,
                )
                entry_points.add(model)
            except ValueError:
                pass

        return sorted(entry_points)
