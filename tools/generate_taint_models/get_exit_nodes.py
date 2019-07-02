# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import inspect
import types
from typing import Callable, Iterable

from .inspect_parser import extract_name, extract_view_name
from .model_generator import ModelGenerator


class ExitNodeGenerator(ModelGenerator):
    def __init__(self, whitelisted_views: Iterable[str]) -> None:
        self.whitelisted_views = whitelisted_views

    def compute_models(self, visit_all_views: Callable[..., None]) -> Iterable[str]:
        exit_nodes = set()

        def exit_point_visitor(view_function: Callable[..., object]) -> None:
            view_name = extract_view_name(view_function)
            if view_name in self.whitelisted_views:
                return
            if isinstance(view_function, types.FunctionType):
                all_parameters = inspect.signature(view_function).parameters
            elif isinstance(view_function, types.MethodType):
                # pyre-ignore[6]: `types._StaticFunctionType </: Callable[..., Any].
                all_parameters = inspect.signature(view_function.__func__).parameters
            else:
                return
            parameters = ", ".join(
                extract_name(all_parameters[key]) for key in all_parameters
            )
            exit_node = (
                f"def {view_name}({parameters}) -> TaintSink[ReturnedToUser]: ..."
            )
            exit_nodes.add(exit_node)

        visit_all_views(exit_point_visitor)
        return sorted(exit_nodes)
