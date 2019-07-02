# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import inspect
import types
from typing import Callable, Iterable

from .inspect_parser import extract_annotation, extract_name, extract_view_name
from .model_generator import ModelGenerator


class RESTApiSourceGenerator(ModelGenerator):
    def __init__(
        self, whitelisted_classes: Iterable[str], whitelisted_views: Iterable[str]
    ) -> None:
        self.whitelisted_classes = whitelisted_classes
        self.whitelisted_views = whitelisted_views

    def compute_models(self, visit_all_views: Callable[..., None]) -> Iterable[str]:
        entry_points = set()

        # pyre-fixme[2]: Parameter annotation cannot contain `Any`.
        def entry_point_visitor(view_function: Callable) -> None:
            view_name = extract_view_name(view_function)
            if view_name in self.whitelisted_views:
                return
            parameters = []
            if isinstance(view_function, types.FunctionType):
                view_parameters = inspect.signature(view_function).parameters
                for parameter_name in view_parameters:
                    parameter = view_parameters[parameter_name]
                    annotation = extract_annotation(parameter)
                    if annotation is None or annotation not in self.whitelisted_classes:
                        parameters.append(
                            f"{extract_name(parameter)}: TaintSource[UserControlled]"
                        )
                    else:
                        parameters.append(extract_name(parameter))
            elif isinstance(view_function, types.MethodType):
                # pyre-fixme
                view_parameters = inspect.signature(view_function.__func__).parameters
                for parameter_name in view_parameters:
                    parameter = view_parameters[parameter_name]

                    if extract_annotation(parameter) not in self.whitelisted_classes:
                        parameters.append(
                            f"{extract_name(parameter)}: TaintSource[UserControlled]"
                        )
                    else:
                        parameters.append(extract_name(parameter))
            else:
                return

            parameters = ", ".join(parameters) if len(parameters) > 0 else ""
            entry_points.add(f"def {view_name}({parameters}): ...")

        visit_all_views(entry_point_visitor)
        return sorted(entry_points)
