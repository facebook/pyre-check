# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import inspect
import types
from typing import Callable, Iterable

from .inspect_parser import extract_annotation, extract_name, extract_view_name
from .model import Model
from .model_generator import Configuration, Registry
from .view_generator import ViewGenerator


class RESTApiSourceGenerator(ViewGenerator):
    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[str]:
        entry_points = set()

        for view_function in functions_to_model:
            view_name = extract_view_name(view_function)
            if view_name in Configuration.whitelisted_views:
                continue
            model = Model(
                arg="TaintSource[UserControlled]",
                vararg="TaintSource[UserControlled]",
                kwarg="TaintSource[UserControlled]",
            )
            callable = model.generate(view_function, Configuration.whitelisted_classes)
            if callable is not None:
                entry_points.add(callable)

        return sorted(entry_points)


Registry.register("get_REST_api_sources", RESTApiSourceGenerator)
