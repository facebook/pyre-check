# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import inspect
import types
from typing import Callable, Iterable

from .inspect_parser import extract_name, extract_view_name
from .model import CallableModel
from .model_generator import Configuration, Registry
from .view_generator import ViewGenerator


class ExitNodeGenerator(ViewGenerator):
    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[str]:
        exit_nodes = set()

        for view_function in functions_to_model:
            view_name = extract_view_name(view_function)
            if view_name in Configuration.whitelisted_views:
                continue
            model = CallableModel(
                returns="TaintSink[ReturnedToUser]", callable=view_function
            ).generate()
            if model is not None:
                exit_nodes.add(model)

        return sorted(exit_nodes)


Registry.register("get_exit_nodes", ExitNodeGenerator)
