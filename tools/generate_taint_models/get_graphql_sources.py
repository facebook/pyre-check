# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import inspect
import os
import types
from importlib import import_module
from typing import Callable, Iterable

from .inspect_parser import extract_name, extract_view_name
from .model import CallableModel
from .model_generator import Configuration, ModelGenerator, Registry


class GraphQLSourceGenerator(ModelGenerator):
    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        # Get all graphql import names.
        views = []
        modules = []
        for path in os.listdir(
            os.path.dirname(import_module(Configuration.graphql_module).__file__)
        ):
            if path.endswith(".py") and path != "__init__.py":
                modules.append(f"{Configuration.graphql_module}.{path[:-3]}")

        def visit_all_graphql_resolvers(module_name: str) -> None:
            module = import_module(module_name)
            for key in module.__dict__:
                element = module.__dict__[key]
                if isinstance(element, Configuration.graphql_object_type):
                    for field in element.fields:
                        resolver = element.fields[field].resolver
                        if resolver is not None and resolver.__name__ != "<lambda>":
                            views.append(resolver)

        for module_name in modules:
            visit_all_graphql_resolvers(module_name)
        return views

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[str]:
        graphql_models = set()

        for view_function in functions_to_model:
            model = CallableModel(
                callable=view_function,
                vararg="TaintSource[UserControlled]",
                kwarg="TaintSource[UserControlled]",
                returns="TaintSink[ReturnedToUser]",
            ).generate()
            if model is not None:
                graphql_models.add(model)

        return sorted(graphql_models)


Registry.register(
    "get_graphql_sources", GraphQLSourceGenerator, include_by_default=True
)
