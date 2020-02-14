# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import os
from importlib import import_module
from typing import Any, Callable, Iterable, List, Optional, Type, Union

from .model import CallableModel, Model
from .model_generator import ModelGenerator


# pyre-ignore: Too dynamic.
GraphQLObjectType = Type[Any]


class GraphQLSourceGenerator(ModelGenerator):
    def __init__(
        self,
        graphql_module: Union[List[str], str],
        graphql_object_type: GraphQLObjectType,
    ) -> None:
        super().__init__()
        self.graphql_module: Union[List[str], str] = graphql_module
        self.graphql_object_type: GraphQLObjectType = graphql_object_type

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        # Get all graphql import names.
        views: List[Callable[..., object]] = []
        modules = []

        module_argument = self.graphql_module
        graphql_modules = (
            [module_argument] if isinstance(module_argument, str) else module_argument
        )

        for graphql_module in graphql_modules:
            for path in os.listdir(
                os.path.dirname(import_module(graphql_module).__file__)
            ):
                if path.endswith(".py") and path != "__init__.py":
                    modules.append(f"{graphql_module}.{path[:-3]}")

            def visit_all_graphql_resolvers(module_name: str) -> None:
                module = import_module(module_name)
                for key in module.__dict__:
                    element = module.__dict__[key]

                    if not isinstance(element, self.graphql_object_type):
                        continue

                    try:
                        fields = element.fields
                    except AssertionError:
                        # GraphQL throws an exception when a GraphQL object is created
                        # with 0 fields. Since we don't control the library, we need to
                        # program defensively here :(
                        fields = []
                    for field in fields:
                        resolver = fields[field].resolver
                        if resolver is not None and resolver.__name__ != "<lambda>":
                            views.append(resolver)

            for module_name in modules:
                visit_all_graphql_resolvers(module_name)

        return views

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[Model]:
        graphql_models = set()
        for view_function in functions_to_model:
            try:
                model = CallableModel(
                    callable_object=view_function,
                    vararg="TaintSource[UserControlled]",
                    kwarg="TaintSource[UserControlled]",
                    returns="TaintSink[ReturnedToUser]",
                )
                graphql_models.add(model)
            except ValueError:
                pass

        return sorted(graphql_models)
