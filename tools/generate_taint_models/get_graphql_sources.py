# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import os
from importlib import import_module
from typing import Any, Callable, Iterable, List, Type, Union

from .generator_specifications import AllParametersAnnotation
from .model import CallableModel
from .model_generator import ModelGenerator


# pyre-ignore: Too dynamic.
GraphQLObjectType = Type[Any]


class GraphQLSourceGenerator(ModelGenerator[CallableModel]):
    def __init__(
        self,
        graphql_module: Union[List[str], str],
        graphql_object_type: GraphQLObjectType,
        args_taint_annotation: str = "TaintSource[UserControlled]",
        return_taint_annotation: str = "TaintSink[ReturnedToUser]",
    ) -> None:
        super().__init__()
        self.graphql_module: Union[List[str], str] = graphql_module
        self.graphql_object_type: GraphQLObjectType = graphql_object_type
        self.args_taint_annotation: str = args_taint_annotation
        self.return_taint_annotation: str = return_taint_annotation

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
                # pyre-fixme[6]: For 1st param expected `PathLike[Variable[AnyStr <:
                #  [str, bytes]]]` but got `Optional[str]`.
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
                        resolver = fields[field].resolve
                        if resolver is not None and resolver.__name__ != "<lambda>":
                            views.append(resolver)

            for module_name in modules:
                visit_all_graphql_resolvers(module_name)

        return views

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[CallableModel]:
        graphql_models = set()
        for view_function in functions_to_model:
            try:
                model = CallableModel(
                    callable_object=view_function,
                    parameter_annotation=AllParametersAnnotation(
                        vararg=self.args_taint_annotation,
                        kwarg=self.args_taint_annotation,
                    ),
                    returns=self.return_taint_annotation,
                )
                graphql_models.add(model)
            except ValueError:
                pass

        return sorted(graphql_models)
