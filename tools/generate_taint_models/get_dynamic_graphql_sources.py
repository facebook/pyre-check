# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import Any, Callable, Iterable, List, Optional, Set, Type

try:
    from graphql3 import GraphQLSchema
except ModuleNotFoundError:
    # pyre-fixme[21]: Could not find module `graphql`.
    from graphql import GraphQLSchema

from .generator_specifications import AnnotationSpecification
from .model import CallableModel, UnsupportedCallable
from .model_generator import ModelGenerator, ModelGenerationException


# pyre-ignore: Too dynamic.
GraphQLObjectType = Type[Any]

class DynamicGraphQLFormattableSpecification():
    def __init__(self, template_str: str) -> None:
        if not template_str or '{gql_type_name}' not in template_str or '{gql_field}' not in template_str:
            raise ModelGenerationException("Template string must be provided and contain '{gql_type_name}' and '{gql_field}'")
        self.template_str = template_str

    def format(self, gql_type_name: str, gql_field: str) -> str:
        return self.template_str.format(gql_type_name=gql_type_name, gql_field=gql_field)


class DynamicGraphQLSourceGenerator(ModelGenerator[CallableModel]):
    def __init__(
        self,
        # pyre-fixme[11]: Annotation `GraphQLSchema` is not defined as a type.
        graphql_schema: GraphQLSchema,
        graphql_object_type: GraphQLObjectType,
        annotations: AnnotationSpecification,
        formattable_return: Optional[DynamicGraphQLFormattableSpecification] = None,
        resolvers_to_exclude: Optional[List[str]] = None,
    ) -> None:
        super().__init__()
        self.graphql_schema: GraphQLSchema = graphql_schema
        self.graphql_object_type: GraphQLObjectType = graphql_object_type
        if formattable_return and annotations.returns:
            raise ModelGenerationException("Setting a returns in annotations will be overwritten when specifying a formattable_return")
        self.annotations: AnnotationSpecification = annotations
        self.formattable_return: Optional[DynamicGraphQLFormattableSpecification] = formattable_return
        self.resolvers_to_exclude: List[str] = resolvers_to_exclude or []

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return []

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[CallableModel]:
        type_map = self.graphql_schema.type_map

        # Get all graphql resolver functions.
        entry_points: Set[CallableModel] = set()

        for element in type_map.values():
            if not isinstance(element, self.graphql_object_type):
                continue

            try:
                fields = element.fields
                gql_object_name = element.name
            except AssertionError:
                # GraphQL throws an exception when a GraphQL object is created
                # with 0 fields. Since we don't control the library, we need to
                # program defensively here :(
                continue

            for field in fields:
                resolver = fields[field].resolve
                if (
                    resolver is not None
                    and resolver.__name__ != "<lambda>"
                    and f"{resolver.__module__}.{resolver.__name__}"
                    not in self.resolvers_to_exclude
                ):
                    annotation = self.annotations
                    if self.formattable_return:
                        formatted_return = self.formattable_return.format(gql_object_name, field)
                        annotation = self.annotations._replace(returns=formatted_return)

                    try:
                        model = CallableModel(
                            callable_object=resolver, annotations=annotation
                        )
                        entry_points.add(model)
                    except UnsupportedCallable:
                        pass

        return sorted(entry_points)
