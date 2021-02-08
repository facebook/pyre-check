# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Callable, Iterable, Optional

# @manual=//python/wheel/graphene3:graphene3
from graphene import Schema  # Manually specify the dependency for graphene3

from .function_tainter import taint_callable_functions
from .generator_specifications import AllParametersAnnotation, AnnotationSpecification
from .model import CallableModel
from .model_generator import ModelGenerator


class DynamicGrapheneV3ModelsGenerator(ModelGenerator[CallableModel]):
    def __init__(
        self,
        graphene_schema: Schema,
        annotations: Optional[AnnotationSpecification] = None,
    ) -> None:
        self.graphene_schema: Schema = graphene_schema
        self.annotations: AnnotationSpecification = (
            annotations
            or AnnotationSpecification(
                parameter_annotation=AllParametersAnnotation(
                    arg="TaintSource[UserControlled]",
                    vararg="TaintSource[UserControlled]",
                    kwarg="TaintSource[UserControlled]",
                ),
                returns="TaintSink[ReturnedToUser]",
            )
        )

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        query_resolvers = (
            [
                graphql_type.resolver
                for graphql_type in self.graphene_schema.query._meta.fields.values()  # pyre-ignore Undefined attribute [16]: `Schema` has no attribute `query`
            ]
            if self.graphene_schema.query
            else []
        )
        mutation_resolvers = (
            [
                graphql_type.resolver
                for graphql_type in self.graphene_schema.mutation._meta.fields.values()  # pyre-ignore Undefined attribute [16]: `Schema` has no attribute `mutation`
            ]
            if self.graphene_schema.mutation
            else []
        )

        return mutation_resolvers + query_resolvers

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[CallableModel]:
        return taint_callable_functions(
            functions_to_model, annotations=self.annotations
        )
