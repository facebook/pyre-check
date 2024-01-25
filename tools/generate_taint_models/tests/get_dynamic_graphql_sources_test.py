# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from dataclasses import dataclass

from graphql3 import (
    GraphQLBoolean,
    GraphQLField,
    GraphQLID,
    GraphQLNonNull,
    GraphQLObjectType,
    GraphQLSchema,
)

from ...generate_taint_models.get_dynamic_graphql_sources import (
    DynamicGraphQLSourceGenerator, DynamicGraphQLFormattableSpecification
)
from tools.pyre.tools.generate_taint_models.generator_specifications import (
    AllParametersAnnotation,
    AnnotationSpecification,
)
from tools.pyre.tools.generate_taint_models.model_generator import  ModelGenerationException

def function1(foo) -> bool:
    return True


def function2(foo, *bar) -> bool:
    return True


def excluded_function(foo) -> bool:
    return True


class TestClass:
    def method1(self, foo) -> bool:
        return True

    def method2(self, foo, *bar) -> bool:
        return True


@dataclass
class DirectObject:
    id: int
    resolver1: bool
    resolver2: bool
    resolver3: bool
    resolver4: bool
    lambda_resolver: bool


queryType = GraphQLObjectType(
    name="queryType",
    description="GraphQLObject directly created at top level",
    fields={
        "no_resolver": GraphQLField(GraphQLNonNull(GraphQLID)),
        "resolver1": GraphQLField(GraphQLBoolean, resolve=function1),
        "resolver2": GraphQLField(GraphQLBoolean, resolve=function2),
        "resolver3": GraphQLField(GraphQLBoolean, resolve=TestClass.method1),
        "resolver4": GraphQLField(GraphQLBoolean, resolve=TestClass.method2),
        "lambda_resolver": GraphQLField(GraphQLBoolean, resolve=lambda x: x),
        "res": GraphQLField(GraphQLBoolean, resolve=excluded_function),
    },
)

SCHEMA = GraphQLSchema(query=queryType)


class GetDynamicGraphQLSourcesTest(unittest.TestCase):
    def test_compute_models(self) -> None:
        source = "TaintSource[UserControlled]"
        sink = "TaintSink[ReturnedToUser]"

        self.assertEqual(
            [
                str(callable_model) for callable_model in DynamicGraphQLSourceGenerator(
                        graphql_schema=SCHEMA,
                        graphql_object_type=GraphQLObjectType,
                        resolvers_to_exclude=[
                            "tools.pyre.tools.generate_taint_models.tests.get_dynamic_graphql_sources_test.excluded_function"
                        ],
                        annotations=AnnotationSpecification(
                            parameter_annotation=AllParametersAnnotation(
                                vararg=source,
                                kwarg=source,
                            ),
                            returns=sink,
                        ),
                    ).compute_models([])
            ],
            [
                f"def graphql3.type.introspection.InputValueFieldResolvers.default_value(item, _info) -> {sink}: ...",
                f"def graphql3.type.introspection.InputValueFieldResolvers.description(item, _info) -> {sink}: ...",
                f"def graphql3.type.introspection.InputValueFieldResolvers.name(item, _info) -> {sink}: ...",
                f"def graphql3.type.introspection.InputValueFieldResolvers.type(item, _info) -> {sink}: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.description(type_, _info) -> {sink}: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.enum_values(type_, _info, includeDeprecated) -> {sink}: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.fields(type_, _info, includeDeprecated) -> {sink}: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.input_fields(type_, _info) -> {sink}: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.interfaces(type_, _info) -> {sink}: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.kind(type_, _info) -> {sink}: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.name(type_, _info) -> {sink}: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.of_type(type_, _info) -> {sink}: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.possible_types(type_, info) -> {sink}: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.specified_by_url(type_, _info) -> {sink}: ...",
                f"def tools.pyre.tools.generate_taint_models.tests.get_dynamic_graphql_sources_test.TestClass.method1(self, foo) -> {sink}: ...",
                f"def tools.pyre.tools.generate_taint_models.tests.get_dynamic_graphql_sources_test.TestClass.method2(self, foo, *bar: {source}) -> {sink}: ...",
                f"def tools.pyre.tools.generate_taint_models.tests.get_dynamic_graphql_sources_test.function1(foo) -> {sink}: ...",
                f"def tools.pyre.tools.generate_taint_models.tests.get_dynamic_graphql_sources_test.function2(foo, *bar: {source}) -> {sink}: ...",
            ],
        )

    def test_formattable_compute_models(self)-> None:
        source = "TaintSource[UserControlled]"
        sink = "TaintSink[ReturnedToUser, ViaDynamicFeature[graphql-legacy-field:"

        self.assertEqual(
            [str(callable_model) for callable_model in DynamicGraphQLSourceGenerator(
                        graphql_schema=SCHEMA,
                        graphql_object_type=GraphQLObjectType,
                        resolvers_to_exclude=[
                            "tools.pyre.tools.generate_taint_models.tests.get_dynamic_graphql_sources_test.excluded_function"
                        ],
                        annotations=AnnotationSpecification(
                            parameter_annotation=AllParametersAnnotation(
                                vararg=source,
                                kwarg=source,
                            ),
                        ),
                         formattable_return=DynamicGraphQLFormattableSpecification(
                            "TaintSink[ReturnedToUser, ViaDynamicFeature[graphql-legacy-field:{gql_type_name}.{gql_field}]]"
                        ),
                    ).compute_models([])
            ],
            [
                f"def graphql3.type.introspection.InputValueFieldResolvers.default_value(item, _info) -> {sink}__InputValue.defaultValue]]: ...",
                f"def graphql3.type.introspection.InputValueFieldResolvers.description(item, _info) -> {sink}__InputValue.description]]: ...",
                f"def graphql3.type.introspection.InputValueFieldResolvers.name(item, _info) -> {sink}__InputValue.name]]: ...",
                f"def graphql3.type.introspection.InputValueFieldResolvers.type(item, _info) -> {sink}__InputValue.type]]: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.description(type_, _info) -> {sink}__Type.description]]: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.enum_values(type_, _info, includeDeprecated) -> {sink}__Type.enumValues]]: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.fields(type_, _info, includeDeprecated) -> {sink}__Type.fields]]: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.input_fields(type_, _info) -> {sink}__Type.inputFields]]: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.interfaces(type_, _info) -> {sink}__Type.interfaces]]: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.kind(type_, _info) -> {sink}__Type.kind]]: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.name(type_, _info) -> {sink}__Type.name]]: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.of_type(type_, _info) -> {sink}__Type.ofType]]: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.possible_types(type_, info) -> {sink}__Type.possibleTypes]]: ...",
                f"def graphql3.type.introspection.TypeFieldResolvers.specified_by_url(type_, _info) -> {sink}__Type.specifiedByUrl]]: ...",
                f"def tools.pyre.tools.generate_taint_models.tests.get_dynamic_graphql_sources_test.TestClass.method1(self, foo) -> {sink}queryType.resolver3]]: ...",
                f"def tools.pyre.tools.generate_taint_models.tests.get_dynamic_graphql_sources_test.TestClass.method2(self, foo, *bar: {source}) -> {sink}queryType.resolver4]]: ...",
                f"def tools.pyre.tools.generate_taint_models.tests.get_dynamic_graphql_sources_test.function1(foo) -> {sink}queryType.resolver1]]: ...",
                f"def tools.pyre.tools.generate_taint_models.tests.get_dynamic_graphql_sources_test.function2(foo, *bar: {source}) -> {sink}queryType.resolver2]]: ...",
            ],
        )

    def test_compute_models_double_returns_raises(self)-> None:
        source = "TaintSource[UserControlled]"
        sink = "TaintSink[ReturnedToUser, ViaDynamicFeature[graphql-legacy-field:"

        with self.assertRaises(ModelGenerationException):
            DynamicGraphQLSourceGenerator(
                        graphql_schema=SCHEMA,
                        graphql_object_type=GraphQLObjectType,
                        resolvers_to_exclude=[
                            "tools.pyre.tools.generate_taint_models.tests.get_dynamic_graphql_sources_test.excluded_function"
                        ],
                        annotations=AnnotationSpecification(
                            parameter_annotation=AllParametersAnnotation(
                                vararg=source,
                                kwarg=source,
                            ),
                            returns=sink,
                        ),
                         formattable_return=DynamicGraphQLFormattableSpecification(
                            "TaintSink[ReturnedToUser, ViaDynamicFeature[graphql-legacy-field:{gql_type_name}.{gql_field}]]"
                        ),
                    ).compute_models([])
