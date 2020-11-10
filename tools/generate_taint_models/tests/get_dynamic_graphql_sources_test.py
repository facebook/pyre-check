# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from dataclasses import dataclass
from typing import Optional

from graphql import (
    GraphQLBoolean,
    GraphQLField,
    GraphQLID,
    GraphQLNonNull,
    GraphQLObjectType,
    GraphQLSchema,
)
from graphql_server.typemap import TypeMap
from graphql_server.types import graphql_field, graphql_object
from tools.pyre.tools.generate_taint_models.get_dynamic_graphql_sources import (
    DynamicGraphQLSourceGenerator,
)

from .test_functions import __name__ as qualifier, all_functions


def function1(foo):
    return True


def function2(foo, *bar):
    return True


class TestClass(object):
    def method1(self, foo):
        return True

    def method2(self, foo, *bar):
        return True


@dataclass
class DirectObject:
    id: int
    resolver1: bool
    resolver2: bool
    resolver3: bool
    resolver4: bool
    lambda_resolver: bool


@graphql_object()
class DirectObjectResult:
    @graphql_field()
    def success(self) -> str:
        return "True"

    @graphql_field()
    def error_message(self) -> Optional[str]:
        return "Foo"


DirectObjectType = GraphQLObjectType(
    name="DirectObjectType",
    description="GraphQLObject directly created at top level",
    fields={
        "no_resolver": GraphQLField(GraphQLNonNull(GraphQLID)),
        "resolver1": GraphQLField(GraphQLBoolean, resolver=function1),
        "resolver2": GraphQLField(GraphQLBoolean, resolver=function2),
        "resolver3": GraphQLField(GraphQLBoolean, resolver=TestClass.method1),
        "resolver4": GraphQLField(GraphQLBoolean, resolver=TestClass.method2),
        "lambda_resolver": GraphQLField(GraphQLBoolean, resolver=lambda x: x),
    },
)


@graphql_object()
class Query:
    @graphql_field(graphql_core_type=DirectObjectType)
    def get_object(self) -> DirectObject:
        return DirectObject(4, True, True, True, True, True)


TYPEMAP = TypeMap([Query])
# pyre-fixme[6]: Expected
#  `Optional[typing.List[graphql.type.definition.GraphQLNamedType]]` for 2nd param but
#  got `_OrderedDictValuesView[typing.Any]`.
SCHEMA = GraphQLSchema(query=TYPEMAP["Query"], types=TYPEMAP.values())


class GetDynamicGraphQLSourcesTest(unittest.TestCase):
    def test_gather_functions_to_model(self) -> None:
        functions = DynamicGraphQLSourceGenerator(
            graphql_schema=SCHEMA, graphql_object_type=GraphQLObjectType
        ).gather_functions_to_model()

        self.assertTrue(
            {function1, function2, TestClass.method1, TestClass.method2}.issubset(
                set(functions)
            )
        )

    def test_compute_models(self) -> None:
        source = "TaintSource[UserControlled]"
        sink = "TaintSink[ReturnedToUser]"
        self.assertEqual(
            [
                *map(
                    str,
                    DynamicGraphQLSourceGenerator(
                        graphql_schema=SCHEMA, graphql_object_type=GraphQLObjectType
                    ).compute_models(all_functions),
                )
            ],
            [
                f"def {qualifier}.TestClass.methodA(self, x) -> {sink}: ...",
                f"def {qualifier}.TestClass.methodB(self, *args: {source}) -> {sink}: ...",
                f"def {qualifier}.testA() -> {sink}: ...",
                f"def {qualifier}.testB(x) -> {sink}: ...",
                f"def {qualifier}.testC(x) -> {sink}: ...",
                f"def {qualifier}.testD(x, *args: {source}) -> {sink}: ...",
                f"def {qualifier}.testE(x, **kwargs: {source}) -> {sink}: ...",
            ],
        )
