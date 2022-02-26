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
    DynamicGraphQLSourceGenerator,
)
from .test_functions import __name__ as qualifier, all_functions


def function1(foo) -> bool:
    return True


def function2(foo, *bar) -> bool:
    return True


class TestClass(object):
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
    },
)

SCHEMA = GraphQLSchema(query=queryType)


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
