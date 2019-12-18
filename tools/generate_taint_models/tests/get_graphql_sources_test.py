# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import os  # noqa
import unittest
from typing import Callable
from unittest.mock import patch

from graphql.type import (
    GraphQLBoolean,
    GraphQLField,
    GraphQLID,
    GraphQLNonNull,
    GraphQLObjectType,
)
from graphql.type.definition import GraphQLType
from tools.pyre.tools.generate_taint_models import get_graphql_sources
from tools.pyre.tools.generate_taint_models.get_graphql_sources import (
    GraphQLSourceGenerator,
)

from .test_functions import __name__ as qualifier, all_functions


class GetGraphQLSourcesTest(unittest.TestCase):
    @patch.object(get_graphql_sources, "Configuration")
    def test_gather_functions_to_model(self, configuration) -> None:
        configuration.graphql_module = "tools.pyre.tools.generate_taint_models.tests"
        configuration.graphql_object_type = GraphQLObjectType

        functions = GraphQLSourceGenerator().gather_functions_to_model()
        self.assertSetEqual(set(functions), {function_1, function_2})

        # Run the same test again, passing in a list for 'graphql_module', to
        # ensure both work
        configuration.graphql_module = ["tools.pyre.tools.generate_taint_models.tests"]
        configuration.graphql_object_type = GraphQLObjectType

        functions = GraphQLSourceGenerator().gather_functions_to_model()
        self.assertSetEqual(set(functions), {function_1, function_2})

    def test_compute_models(self) -> None:
        source = "TaintSource[UserControlled]"
        sink = "TaintSink[ReturnedToUser]"
        self.assertEqual(
            [*map(str, GraphQLSourceGenerator().compute_models(all_functions))],
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


# Defined for testing purposes (see 'test_gather_functions_to_model')
# These functions are not used otherwise.
def function_1() -> None:
    pass


def function_2() -> None:
    pass


# Create an object directly at the top level of the file so that
# 'test_gather_functions_to_model' can verify that we correctly identify the
# resolver
DirectObjectType = GraphQLObjectType(
    name="DirectObjectType",
    description="GraphQLObject directly created at top level",
    fields={
        "no_resolver": GraphQLField(GraphQLNonNull(GraphQLID)),
        "resolver": GraphQLField(GraphQLBoolean, resolver=function_1),
        "lambda_resolver": GraphQLField(GraphQLBoolean, resolver=lambda x: x),
    },
)


def add_field(type: GraphQLType, name: str, resolver: Callable) -> None:
    # pyre-ignore[16]: Undefined attribute
    type._fields[name] = GraphQLField(GraphQLNonNull(GraphQLID), resolver=resolver)


# Indirectly add in an additional resolver, so that
# 'test_gather_functions_to_model' can verify that that resolver is detected
IndirectObjectType = add_field(
    type=DirectObjectType, name="indirect", resolver=function_2
)
