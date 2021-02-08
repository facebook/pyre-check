# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest

# @manual=//python/wheel/graphene3:graphene3
import graphene  # Manually specify the dependency for graphene3

from ...get_dynamic_graphene3_models import (
    DynamicGrapheneV3ModelsGenerator,
)


class TestObject(graphene.ObjectType):
    text = graphene.String()


class TestQuery:
    @classmethod
    def test_object_resolver_method(cls, parent, info, **kwargs):
        return TestObject(
            text="Hello World",
        )


def test_object_resolver_function(parent, info, **kwargs):
    return TestObject(
        text="Hello World",
    )


class Query(graphene.ObjectType):

    test_object = graphene.Field(
        TestObject,
        resolver=TestQuery.test_object_resolver_method,
    )

    test_object2 = graphene.Field(
        TestObject,
        resolver=test_object_resolver_function,
    )


schema = graphene.Schema(query=Query)


class GetDynamicGrapheneModelsV3Test(unittest.TestCase):
    def test_gather_functions_to_model(self) -> None:
        functions = DynamicGrapheneV3ModelsGenerator(
            graphene_schema=schema,
        ).gather_functions_to_model()

        self.assertCountEqual(
            {TestQuery.test_object_resolver_method, test_object_resolver_function},
            set(functions),
        )
