# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import Any, Callable


def method_decorator(callable: Callable[[Any], Any]) -> Callable[[Any], Any]:
    return callable


class GraphQLEntrypoint:
    x: str

    def __init__(self, x) -> None:
        self.x = x

    @method_decorator
    def callee(self) -> None:
        _test_sink(self)

    # This should not be called
    def not_callee(self) -> None:
        _test_sink(self)


def entrypoint_decorator(callable: Callable[[Any], Any]) -> Callable[[Any], Any]:
    return callable


@entrypoint_decorator
def return_graphql_entrypoint_1(x: Any) -> GraphQLEntrypoint:
    entrypoint = GraphQLEntrypoint(_test_source())
    return entrypoint


@entrypoint_decorator
def return_graphql_entrypoint_2(x: Any) -> GraphQLEntrypoint:
    # Test co-existence of return callees and expression callees
    return GraphQLEntrypoint(_test_source())


@entrypoint_decorator
def return_graphql_entrypoint_3(entrypoint: GraphQLEntrypoint) -> GraphQLEntrypoint:
    # Test sink
    return entrypoint
