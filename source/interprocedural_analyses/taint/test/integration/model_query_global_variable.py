# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import typing
from builtins import _test_sink, _test_source
from os import environ

foo = []


def f():
    _test_sink(environ)


# TODO (T132423781): classes are not correctly selected as sources
class Baz:
    ...


hello = "hello"
world: str = "world"


def g():
    foo.append(1)


def h():
    _test_sink(Baz)


_test_sink(_test_source())


def returns_any() -> typing.Any:
    return {"hello": Baz()}


typed_global_dict: typing.Dict[str, Baz] = returns_any()

untyped_global_dict = returns_any()

typed_global_lambda: typing.Callable[[int, str], int] = lambda x, y: x + int(y)


def fun(x: int, y: str) -> int:
    return x + int(y)


typed_global_callable: typing.Callable[[int, str], int] = fun

untyped_global_callable = fun(1, "2")
typed_callable_assignment: int = fun(1, "2")

untyped_callable_assignment = fun
