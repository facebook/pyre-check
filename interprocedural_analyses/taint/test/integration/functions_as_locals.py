# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
from builtins import __test_sink, __test_source


def foo(arg):
    __test_sink(arg)


def foo_as_local():
    x = __test_source()
    f = foo
    foo(x)
    f(x)


def local_tito(arg):
    f = foo
    f(arg)


class C:
    def m(self, arg):
        __test_sink(arg)


def local_function_with_method_sink(c: C):
    f = c.m
    x = __test_source()
    c.m(x)
    f(x)


def method_tito(c: C, arg):
    f = c.m
    f(arg)


def barA(arg1: str, arg2: str):
    __test_sink(arg1)


def barB(arg1: str, arg2: int):
    __test_sink(arg2)


def a_or_b():
    if 1 > 2:
        f = barA
    else:
        f = barB

    f(__test_source(), 0)
    f(0, __test_source())
