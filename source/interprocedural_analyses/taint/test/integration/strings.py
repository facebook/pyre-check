# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import __test_sink, __test_source


def concatenate_lhs(source: str):
    return source + "A"


def concatenate_rhs(source: str):
    return "A" + source


def bad_1():
    a = concatenate_lhs(__test_source())
    __test_sink(a)


def bad_2():
    a = concatenate_rhs(__test_source())
    __test_sink(a)


def either(b: bool):
    if b:
        a = concatenate_lhs(__test_source())
    else:
        a = concatenate_rhs(__test_source())
    __test_sink(a)


def maybe_lhs(b: bool):
    if b:
        a = concatenate_lhs(__test_source())
    else:
        a = __test_source()
    __test_sink(a)


def maybe_rhs(b: bool):
    if b:
        a = __test_source()
    else:
        a = concatenate_rhs(__test_source())
    __test_sink(a)


def through_iadd():
    a = __test_source()
    b = ""
    b += a
    __test_sink(b)


def format_tito(x):
    return "a {}".format(x)


def format_source():
    x = __test_source()
    return "a {}".format(x)


def format_sink(x):
    y = "a {}".format(x)
    __test_sink(y)
