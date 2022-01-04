# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def goes_to_sink(arg):
    _test_sink(arg)


def has_tito(arg):
    return arg


def higher_order_function(f, arg):
    f(arg)


def test_higher_order_function():
    higher_order_function(goes_to_sink, _test_source())


class C:
    def method_to_sink(self, arg):
        _test_sink(arg)

    def self_to_sink(self):
        _test_sink(self)


def higher_order_method(c: C, arg):
    higher_order_function(c.method_to_sink, arg)


def test_higher_order_method():
    higher_order_method(C(), _test_source())


def test_higher_order_method_self():
    c: C = _test_source()
    higher_order_function(c.self_to_sink)


def higher_order_function_and_sink(f, arg):
    f(arg)
    _test_sink(arg)


def test_higher_order_function_and_sink():
    higher_order_function_and_sink(goes_to_sink, _test_source())


def test_higher_order_tito(x):
    # no tito because higher_order_function does not return.
    return higher_order_function(has_tito, x)


def apply(f, x):
    return f(x)


def test_apply_tito(x):
    return apply(has_tito, x)


def source_through_tito():
    x = _test_source()
    y = apply(has_tito, x)
    return y


class Callable:
    def __init__(self, value):
        self.value = value

    def __call__(self):
        return


def callable_class():
    c = Callable(_test_source())
    # Even if c is a callable, we should still propagate the taint on it.
    _test_sink(c)


def sink_args(*args):
    for arg in args:
        _test_sink(arg)


def test_location(x: int, y: Callable, z: int):
    sink_args(x, y, z)
