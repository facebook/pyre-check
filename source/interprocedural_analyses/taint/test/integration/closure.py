# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def side_effect_reduction_closure():
    x = _test_source()
    z = _test_source()

    def inner(z):
        _test_sink(x)
        _test_sink(z)
    inner(z)


def taint_propagation():
    x = _test_source()
    z = _test_source()
    wrapper_for_taint_propagation(x, z)


def wrapper_for_taint_propagation(x, z):
    def inner(z):
        _test_sink(x)
        _test_sink(z)
    inner(z)


def taint_propagation_hof():
    x = _test_source()
    z = _test_source()
    wrapper_for_taint_propagation_hof(x, z)


def wrapper_for_taint_propagation_hof(x, z):
    def inner(z):
        _test_sink(x)
        _test_sink(z)

    higher_order_function(inner, z)


def higher_order_function(f, z):
    f(z)


def parameter_order_swap(x, y, z):
    def inner():
        _test_sink(y)
        _test_sink(x)
        _test_sink(z)
    inner()


def parameter_order_swap_different_variable_names(x, y, z):
    a, b, c = x, y, z

    def inner():
        _test_sink(b)
        _test_sink(a)
        _test_sink(c)
    inner()


class Object:
    pass


def closure():
    obj = Object()

    def source():
        obj.x = _test_source()

    def sink():
        _test_sink(obj.x)

    return source, sink


def closure_flow():
    # TODO(T145247918): False Negative
    source, sink = closure()
    source()
    sink()


def closure_no_flow():
    source, sink = closure()
    sink()
    source()


def nonlocal_closure():
    obj = ""

    def source():
        nonlocal obj
        obj = _test_source()

    def sink():
        _test_sink(obj)

    return source, sink


def nonlocal_flow():
    # TODO(T145247918): False Negative
    source, sink = nonlocal_closure()
    source()
    sink()


def nonlocal_no_flow():
    source, sink = nonlocal_closure()
    sink()
    source()
