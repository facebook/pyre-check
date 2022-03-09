# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
from builtins import _test_sink, _test_source
from typing import TypeVar


T = TypeVar("T")


def sanitize_all(x: T) -> T:
    ...


def test1(x):
    y = sanitize_all(x)
    _test_sink(y)


def test2():
    x = _test_source()
    y = sanitize_all(x)
    _test_sink(y)


def sanitize_tito(x: T) -> T:
    ...


def test3(x):
    y = sanitize_tito(x)
    _test_sink(y)


def test4():
    x = _test_source()
    y = sanitize_tito(x)
    _test_sink(y)


def a_source():
    return


def b_source():
    return


def a_sink(x):
    return


def b_sink(x):
    return


def sanitize_a_tito(x):
    ...


def no_propagation_with_sanitize_a_tito():
    a = a_source()
    b = sanitize_a_tito(a)
    return b


def propagation_of_b_with_sanitize_a_tito():
    b = b_source()
    tito = sanitize_a_tito(b)
    return tito


def sanitize_a_sink_tito(x):
    ...


def no_propagation_of_a_sink(x):
    y = sanitize_a_sink_tito(x)
    a_sink(y)


def propagation_of_b_sink(x):
    y = sanitize_a_sink_tito(x)
    b_sink(y)


def sanitize_a_source_tito(x):
    ...


def no_propagation_of_a_source():
    x = a_source()
    return sanitize_a_source_tito(x)


def propagation_of_b_source():
    x = b_source()
    return sanitize_a_source_tito(x)


def sanitize_parameter_source_a_tito(x, y):
    ...


def no_propagation_of_a_source_via_parameter_tito():
    x = a_source()
    return sanitize_parameter_source_a_tito(x)


def propagation_of_a_source_via_other_parameter_tito():
    x = a_source()
    return sanitize_parameter_source_a_tito(y=x, x="foo")


def propagation_of_b_source_via_parameter_tito():
    x = b_source()
    return sanitize_parameter_source_a_tito(x)


def sanitize_parameter_sink_a_tito(x, y):
    ...


def no_propagation_of_a_sink_via_parameter_tito(x):
    y = sanitize_parameter_sink_a_tito(x)
    a_sink(y)


def propagation_of_a_sink_via_other_parameter_tito(x):
    y = sanitize_parameter_sink_a_tito("foo", x)
    a_sink(y)


def propagation_of_b_sink_via_parameter_tito(x):
    y = sanitize_parameter_sink_a_tito(x)
    b_sink(y)


def sanitize_return_source_a_tito(x):
    ...


def no_propagation_of_a_source_via_return_tito():
    x = a_source()
    return sanitize_return_source_a_tito(x)


def propagation_of_b_source_via_return_tito():
    x = b_source()
    return sanitize_return_source_a_tito(x)


def sanitize_return_sink_a_tito(x):
    ...


def no_propagation_of_a_sink_via_return_tito(x):
    y = sanitize_return_sink_a_tito(x)
    a_sink(y)


def propagation_of_b_sink_via_return_tito(x):
    y = sanitize_return_sink_a_tito(x)
    b_sink(y)


def sanitize_parameter_source_a(x):
    ...


def no_propagation_of_a_source_via_parameter():
    x = a_source()
    return sanitize_parameter_source_a(x)


def propagation_of_b_source_via_parameter():
    x = b_source()
    return sanitize_parameter_source_a(x)


def sanitize_parameter_sink_a(x):
    ...


def no_propagation_of_a_sink_via_parameter(x):
    y = sanitize_parameter_sink_a(x)
    a_sink(y)


def propagation_of_b_sink_via_parameter(x):
    y = sanitize_parameter_sink_a(x)
    b_sink(y)


def sanitize_return_source_a(x):
    ...


def no_propagation_of_a_source_via_return():
    x = a_source()
    return sanitize_return_source_a(x)


def propagation_of_b_source_via_return():
    x = b_source()
    return sanitize_return_source_a(x)


def sanitize_return_sink_a(x):
    ...


def no_propagation_of_a_sink_via_return(x):
    y = sanitize_return_sink_a(x)
    a_sink(y)


def propagation_of_b_sink_via_return(x):
    y = sanitize_return_sink_a(x)
    b_sink(y)
