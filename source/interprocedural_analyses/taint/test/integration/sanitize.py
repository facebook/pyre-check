# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
from builtins import _test_sink, _test_source
from typing import Sequence, TypeVar


T = TypeVar("T")


class C_sanitized_a_source:
    attribute = None

    def __init__(self, value):
        self.instance = value
        self.attribute = value


class C_sanitized_b_source:
    attribute = None

    def __init__(self, value):
        self.instance = value
        self.attribute = value


class C_sanitized_ab_sources:
    attribute = None

    def __init__(self, value):
        self.instance = value
        self.attribute = value


class C_sanitized_all_sources:
    attribute = None

    def __init__(self, value):
        self.instance = value
        self.attribute = value


class C_sanitized_a_sink:
    attribute = ...

    def __init__(self, value):
        self.instance = value
        self.attribute = value


class C_sanitized_b_sink:
    attribute = ...

    def __init__(self, value):
        self.instance = value
        self.attribute = value


class C_sanitized_ab_sinks:
    attribute = ...

    def __init__(self, value):
        self.instance = value
        self.attribute = value


class C_sanitized_all_sinks:
    attribute = ...

    def __init__(self, value):
        self.instance = value
        self.attribute = value


def return_taint_sanitize(arg: T) -> T:
    """Identity function that returns the argument unmodified, but is marked as
    'Sanitize' in the accompanying .pysa file
    """
    return arg


def test1():
    tainted = object()
    tainted.id = _test_source()
    test2(tainted)
    test3(tainted)


def test2(tainted_other):
    tainted = return_taint_sanitize(tainted_other)
    _test_sink(tainted.id)


def test3(colliding_name):
    colliding_name = return_taint_sanitize(colliding_name)
    _test_sink(colliding_name.id)


def source_with_tito(x):
    return x


def sanitize_sources(x):
    _test_sink(x)
    return source_with_tito(x)


def sanitize_sinks(x):
    _test_sink(x)
    return source_with_tito(x)


def sanitize_tito(x):
    _test_sink(x)
    return source_with_tito(x)


def a_source():
    ...


def b_source():
    ...


def sanitize_test_a_source():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    return x


def sanitize_test_b_source():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    return x


def sanitize_a_and_b_source():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    return x


def a_sink(x):
    ...


def b_sink(x):
    ...


def sanitize_a_sink(x):
    if 1 > 2:
        a_sink(x)
    else:
        b_sink(x)


def sanitize_b_sink(x):
    if 1 > 2:
        a_sink(x)
    else:
        b_sink(x)


def sanitize_a_and_b_sinks(x):
    if 1 > 2:
        a_sink(x)
    else:
        b_sink(x)


def sanitize_a_tito(x):
    return x


def no_propagation_with_sanitize_a_tito():
    a = a_source()
    b = sanitize_a_tito(a)
    return b


def propagation_of_b_with_sanitize_a_tito():
    b = b_source()
    tito = sanitize_a_tito(b)
    return tito


def sanitize_a_sink_tito(x):
    return x


def no_propagation_of_a_sink(x):
    y = sanitize_a_sink_tito(x)
    a_sink(y)


def propagation_of_b_sink(x):
    y = sanitize_a_sink_tito(x)
    b_sink(y)


def sanitize_a_tito_with_sink(x):
    a_sink(x)
    return x


def test4():
    x = a_source()
    y = sanitize_a_tito_with_sink(x)  # flow here
    a_sink(y)  # no flow here


def sanitize_test_a_source_attribute():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    c = C_sanitized_a_source(x)
    _test_sink(c.attribute)


def sanitize_test_b_source_attribute():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    c = C_sanitized_b_source(x)
    _test_sink(c.attribute)


def sanitize_test_ab_sources_attribute():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    c = C_sanitized_ab_sources(x)
    _test_sink(c.attribute)  # should only trigger Test -> Test


def sanitize_test_all_sources_attribute():
    if 1 > 2:
        x = a_source()
    elif 2 > 3:
        x = b_source()
    else:
        x = _test_source()
    c = C_sanitized_all_sources(x)
    _test_sink(c.attribute)


def sanitize_test_a_source_instance():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    c = C_sanitized_a_source(x)
    _test_sink(c.instance)


def sanitize_test_b_source_instance():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    c = C_sanitized_b_source(x)
    _test_sink(c.instance)


def sanitize_test_ab_sources_instance():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    c = C_sanitized_ab_sources(x)
    _test_sink(c.instance)  # should only trigger Test -> Test


def sanitize_test_all_sources_instance():
    if 1 > 2:
        x = a_source()
    elif 2 > 3:
        x = b_source()
    else:
        x = _test_source()
    c = C_sanitized_all_sources(x)
    _test_sink(c.instance)


def sanitize_a_sink_attribute(c: C_sanitized_a_sink):
    if 1 > 2:
        a_sink(c.attribute)
    else:
        b_sink(c.attribute)


def sanitize_b_sink_attribute(c: C_sanitized_b_sink):
    if 1 > 2:
        a_sink(c.attribute)
    else:
        b_sink(c.attribute)


def sanitize_ab_sinks_attribute(c: C_sanitized_ab_sinks):
    if 1 > 2:
        a_sink(c.attribute)
    else:
        b_sink(c.attribute)


def sanitize_all_sinks_attribute(c: C_sanitized_all_sinks):
    if 1 > 2:
        a_sink(c.attribute)
    elif 2 > 3:
        b_sink(c.attribute)
    else:
        _test_sink(c.attribute)


def sanitize_a_sink_instance(c: C_sanitized_a_sink):
    if 1 > 2:
        a_sink(c.instance)
    else:
        b_sink(c.instance)


def sanitize_b_sink_instance(c: C_sanitized_b_sink):
    if 1 > 2:
        a_sink(c.instance)
    else:
        b_sink(c.instance)


def sanitize_ab_sinks_instance(c: C_sanitized_ab_sinks):
    if 1 > 2:
        a_sink(c.instance)
    else:
        b_sink(c.instance)


def sanitize_all_sinks_instance(c: C_sanitized_all_sinks):
    if 1 > 2:
        a_sink(c.instance)
    elif 2 > 3:
        b_sink(c.instance)
    else:
        _test_sink(c.instance)


def sanitize_test_a_sink_attribute():
    sanitize_a_sink_attribute(_test_source())


def sanitize_test_b_sink_attribute():
    sanitize_b_sink_attribute(_test_source())


def sanitize_test_ab_sinks_attribute():
    sanitize_ab_sinks_attribute(_test_source())


def sanitize_test_all_sinks_attribute():
    sanitize_all_sinks_attribute(_test_source())  # should not trigger
    c = C_sanitized_all_sinks({})
    c.attribute = _test_source()  # should trigger Test -> Test


def sanitize_test_a_sink_instance():
    sanitize_a_sink_instance(_test_source())


def sanitize_test_b_sink_instance():
    sanitize_b_sink_instance(_test_source())


def sanitize_test_ab_sinks_instance():
    sanitize_ab_sinks_instance(_test_source())


def sanitize_test_all_sinks_instance():
    sanitize_all_sinks_instance(_test_source())  # should not trigger
    c = C_sanitized_all_sinks({})
    c.instance = _test_source()  # should trigger Test -> Test


def sanitize_parameter(x, y):
    _test_sink(x)
    _test_sink(y)
    return source_with_tito(x) + source_with_tito(y)


def sanitize_parameter_all_sources(x, y):
    _test_sink(x)
    _test_sink(y)
    return source_with_tito(x) + source_with_tito(y)


def sanitize_parameter_all_sinks(x, y):
    _test_sink(x)
    _test_sink(y)
    return source_with_tito(x) + source_with_tito(y)


def sanitize_parameter_all_tito(x, y):
    _test_sink(x)
    _test_sink(y)
    return source_with_tito(x) + source_with_tito(y)


def sanitize_parameter_a_sink(x):
    if 1 > 2:
        a_sink(x)
    else:
        b_sink(x)


def sanitize_parameter_b_sink(x):
    if 1 > 2:
        a_sink(x)
    else:
        b_sink(x)


def sanitize_parameter_a_source_tito(x):
    return x


def no_propagation_with_sanitize_parameter_a_source_tito():
    a = a_source()
    b = sanitize_parameter_a_source_tito(a)
    return b


def propagation_of_b_with_sanitize_parameter_a_source_tito():
    b = b_source()
    tito = sanitize_parameter_a_source_tito(b)
    return tito


def sanitize_parameter_a_sink_tito(x):
    return x


def no_propagation_with_sanitize_parameter_a_sink_tito(x):
    y = sanitize_parameter_a_sink_tito(x)
    a_sink(y)


def propagation_of_b_with_sanitize_parameter_a_sink_tito(x):
    y = sanitize_a_sink_tito(x)
    b_sink(y)


def sanitize_return(x):
    _test_sink(x)
    return source_with_tito(x)


def sanitize_return_all_sources(x):
    _test_sink(x)
    return source_with_tito(x)


def sanitize_return_a_source():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    return x


def sanitize_return_b_source():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    return x


def sanitize_return_a_and_b_source():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    return x
