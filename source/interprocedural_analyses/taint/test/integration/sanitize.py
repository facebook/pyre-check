# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
from builtins import __test_sink, __test_source
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


def return_taint_sanitize(arg: T) -> T:
    """Identity function that returns the argument unmodified, but is marked as
    'Sanitize' in the accompanying .pysa file
    """
    return arg


def test1():
    tainted = object()
    tainted.id = __test_source()
    test2(tainted)
    test3(tainted)


def test2(tainted_other):
    tainted = return_taint_sanitize(tainted_other)
    __test_sink(tainted.id)  # This shouldn't be a flow, but it is


def test3(colliding_name):
    colliding_name = return_taint_sanitize(colliding_name)
    __test_sink(colliding_name.id)


def source_with_tito(x):
    return x


def sanitize_sources(x):
    __test_sink(x)
    return source_with_tito(x)


def sanitize_sinks(x):
    __test_sink(x)
    return source_with_tito(x)


def sanitize_tito(x):
    __test_sink(x)
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
    __test_sink(c.attribute)


def sanitize_test_b_source_attribute():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    c = C_sanitized_b_source(x)
    __test_sink(c.attribute)


def sanitize_test_ab_sources_attribute():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    c = C_sanitized_ab_sources(x)
    __test_sink(c.attribute)  # should only trigger Test -> Test


def sanitize_test_all_sources_attribute():
    if 1 > 2:
        x = a_source()
    elif 2 > 3:
        x = b_source()
    else:
        x = __test_source()
    c = C_sanitized_all_sources(x)
    __test_sink(c.attribute)


def sanitize_test_a_source_instance():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    c = C_sanitized_a_source(x)
    __test_sink(c.instance)


def sanitize_test_b_source_instance():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    c = C_sanitized_b_source(x)
    __test_sink(c.instance)


def sanitize_test_ab_sources_instance():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    c = C_sanitized_ab_sources(x)
    __test_sink(c.instance)  # should only trigger Test -> Test


def sanitize_test_all_sources_instance():
    if 1 > 2:
        x = a_source()
    elif 2 > 3:
        x = b_source()
    else:
        x = __test_source()
    c = C_sanitized_all_sources(x)
    __test_sink(c.instance)
