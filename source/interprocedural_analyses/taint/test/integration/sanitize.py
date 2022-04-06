# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
from builtins import _cookies, _rce, _sql, _test_sink, _test_source, _user_controlled
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


def sanitize_a_source_tito(x):
    return x


def no_propagation_with_sanitize_a_source_tito():
    a = a_source()
    b = sanitize_a_source_tito(a)
    return b


def propagation_of_b_with_sanitize_a_source_tito():
    b = b_source()
    tito = sanitize_a_source_tito(b)
    return tito


def propagation_of_sanitize_a_source_tito(x):
    return sanitize_a_source_tito(x)


def no_issue_through_propagation_of_sanitize_a_source_tito():
    x = a_source()
    y = propagation_of_sanitize_a_source_tito(x)
    a_sink(y)


def propagation_of_sanitize_a_source_in_sink_trace(x):
    y = propagation_of_sanitize_a_source_tito(x)
    a_sink(y)


def no_issue_propagation_of_sanitize_a_source_in_sink_trace():
    x = a_source()
    propagation_of_sanitize_a_source_in_sink_trace(x)


def sanitize_b_source_tito(x):
    return x


def sanitize_test_source_tito(x):
    return x


def combine_sanitize_a_source_b_source_in_sink_trace(x):
    y = sanitize_b_source_tito(x)
    propagation_of_sanitize_a_source_in_sink_trace(y)


def sanitize_a_sink_tito(x):
    return x


def no_propagation_of_a_sink(x):
    y = sanitize_a_sink_tito(x)
    a_sink(y)


def propagation_of_b_sink(x):
    y = sanitize_a_sink_tito(x)
    b_sink(y)


def combine_sanitize_a_source_a_sink_tito(x):
    y = sanitize_a_source_tito(x)
    z = sanitize_a_sink_tito(y)
    return z


def no_issue_through_combine_sanitize_a_source_a_sink_tito():
    x = a_source()
    y = combine_sanitize_a_source_a_sink_tito(x)
    a_sink(y)


def propagation_of_sanitize_a_sink_in_source_trace():
    x = a_source()
    y = sanitize_a_sink_tito(x)
    return y


def no_issue_propagation_of_sanitize_a_sink_in_source_trace():
    x = propagation_of_sanitize_a_sink_in_source_trace()
    a_sink(x)


def sanitize_b_sink_tito(x):
    return x


def combine_sanitize_a_sink_b_sink_in_source_trace():
    x = propagation_of_sanitize_a_sink_in_source_trace()
    y = sanitize_b_sink_tito(x)
    return y


def sanitize_a_source_tito_with_sink(x):
    a_sink(x)
    return x


def sanitize_with_user_declared_source():
    return 0


def sanitize_with_user_declared_sink(x):
    return


def test4():
    x = a_source()
    y = sanitize_a_source_tito_with_sink(x)  # flow here
    a_sink(y)  # no flow here


def sanitize_b_sink_tito(x):
    return x


def no_issue_fixpoint_sanitize_sources():
    if 1 > 2:
        x = a_source()
        return sanitize_a_sink_tito(x)
    else:
        x = _test_source()
        y = sanitize_a_sink_tito(x)
        return sanitize_b_sink_tito(y)


def no_issue_fixpoint_sanitize_sinks(x):
    if 1 > 2:
        a_sink(x)
    else:
        y = sanitize_a_source_tito(x)
        b_sink(y)


def no_issue_fixpoint_sanitize():
    # Example where we need a fixpoint in the implementation to sanitize everything.
    # Sources: {NotSink[TestA]@TestA, NotSink[TestB]:NotSink[TestA]@Test}
    # Sinks: {TestA, NotSource[A]@TestB}
    x = no_issue_fixpoint_sanitize_sources()
    no_issue_fixpoint_sanitize_sinks(x)


def partial_issue_sources():
    if 1 > 2:
        x = a_source()
        return sanitize_a_sink_tito(x)
    else:
        return a_source()


def partial_issue_sinks(x):
    if 1 > 2:
        a_sink(x)
    else:
        y = sanitize_a_source_tito(x)
        a_sink(y)


def partial_issue_sanitize():
    # Sources: {NotSink[TestA]@TestA, TestA}
    # Sinks: {TestA, NotSource[testA]@TestA}
    x = partial_issue_sources()
    partial_issue_sinks(x)


def sanitize_test_a_source_attribute():
    if 1 > 2:
        x = a_source()
    else:
        x = b_source()
    c = C_sanitized_a_source(x)
    _test_sink(c.attribute)


def sanitize_test_a_source_attribute_in_sink_trace(x):
    c = C_sanitized_a_source(x)
    _test_sink(c.attribute)


def no_issue_sanitize_test_a_source_attribute_in_sink_trace():
    x = a_source()
    sanitize_test_a_source_attribute_in_sink_trace(x)


def issue_sanitize_test_a_source_attribute_in_sink_trace():
    x = b_source()
    sanitize_test_a_source_attribute_in_sink_trace(x)


def sanitize_test_a_source_attribute_in_tito(x):
    c = C_sanitized_a_source(x)
    return c.attribute


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


def sanitize_a_sink_attribute_in_source_trace():
    x = a_source()
    y = C_sanitized_a_sink(x)
    return y.attribute


def no_issue_sanitize_a_sink_attribute_in_source_trace():
    x = sanitize_a_sink_attribute_in_source_trace()
    a_sink(x)


def issue_sanitize_a_sink_attribute_in_source_trace():
    x = sanitize_a_sink_attribute_in_source_trace()
    b_sink(x)


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


def sanitize_parameter_all_tito(x, y):
    _test_sink(x)
    _test_sink(y)
    return source_with_tito(x) + source_with_tito(y)


def sanitize_parameter_no_user_controlled(x, y):
    if 1 > 2:
        return x
    elif 2 > 3:
        return y
    elif 3 > 4:
        _sql(x)
    else:
        _rce(y)


def propagation_of_sanitize_parameter_no_user_controlled(a, b):
    sanitize_parameter_no_user_controlled(b, a)


def no_issue_propagation_of_sanitize_parameter_no_user_controlled():
    x = _user_controlled()
    propagation_of_sanitize_parameter_no_user_controlled(0, x)


def issue_propagation_of_sanitize_parameter_no_user_controlled():
    x = _cookies()
    propagation_of_sanitize_parameter_no_user_controlled(0, x)


def sanitize_parameter_no_sql(x):
    if 1 > 2:
        _sql(x)
    elif 2 > 3:
        _rce(x)
    else:
        return x


def sanitize_parameter_no_rce(x):
    if 1 > 2:
        _sql(x)
    elif 2 > 3:
        _rce(x)
    else:
        return x


def sanitize_parameter_no_user_controlled_tito(x, y):
    if 1 > 2:
        return x
    else:
        return y


def no_propagation_with_sanitize_parameter_no_user_controlled_tito():
    a = _user_controlled()
    b = sanitize_parameter_no_user_controlled_tito(a, 0)
    return b


def propagation_of_cookies_with_sanitize_parameter_no_user_controlled_tito():
    b = _cookies()
    tito = sanitize_parameter_no_user_controlled_tito(b, 0)
    return tito


def propagation_of_sanitize_parameter_no_user_controlled_tito(a, b):
    return sanitize_parameter_no_user_controlled_tito(b, a)


def propagation_of_sanitize_parameter_no_user_controlled_tito_in_sink_trace(x):
    y = propagation_of_sanitize_parameter_no_user_controlled_tito(0, x)
    _sql(y)


def no_issue_propagation_of_sanitize_parameter_no_user_controlled_tito_in_sink_trace():
    x = _user_controlled()
    propagation_of_sanitize_parameter_no_user_controlled_tito_in_sink_trace(x)


def issue_propagation_of_sanitize_parameter_no_user_controlled_tito_in_sink_trace():
    x = _cookies()
    propagation_of_sanitize_parameter_no_user_controlled_tito_in_sink_trace(x)


def sanitize_parameter_no_sql_tito(x, y):
    if 1 > 2:
        return x
    else:
        return y


def no_propagation_with_sanitize_parameter_no_sql_tito(x):
    y = sanitize_parameter_no_sql_tito(x, 0)
    _sql(y)


def propagation_of_rce_with_sanitize_parameter_no_sql_tito(x):
    y = sanitize_parameter_no_sql_tito(x, 0)
    _rce(y)


def propagation_of_sanitize_parameter_no_sql_tito(a, b):
    return sanitize_parameter_no_sql_tito(b, a)


def propagation_of_sanitize_parameter_no_sql_tito_in_source_trace():
    x = _user_controlled()
    return propagation_of_sanitize_parameter_no_sql_tito(0, x)


def no_issue_propagation_of_sanitize_parameter_no_sql_tito_in_source_trace():
    x = propagation_of_sanitize_parameter_no_sql_tito_in_source_trace()
    _sql(x)


def issue_propagation_of_sanitize_parameter_no_sql_tito_in_source_trace():
    x = propagation_of_sanitize_parameter_no_sql_tito_in_source_trace()
    _rce(x)


def sanitize_parameter_with_user_declared_sink(x):
    return


def sanitize_return(x):
    _test_sink(x)
    return source_with_tito(x)


def sanitize_return_no_user_controlled(x):
    if 1 > 2:
        return _user_controlled()
    elif 2 > 3:
        return _cookies()
    else:
        return x


def sanitize_return_no_sql(x):
    return x


def propagation_of_sanitize_return_no_sql(x):
    return sanitize_return_no_sql(x)


def propagation_of_sanitize_return_no_sql_in_source_trace():
    x = _user_controlled()
    y = propagation_of_sanitize_return_no_sql(x)
    return y


def no_issue_propagation_of_sanitize_return_no_sql_in_source_trace():
    x = propagation_of_sanitize_return_no_sql_in_source_trace()
    _sql(x)


def issue_propagation_of_sanitize_return_no_sql_in_source_trace():
    x = propagation_of_sanitize_return_no_sql_in_source_trace()
    _rce(x)


def sanitize_return_no_cookies():
    if 1 > 2:
        x = _user_controlled()
    else:
        x = _cookies()
    return x


def sanitize_return_no_user_controlled_cookies():
    if 1 > 2:
        x = _user_controlled()
    else:
        x = _cookies()
    return x


def sanitize_return_no_rce():
    return _user_controlled()


def propagation_of_sanitize_return_no_rce():
    return sanitize_return_no_rce()


def no_issue_propagation_of_sanitize_return_no_rce():
    x = propagation_of_sanitize_return_no_rce()
    _rce(x)


def issue_propagation_of_sanitize_return_no_rce():
    x = propagation_of_sanitize_return_no_rce()
    _sql(x)


def sanitize_return_with_user_declared_source(x):
    return 0


def sanitize_all_parameters(x, y):
    _test_sink(x)
    _test_sink(y)
    return source_with_tito(x) + source_with_tito(y)


def sanitize_all_parameters_all_tito(x, y):
    _test_sink(x)
    _test_sink(y)
    return source_with_tito(x) + source_with_tito(y)


def sanitize_all_parameters_no_user_controlled(x):
    _test_sink(x)
    return x


def propagation_of_sanitize_all_parameters_no_user_controlled(x):
    sanitize_all_parameters_no_user_controlled(x)


def no_issue_propagation_of_sanitize_all_parameters_no_user_controlled():
    x = _user_controlled()
    propagation_of_sanitize_all_parameters_no_user_controlled(x)


def issue_propagation_of_sanitize_all_parameters_no_user_controlled():
    x = _cookies()
    propagation_of_sanitize_all_parameters_no_user_controlled(x)


def sanitize_all_parameters_no_sql(x):
    if 1 > 2:
        _sql(x)
    elif 2 > 3:
        _rce(x)
    else:
        return x


def sanitize_all_parameters_no_rce(x):
    if 1 > 2:
        _sql(x)
    elif 2 > 3:
        _rce(x)
    else:
        return x


def sanitize_all_parameters_no_user_controlled_tito(x):
    return x


def no_propagation_with_sanitize_all_parameters_no_user_controlled_tito():
    a = _user_controlled()
    b = sanitize_all_parameters_no_user_controlled_tito(a)
    return b


def propagation_of_cookies_with_sanitize_all_parameters_no_user_controlled_tito():
    b = _cookies()
    tito = sanitize_all_parameters_no_user_controlled_tito(b)
    return tito


def propagation_of_sanitize_user_controlled_tito_in_sink_trace(x):
    y = sanitize_all_parameters_no_user_controlled_tito(x)
    _sql(y)


def sanitize_all_parameters_no_sql_tito(x):
    return x


def no_propagation_with_sanitize_all_parameters_no_sql_tito(x):
    y = sanitize_all_parameters_no_sql_tito(x)
    _sql(y)


def propagation_of_rce_with_sanitize_all_parameters_no_sql_tito(x):
    y = sanitize_all_parameters_no_sql_tito(x)
    _rce(y)


def propagation_of_sanitize_sql_tito_in_source_trace():
    x = _user_controlled()
    y = sanitize_all_parameters_no_sql_tito(x)
    return y


def no_issue_propagation_of_sanitize_sql_tito_in_source_trace():
    x = propagation_of_sanitize_sql_tito_in_source_trace()
    _sql(x)


def sanitize_all_parameters_no_cookies_sql_tito(x):
    return x


def no_propagation_of_cookies_with_sanitize_all_parameters_no_cookies_sql_tito():
    a = _cookies()
    b = sanitize_all_parameters_no_cookies_sql_tito(a)
    return b


def propagation_of_user_controlled_with_sanitize_all_parameters_no_cookies_sql_tito():
    b = _user_controlled()
    tito = sanitize_all_parameters_no_cookies_sql_tito(b)
    return tito


def no_propagation_of_sql_with_sanitize_all_parameters_no_cookies_sql_tito(x):
    y = sanitize_all_parameters_no_cookies_sql_tito(x)
    _sql(y)


def propagation_of_rce_with_sanitize_all_parameters_no_cookies_sql_tito(x):
    y = sanitize_all_parameters_no_cookies_sql_tito(x)
    _rce(y)


def sanitize_all_parameters_with_user_declared_sink(x):
    return x


# Frame linking test


def sink_taint_sanitize_a(arg):
    arg = sanitize_a_source_tito(arg)
    _rce(arg)


def sink_taint_sanitize_a_sanitize_b(arg):
    arg = sanitize_b_source_tito(arg)
    sink_taint_sanitize_a(arg)


def sink_taint_sanitize_a_sanitize_b_santize_test(arg):
    arg = sanitize_test_source_tito(arg)
    sink_taint_sanitize_a_sanitize_b(arg)


def sink_taint_sanitize_b(arg):
    arg = sanitize_b_source_tito(arg)
    _rce(arg)


def sink_taint_sanitize_b_sanitize_a(arg):
    arg = sanitize_a_source_tito(arg)
    sink_taint_sanitize_b(arg)


def sink_taint_sanitize_b_sanitize_a_santize_test(arg):
    arg = sanitize_test_source_tito(arg)
    sink_taint_sanitize_b_sanitize_a(arg)
