# flake8: noqa

from functools import partial


def a_flows_to_sink(a, b):
    __test_sink(a)


def partial_application_with_tainted():
    x = __test_source()
    partial(a_flows_to_sink, x)


def partial_application_with_benign():
    x = 1
    partial(a_flows_to_sink, x)


def partial_application_with_named_a():
    x = __test_source()
    partial(a_flows_to_sink, a=x)


def partial_application_with_named_b():
    x = __test_source()
    partial(a_flows_to_sink, b=x)
