# flake8: noqa
import multiprocessing
from builtins import __test_sink, __test_source
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


def multiprocessing_tainted():
    multiprocessing.Process(target=a_flows_to_sink, args=(__test_source(), 1))


def multiprocessing_not_tainted():
    multiprocessing.Process(target=a_flows_to_sink, args=(1, __test_source()))
