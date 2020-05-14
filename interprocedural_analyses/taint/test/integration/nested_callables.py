# flake8: noqa
from builtins import __test_sink, __test_source


def foo():
    def inner():
        x = __test_source()
        __test_sink(x)

    def inner_with_model():
        return __test_source()
