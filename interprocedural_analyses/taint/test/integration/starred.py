# flake8: noqa
from builtins import __test_sink, __test_source


def sink(json):
    __test_sink(json)


def test():
    query = {"json": __test_source()}
    sink(query)
