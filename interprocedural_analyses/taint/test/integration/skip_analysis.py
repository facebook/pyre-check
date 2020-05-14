#!/usr/bin/python3
# flake8: noqa
from builtins import __test_sink, __test_source


class SkipMe:
    def taint_here(self, x):
        __test_sink(x)

    def tito_here(self, x):
        return x


def no_issue_due_to_skip():
    x = __test_source()
    skip = SkipMe()
    skip.taint_here(x)
    __test_sink(skip.tito_here(x))
