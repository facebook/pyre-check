#!/usr/bin/env python3
# flake8: noqa


def foo() -> int:
    def bar() -> int:
        return 0

    return bar()
