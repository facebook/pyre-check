#!/usr/bin/env python3
# flake8: noqa


def foo() -> str:
    def bar() -> str:
        return 0

    return bar()
