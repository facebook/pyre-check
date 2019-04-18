#!/usr/bin/env python3
# flake8: noqa


def foo() -> str:
    def bar() -> str:
        return ""

    return bar()
