#!/usr/bin/env python3
class C(metaclass=Missing):
    pass


def bar(c: C) -> int:
    return c.__hash__()
