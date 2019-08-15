# flake8: noqa

from typing import Any


def len(o: Any) -> int:
    ...


def min(x: int, y: str):
    ...


def named(*, named_parameter: int, **kw):
    ...


def tito_via_len(o: Any):
    return len(o)


def tito_via_min_left(o: Any):
    return min(o, "")


def tito_via_min_right(o: Any):
    return min(5, o)


def tito_via_named(o: Any):
    return named(named_parameter=o)


def tito_via_min_or_not(o: Any, b: bool):
    if b:
        return min(o, 5)
    else:
        return o
