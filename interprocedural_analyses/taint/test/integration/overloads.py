# flake8: noqa

from typing import Union, overload


@overload
def f(x: int) -> None:
    pass


@overload
def f(x: str) -> None:
    pass


def f(x: Union[int, str]) -> None:
    call_me(x)


def call_me(x):
    __test_sink(x)
