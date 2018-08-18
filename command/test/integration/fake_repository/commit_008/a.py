#!/usr/bin/env python3
from typing import Protocol


class MyProtocol(Protocol):
    def foo(self) -> int:
        ...


class Implements:
    def foo(self) -> int:
        return 1


def expects_my_protocol(x: MyProtocol) -> None:
    pass


def should_not_error(x: Implements) -> None:
    expects_my_protocol(x)
