#!/usr/bin/env python3
# flake8: noqa
class Base:
    def foo(self, x: int) -> None:
        pass


# This class inherits incorrectly from its parent.
class Derived(Base):
    def foo(self, x: str) -> None:
        pass


class Standalone:
    pass
