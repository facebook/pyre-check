#!/usr/bin/env python3
from a import Derived


# This class inherits incorrectly from its parent.
class AnotherDerived(Derived):
    def foo(self, x: int) -> None:
        pass
