# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink

"""
This test demonstrates a case where the analysis is exponential due to 4
methods recursively calling each other with different paths. The set of sinks
would potentially be infinite, it is all combinations of [a], [b], [c] with an
ending [x], i.e `([a]|[b]|[c])+[x]` in a regular expression. This is prevented
using the widening on the taint tree, which limits the maximum depth of the
tree.
"""


class Base:
    def method(self) -> None:
        pass


class A(Base):
    def __init__(self, base: Base) -> None:
        self.a: Base = base

    def method(self):
        self.a.method()


class B(Base):
    def __init__(self, base: Base) -> None:
        self.b: Base = base

    def method(self):
        self.b.method()


class C(Base):
    def __init__(self, base: Base, x: str) -> None:
        self.c: Base = base
        self.x: str = x

    def method(self):
        _test_sink(self.x)
        self.c.method()
