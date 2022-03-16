# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source

"""
  A0
 /  \
B0   C0
"""


class A0:
    def m1(self, x):
        self.m2(x)

    def m2(self, x):
        pass  # TODO(T114456058): Unexpected position -1 in the sinks of override models


class B0(A0):
    def m0(self, x):
        self.m1(x)


class C0(A0):
    def m2(self, x):
        _test_sink(x)  # Invalid issue


def canonical_example(b: B0):
    b.m0(_test_source())


"""
  A1
 /  \
B1   C1
"""


class A1:
    def m1(self, x):
        self.m2(x)

    def m2(self, x):
        pass


class B1(A1):
    def m0(self, x):
        self.m1(x)

    def m1(self, x):
        pass


class C1(A1):
    def m2(self, x):
        _test_sink(x)  # No issue


def no_call_to_parent_class(b: B1):
    b.m0(_test_source())


"""
  A2
 /  \
B2   C2
 \  /
  D2
"""


class A2:
    def m1(self, x):
        self.m2(x)

    def m2(self, x):
        pass


class B2(A2):
    def m0(self, x):
        self.m1(x)


class C2(A2):
    def m2(self, x):
        pass


class D2(B2, C2):
    def m2(self, x):
        _test_sink(x)  # Valid issue


def multiple_inheritance(b: B2):
    b.m0(_test_source())


"""
  A3
  |
  B3
 /  \
C3  D3
"""


class A3:
    def m1(self, x):
        self.m2(x)

    def m2(self, x):
        pass


class B3(A3):
    def m0(self, x):
        self.m1(x)


class C3(B3):
    def m0(self, x):
        self.m1(x)

    def m2(self, x):
        _test_sink(x)  # Valid issue


class D3(B3):
    def m0(self, x):
        pass

    def m2(self, x):
        pass  # Issue or not?


def sink_in_subclass(b: B3):
    b.m0(_test_source())


"""
  A4
 /  \
B4   C4
|
D4
"""


class A4:
    def m2(self, x):
        self.m3(x)

    def m3(self, x):
        pass


class B4(A4):
    def m1(self, x):
        self.m2(x)


class C4(A4):
    def m3(self, x):
        _test_sink(x)  # Invalid issue


class D4(B4):
    def m0(self, x):
        self.m1(x)


def source_two_hops(d: D4):
    d.m0(_test_source())


"""
  A5
 /  \
B5   C5
     |
     D5
"""


class A5:
    def m1(self, x):
        self.m2(x)

    def m2(self, x):
        pass


class B5(A5):
    def m0(self, x):
        self.m1(x)


class C5(A5):
    pass


class D5(C5):
    def m2(self, x):
        _test_sink(x)  # Invalid issue


def sink_two_hops(b: B5):
    b.m0(_test_source())
