# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from abc import abstractmethod
from pysa import _test_sink, _test_source
from typing import Union, Any
import random

"""
  A0
 /  \
B0   C0
"""


class A0:
    def m1(self, x):
        self.m2(x)

    def m2(self, x):
        # TODO(T114456058): Unexpected position -1 in the sinks of
        # override models
        pass


class B0(A0):
    def m0(self, x):
        self.m1(x)


class C0(A0):
    def m2(self, x):
        _test_sink(x)  # No issue


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
        _test_sink(x)  # Issue here


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
        _test_sink(x)  # Issue here


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
        _test_sink(x)  # No issue


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
        _test_sink(x)  # No issue


def sink_two_hops(b: B5):
    b.m0(_test_source())


"""
   A6: [1,8]
      /  \
     /    \
B6: [2,5]   C6: [6,7]
  |
  |
D6: [3,4]

E6: [9,10]
"""


class A6:
    def m1(self):
        return self.m0()  # Interval: [2,5] /\ [1,8] = [2,5]

    def m0(self) -> Any:
        return


class B6(A6):
    def m0(self) -> Any:
        if random.random() > 0.5:
            return _test_source()  # Interval: (-∞,+∞) /\ [2,5] = [2,5]
        else:
            return E6().m3()  # Interval: [2,5]


class C6(A6):
    def m2(self) -> Any:
        return self.m1()  # Interval: [2,5] /\ [6,7] = Empty


class D6(B6):
    def m0(self) -> Any:
        return super().m0()


class E6:
    def m3(self) -> Any:
        return _test_source()  # Interval: (-∞,+∞) /\ [9,10] = [9,10]


def propagate_source_empty(c: C6):
    return _test_sink(c.m1())  # Interval: [6,7] /\ [2,5] = Empty


"""
A7: [1,2]
B7: [3,4]
"""


class B7:
    def foo(self) -> Any:
        return self.bar()  # Interval: [3,4]

    def bar(self) -> Any:
        return _test_source()  # Interval: [3,4]


class A7:
    def bar(self, x) -> Any:
        return x

    def f(self, b: B7) -> Any:
        y = b.foo()  # Interval: [1,2]
        return y


"""
A8: [1,2]
B8: [3,4]
C8: [5,6]
"""


class B8:
    def foo(self, x) -> Any:
        return _test_source()  # Interval: [3,4]


class C8:
    def foo(self, x) -> Any:
        pass


class A8:
    def bar(self, b: Union[B8, C8], x) -> Any:  # Interval: [1,2]
        if x == 1:
            return self.baz()  # Interval: [1,2] /\ [1,2] = [1,2]
        elif x == 2:
            # Interval: [1,2]. First, this may return a source because b may
            # resolve to B and the intersection between B’s interval and
            # B.foo’s interval is not empty. Second, the interval is [1,2]
            # because we forget the original interval [3,4] and impose A’s
            # interval.
            return b.foo(x)
        elif x == 3:
            # Interval: [1,2]. The reasoning is the same as that of b.foo(x).
            return A8().baz()
        else:
            return x  # No taint (or interval)

    def baz(self) -> Any:
        return _test_source()  # Interval: (-∞,+∞) /\ [1,2] = [1,2]


class A9:
    def f(self) -> Any:
        return _test_source()


def call_method_via_class_name(a: A9):
    return A9.f(a)


class A10:
    f: int = 0

    def object_target(self, x):
        a = A10()
        a.f = x


class A12:
    def f(self) -> Any:
        # pyrefly: ignore[missing-attribute]
        return self.g()  # pyre-ignore


class B12(A12):
    def g(self) -> Any:
        return 0


class C12(A12):
    def g(self) -> Any:
        return _test_source()


def undetected_issue(c: C12):
    x = c.f()
    # TODO(T116242472): Should detect an issue here, but this may not need to
    # be addressed because Pyre reports a type error above
    _test_sink(x)


class A13:
    def f(self) -> Any:
        return self.g()

    @abstractmethod
    def g(self) -> Any:
        return


class B13(A13):
    def g(self) -> Any:
        return 0


class C13(A13):
    def g(self) -> Any:
        return _test_source()


def abstract_method(b: B13, c: C13):
    x = c.f()
    _test_sink(x)  # Issue here
    y = b.f()
    _test_sink(y)  # No issue here


"""
           A: [1,8]
          / \
         /   \
        /     \
B: [2,5]   C: [6,7] \/ [3,4]
        \     /
         \   /
          \ /
           D: [3,4]
"""


class A14:
    def m1(self) -> Any:
        return self.m2()

    def m2(self) -> Any:
        return


class C14(A14):
    def m2(self) -> Any:
        return _test_source()


class B14(A14):
    def m0(self) -> Any:
        return self.m1()

    def m2(self) -> Any:
        return 0


class D14(B14, C14):
    pass


def multi_inheritance_no_issue_one_hop(b: B14):
    _test_sink(b.m0())


class A15:
    def m1(self) -> Any:
        return self.m2()

    def m2(self) -> Any:
        return


class E15(A15):
    def m2(self) -> Any:
        return _test_source()


class B15(A15):
    def m0(self) -> Any:
        return self.m1()

    def m2(self) -> Any:
        return 0


class C15(B15):
    pass


class D15(C15, E15):
    pass


def multi_inheritance_no_issue_two_hops(b: B15):
    _test_sink(b.m0())


class A16:
    def m1(self) -> Any:
        return self.m2()

    def m2(self) -> Any:
        return


class C16(A16):
    def m2(self) -> Any:
        return 0


class B16(A16):
    def m0(self) -> Any:
        return self.m1()

    def m2(self) -> Any:
        return 0


class D16(B16, C16):
    def m2(self) -> Any:
        return _test_source()


def multi_inheritance_issue(b: B16):
    _test_sink(b.m0())  # b may have type D16


def sink_b(arg):
    pass


def sink_c(arg):
    pass


class A17:
    @classmethod
    def m0(cls, arg):
        cls.m1(arg)

    @classmethod
    def m1(cls, arg):
        pass

    @classmethod
    def m2(cls, arg):
        D17.m1(arg)  # Change the receiver object from `cls` to `D17`


class B17(A17):
    @classmethod
    def m1(cls, arg):
        pass


class C17(A17):
    @classmethod
    def m1(cls, arg):
        sink_c(arg)


class D17(A17):
    @classmethod
    def m1(cls, arg):
        sink_c(arg)


def test_class_methods():
    # Expect no issue
    B17.m0(_test_source())

    # Expect no issue as well
    b = B17()
    b.m0(_test_source())

    # Expect an issue, which is not pruned away by class intervals (unlike the
    # above call to B17.m0), because the receiver objects on the sink trace
    # change from B17 to D17 (instead of remaining the same B17)
    B17.m2(_test_source())


class A18:
    pass


class B18(A18):
    @staticmethod
    def m0(arg):
        sink_b(arg)


class C18(A18):
    @staticmethod
    def m0(arg):
        # Expect an issue
        B18.m0(arg)


def test_static_methods():
    # Expect an issue
    C18.m0(_test_source())

    # Expect an issue
    c = C18()
    c.m0(_test_source())


class A19:
    @classmethod
    def m0(c, arg):
        # TODO(T114580705): Currently we consider `c` not as a call to the class itself,
        # because we match syntactically only against `cls`
        c.m1(arg)

    @classmethod
    def m1(c, arg):
        _test_sink(arg)

    def m2(s, arg):
        # TODO(T114580705): Currently we consider `s` not as a call to `self`,
        # because we match syntactically only against `self`
        s.m3(arg)

    def m3(s, arg):
        _test_sink(arg)


"""
           A
          / \
         /   \
        /     \
       B      C
        \     /
         \   /
          \ /
           D
"""


class A20:
    def m0(self, arg):
        pass


class B20(A20):
    pass


class C20(A20):
    def m0(self, arg):
        _test_sink(arg)


class D20(C20, B20):
    pass


def test_multi_inheritance_parent_call(b: B20):
    # Resolved to direct call to A.m0 since we only look up parents and
    # children, but it can also call C.m0 (sibling).
    b.m0(_test_source())


def test_multi_inheritance_parent_issue():
    test_multi_inheritance_parent_call(D20())  # TODO(T183494356): False negative.


def transformX(arg):
    pass


def sink_d(arg):
    pass


class A21:
    @classmethod
    def m0(cls, arg) -> Any:
        return cls.m1(arg)

    @classmethod
    def m1(cls, arg) -> Any:
        return


class B21(A21):
    @classmethod
    def m1(cls, arg) -> Any:
        return transformX(arg)


class C21(A21):
    @classmethod
    def m1(cls, arg) -> Any:
        return arg


def no_issue_taint_transform_with_class_interval_for_classmethods():
    # Should not see an issue, due to not going through the taint transform
    sink_d(C21.m0(_test_source()))


class A22:
    def m0(self, arg) -> Any:
        return self.m1(arg)

    def m1(self, arg) -> Any:
        return


class B22(A22):
    def m1(self, arg) -> Any:
        return transformX(arg)


class C22(A22):
    def m1(self, arg) -> Any:
        return arg


def no_issue_taint_transform_with_class_interval(c: C22):
    # Should not see an issue, due to not going through the taint transform
    sink_d(c.m0(_test_source()))


def add_feature_c(arg) -> Any:
    return arg


def add_feature_d(arg) -> Any:
    return arg


def add_feature_e(arg) -> Any:
    return arg


class A23:
    def m0(self, a) -> Any:
        return self.m1(a)

    def m1(self, a) -> Any:
        return a


class B23(A23):
    pass


class C23(B23):
    def m1(self, a) -> Any:
        return add_feature_c(a)


class D23(B23):
    def m1(self, a) -> Any:
        return add_feature_d(a)


class E23(A23):
    def m1(self, a) -> Any:
        return add_feature_e(a)


def issue_precise_tito_intervals(b: B23):
    # False positive: Should see feature_c and feature_d but not feature_e, due to distinguishing
    # the breadcrumbs from different tito intervals
    _test_sink(b.m0(_test_source()))


class A24:
    def m0(self) -> Any:
        return A24.tito(self.m1())

    def m1(self) -> Any:
        return 0

    @staticmethod
    def tito(a) -> Any:
        return a


class B24(A24):
    def m1(self) -> Any:
        return _test_source()


class C24(A24):
    pass


def no_issue_source_through_staticmethod_tito(c: C24):
    _test_sink(c.m0())  # Should NOT find an issue
