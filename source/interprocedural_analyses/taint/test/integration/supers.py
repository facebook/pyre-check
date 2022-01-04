# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


class A:
    def __init__(self):
        self.attribute = _test_source()

    def f1(self):
        _test_sink(self.attribute)

    def f2(self, x):
        _test_sink(x)

    def f3(self):
        return _test_source()

    def f4(self):
        return "1"

    def f5(self, x):
        pass


class B(A):
    def f1(self):
        return "1"

    def f4(self):
        return _test_source()

    def f5(self, x):
        return _test_sink(x)

    def g1(self):
        super(B, self).f1()
        super().f1()
        super().f2(super().f3())

    def g2(self):
        super(B, self).f5(super(B, self).f4())
        super().f5(super().f4())

    def g3(self):
        self.f5(super().f4())
        super().f5(self.f4())

    def g4(self):
        self.f5(self.f4())


class C(A):
    def f2(self, x):
        return "1"

    def g1(self):
        return super().f1()


class D(C):
    def g1(self):
        super().f1()


class E(B, A):
    def g1(self):
        super().f1()

    def g2(self):
        super(E, self).f1()

    def g3(self):
        super(B, self).f1()


def attribute_B_not_overwritten():
    B().g1()


def attribute_B_overwritten():
    b = B()
    b.attribute = "1"
    b.g1()


def B_overwrite_both():
    b = B()
    b.g2()


def B_overwrite_partial():
    b = B()
    b.g3()


def B_standard():
    b = B()
    b.g4()


def attribute_C_not_overwritten():
    C().g1()


def attribute_C_overwritten():
    c = C()
    c.attribute = "1"
    c.g1()


def attribute_D_not_overwritten():
    d = D()
    d.g1()


def attribute_D_overwritten():
    d = D()
    d.attribute = "1"
    d.g1()


def attribute_E_not_overwritten():
    e = E()
    e.g1()
    e.g2()


def attribute_E_not_overwritten_RCE():
    # TODO(T108231862): Support Diamond Inheritance.
    e = E()
    e.g3()


def attribute_E_overwritten():
    e = E()
    e.attribute = "1"
    e.g1()
    e.g2()
    e.g3()
