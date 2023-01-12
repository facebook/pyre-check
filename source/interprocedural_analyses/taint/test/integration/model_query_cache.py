# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


class Base:
    pass


class A(Base):
    def foo(self, x):
        return 0

    def bar(self, x):
        return 1


class B(Base):
    def foo(self, x):
        return 0

    def bar(self, x):
        return 1


class C(Base):
    def foo(self, x):
        return 0

    def bar(self, x):
        return 1


class DoesNotExtendsBase:
    def foo(self, x):
        return 0

    def bar(self, x):
        return 1


def decorated(f, *args, **kwargs):
    pass


@decorated
class X:
    def foo(self, x):
        return 0

    def bar(self, x):
        return 1


@decorated
class Y:
    def foo(self, x):
        return 0

    def bar(self, x):
        return 1


class Z:
    def foo(self, x):
        return 0

    def bar(self, x):
        return 1


class Table:
    pass


class FooTable(Table):
    def attribute_x(self):
        return 0

    def attribute_y(self):
        return 0


class BarTable(Table):
    def attribute_x(self):
        return 0

    def attribute_z(self):
        return 0

    def non_attribute_t(self):
        return 0
