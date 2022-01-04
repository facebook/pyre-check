# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_source


class C:
    def update(self, parameter):
        ...

    def taint_parameter(self, tainted_parameter):
        ...


class D(C):
    def update(self, parameter):
        ...

    def taint_parameter(self, tainted_parameter):
        ...


def test_obscure_tito():
    c = C()
    c.update(_test_source())
    return c


def test_obscure_return():
    c = C()
    return c.update(_test_source())


def test_obscure_sink(parameter):
    c = C()
    c.taint_parameter(parameter)
