# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import __test_sink, __test_source


class C:
    def obscure(self, x, y):
        ...

    def not_obscure_tito(self, x, y):
        pass

    def not_obscure_not_tito(self, x, y):
        pass


def test_obscure(c: C):
    return c.obscure(0, __test_source())


def test_not_obscure_tito(c: C):
    return c.not_obscure_tito(0, __test_source())


def test_not_obscure_not_tito(c: C):
    return c.not_obscure_not_tito(0, __test_source())
