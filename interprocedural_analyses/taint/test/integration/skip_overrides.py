# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import __test_sink, __test_source


class C:
    def obscure(self, x, y):
        ...

    def not_obscure(self, x, y):
        ...


def test_returning_obscure_taint_if_no_model(c: C):
    return c.obscure(0, __test_source())


def test_no_taint_if_model_exists(c: C):
    return c.not_obscure(0, __test_source())
