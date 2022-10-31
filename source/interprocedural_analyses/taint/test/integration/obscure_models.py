# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


class C:
    def obscure(self, x=0, y=0):
        ...

    def obscure_with_skip_overrides(self, x, y):
        ...

    def obscure_with_skip_inlining(self, x, y):
        ...

    def obscure_with_source(self, x, y):
        ...

    def obscure_with_skip_obscure(self, x, y):
        ...

    def obscure_with_skip_obscure_and_tito(self, x, y):
        ...

    def obscure_with_multiple_models(self, x, y):
        ...

    def obscure_with_tito(self, x):
        ...


def test_obscure(c: C):
    return c.obscure(0, _test_source())


def test_obscure_with_skip_overrides(c: C):
    return c.obscure_with_skip_overrides(0, _test_source())


def test_obscure_with_skip_inlining(c: C):
    return c.obscure_with_skip_inlining(0, _test_source())


def test_obscure_with_source(c: C):
    return c.obscure_with_source(0, _test_source())


def test_obscure_with_skip_obscure(c: C):
    return c.obscure_with_skip_obscure(0, _test_source())


def test_obscure_with_skip_obscure_and_tito(c: C):
    return c.obscure_with_skip_obscure_and_tito(0, _test_source())


def test_obscure_with_multiple_models(c: C):
    return c.obscure_with_multiple_models(0, _test_source())


def test_obscure_with_tito(c: C):
    _test_sink(c.obscure_with_tito(_test_source()))


def test_issue(c: C):
    x = _test_source()
    y = c.obscure(x)
    _test_sink(y)


def test_collapse_source(c: C):
    x = {"a": _test_source()}
    y = c.obscure(x)
    _test_sink(y["b"])


def test_sink_collapse(arg, c: C):
    x = c.obscure(arg)
    _test_sink(x["a"])


def should_collapse_depth_zero(arg, c: C):
    return c.obscure(arg)


def test_collapse_depth():
    x = {"a": _test_source()}
    y = should_collapse_depth_zero(x, C())
    _test_sink(y["b"])
