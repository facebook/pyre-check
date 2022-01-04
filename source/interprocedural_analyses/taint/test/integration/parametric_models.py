# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa


from builtins import _test_source


def f():
    ...


def g():
    ...


def sink(x):
    ...


def f_and_g_to_test():
    if 1 > 2:
        a = f()
    else:
        a = g()

    sink(a)


def sink_subkind_a(x):
    ...


def sink_subkind_b(x):
    ...


def inferred_sink(x):
    if 1 > 2:
        sink_subkind_a(x)
    else:
        sink_subkind_b(x)


def test_to_subkind_sink():
    x = _test_source()
    inferred_sink(x)
