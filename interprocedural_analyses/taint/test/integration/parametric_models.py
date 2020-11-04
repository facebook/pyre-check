# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa


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
