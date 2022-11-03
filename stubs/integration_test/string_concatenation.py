# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from integration_test.taint import source, sink


def concatenate_lhs():
    return source() + "A"


def concatenate_rhs():
    return "A" + source()


def bad_1():
    a = concatenate_lhs()
    sink(a)  # noqa


def bad_2():
    a = concatenate_rhs()
    sink(a)  # noqa
