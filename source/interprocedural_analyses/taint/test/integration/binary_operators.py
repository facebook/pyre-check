# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


class Add:
    def __add__(self, other):
        _test_sink(other)


def test1():
    add = Add()
    add + _test_source()


def test2():
    add = Add()
    add += _test_source()
