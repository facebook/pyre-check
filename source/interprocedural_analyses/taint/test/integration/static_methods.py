# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


class StaticClass:
    @staticmethod
    def sink(oops):
        _test_sink(oops)


def test(source):
    return StaticClass.sink(source)


def run_test(source):
    test(_test_source())
