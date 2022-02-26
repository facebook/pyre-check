# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


class Test:
    @classmethod
    def foo(cls, x) -> None:
        return _test_sink(x)


def bar():
    Test.foo(_test_source())
