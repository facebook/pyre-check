# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import __test_sink, __test_source


class Test:
    @classmethod
    def foo(cls, x) -> None:
        return __test_sink(x)


def bar():
    Test.foo(__test_source())
