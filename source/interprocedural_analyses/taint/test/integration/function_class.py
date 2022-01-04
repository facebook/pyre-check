# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def test():
    class A:
        def __init__(self):
            return _test_sink(_test_source())

    # TODO(T108223424): Analyze classes defined within functions.
    A()
