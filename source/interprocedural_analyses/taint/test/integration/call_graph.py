# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source

# Demonstrate a (currently fixed) false positive due to type resolution.

class IsSource:
    def __init__(self) -> None:
        pass

    def method(self) -> str:
        return _test_source()


class NotSource:
    def __init__(self) -> None:
        pass

    def method(self) -> IsSource:
        pass


def test_multi_assign():
    x = NotSource()
    x = y = x.method()
    # Treating it as x = x.method(); x = y would lead to a false positive.
    _test_sink(x)  # No source
    _test_sink(y)  # No source
