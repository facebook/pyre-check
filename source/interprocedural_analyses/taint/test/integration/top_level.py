# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source

x = _test_source()
_test_sink(x)


def foo(x):
    _test_sink(x)


y = _test_source()
foo(y)
