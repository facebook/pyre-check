# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source, to_callable_target


@to_callable_target
def callable_target(arg):
    _test_sink(arg)


def test_callable_target():
    x = _test_source()
    callable_target.async_schedule(x)
