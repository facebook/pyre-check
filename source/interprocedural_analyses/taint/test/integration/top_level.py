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


def create_global_source():
    global z
    z = _test_source()


create_global_source()


def return_global_source():
    # TODO(T123109154): We should see a model here, because
    # global variable z is a source
    return z
