# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def inner_sink_wrapper():
    def inner_sink(x):
        ...

    inner_sink(_test_source())
