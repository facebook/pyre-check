# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink


def no_if(x):
    _test_sink(x)


def with_if(x):
    if _test_sink(x):
        pass
    pass
