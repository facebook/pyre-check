# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
from builtins import __test_sink, __test_source


def test():
    # fmt: off
    __test_sink([
        "foo",
        __test_source(),
    ])
    # fmt: on
