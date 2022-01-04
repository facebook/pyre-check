# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
from builtins import _test_sink, _test_source


def sink(json):
    _test_sink(json)


def test():
    query = {"json": _test_source()}
    sink(query)
