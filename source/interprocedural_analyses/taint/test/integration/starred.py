# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
from builtins import __test_sink, __test_source


def sink(json):
    __test_sink(json)


def test():
    query = {"json": __test_source()}
    sink(query)
