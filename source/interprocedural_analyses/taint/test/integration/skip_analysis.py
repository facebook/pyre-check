#!/usr/bin/python3
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
from builtins import __test_sink, __test_source


class SkipMe:
    def taint_here(self, x):
        __test_sink(x)

    def tito_here(self, x):
        return x


def no_issue_due_to_skip():
    x = __test_source()
    skip = SkipMe()
    skip.taint_here(x)
    __test_sink(skip.tito_here(x))
