# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa


def cross_repository_source(source_parameter):
    __test_sink(source_parameter)


def returns_crtex_source():
    pass


def test():
    s = returns_crtex_source()
    __test_sink(s)
