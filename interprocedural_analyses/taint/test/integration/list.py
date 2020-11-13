# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from builtins import __test_source, __test_sink


def create_zipped_source():
    x = [__test_source(), 1]
    y = [2, 3]
    return zip(x, y)


def create_zipped_source_with_all_tainted():
    x = [__test_source()]
    y = [__test_source()]
    z = [__test_source()]
    return zip(x, y, z)


def zipped_element_to_sink(x):
    l1 = [x]
    l2 = [1]

    for x, y in zip(l1, l2):
        __test_sink(x)


def zipped_element_not_flowing_to_sink(x):
    l1 = [x]
    l2 = [1]

    for x, y in zip(l1, l2):
        __test_sink(y)
