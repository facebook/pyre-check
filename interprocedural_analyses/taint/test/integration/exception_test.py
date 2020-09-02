# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def test_parameter_flow(ex: Exception):
    return str(ex)


def test_constructed_exception():
    ex = Exception("message")
    return str(ex)


def test_caught_exception():
    try:
        return ""
    except Exception as ex:
        return str(ex)
