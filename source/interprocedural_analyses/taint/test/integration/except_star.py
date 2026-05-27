# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Testing except* (PEP 654) exception group handling


def test_except_star():
    try:
        pass
    except* ValueError as e:
        print(e)
