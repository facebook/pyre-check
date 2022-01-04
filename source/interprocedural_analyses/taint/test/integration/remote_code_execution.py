# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _user_controlled


def rce_problem():
    x = _user_controlled()
    eval(x)
