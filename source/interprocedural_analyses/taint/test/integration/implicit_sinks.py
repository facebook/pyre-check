# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from builtins import _test_source


def propagate_sink(a):
    f"<{a}>"


def inline_issue():
    a = _test_source()
    f"<{a}>"
    f"{a}"
