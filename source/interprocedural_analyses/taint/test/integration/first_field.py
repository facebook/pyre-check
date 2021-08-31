# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from builtins import _test_sink, _test_source


def alternate_fields():
    d = {"a": _test_source(), "b": _test_source()}
    if 1 > 2:
        x = d["a"]
    else:
        x = d["b"]
    _test_sink(x)
    return x
