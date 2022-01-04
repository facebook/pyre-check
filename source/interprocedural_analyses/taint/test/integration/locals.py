# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from builtins import _test_sink, _test_source


def named_sink(x):
    _test_sink(x)


def locals_to_sink():
    # No issue before assigning.
    _test_sink(locals()["x"])
    x = _test_source()

    _test_sink(locals()["x"])
    _test_sink(locals()["y"])

    # We properly handle named parameters through `**`.
    named_sink(**locals())


# Note the limitation in the model - we won't track that `x` flows to a sink..
def source_parameter_to_sink(x, y):
    _test_sink(locals()["x"])
    _test_sink(locals()["y"])
