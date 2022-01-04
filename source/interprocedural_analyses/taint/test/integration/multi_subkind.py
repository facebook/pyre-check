# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def sink_a(arg):
    pass


def sink_b(arg):
    pass


def source_a():
    return 1


def source_b():
    return 2


def multi_sink(d):
    sink_a(d["a"])
    sink_b(d["b"])


def issue():
    d = {}
    d["a"] = source_a()
    d["b"] = source_b()
    multi_sink(d)
