# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Class attributes where the model is inferred
# and not explicitly set in the .pysa file

from builtins import _test_sink, _test_source


class A:
    a = ""
    b = ""

    def __init__(self, c):
        A.b = _test_source()
        self.c = c
        self.d = _test_source()

    def sink_a(self):
        _test_sink(A.a)

    def sink_b(self):
        # TODO(T145247918): False negative, request from seceng to
        # find this issue even without an explicit A().sink_b()
        _test_sink(A.b)

    def sink_c(self):
        _test_sink(self.c)

    def sink_d(self):
        # TODO(T145247918): False negative, request from seceng to
        # find this issue even without an explicit A().sink_d()
        _test_sink(self.d)


def class_attribute_A_a_source():
    A.a = _test_source()


def class_attribute_A_a_sink():
    _test_sink(A.a)


def class_attribute_A_a_flow():
    # TODO(T145247918): False negative
    class_attribute_A_a_source()
    class_attribute_A_a_sink()


def class_attribute_A_a_no_flow():
    class_attribute_A_a_sink()
    class_attribute_A_a_source()


def class_attribute_A_b_sink():
    _test_sink(A.b)


def class_attribute_A_b_flow1():
    # TODO(T145247918): False negative
    A()
    class_attribute_A_b_sink()


def class_attribute_A_b_flow2():
    # TODO(T145247918): False negative
    A().sink_b()


def instance_attribute_A_c_no_flow():
    A().sink_c()


def instance_attribute_A_d_flow():
    A().sink_d()
