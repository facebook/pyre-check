# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
from builtins import __test_sink, __test_source


class Wrapper:
    def __init__(self, a, b):
        self.a = a
        self.b = b


class C:
    def __init__(self, wrapper: Wrapper) -> None:
        self.x = wrapper
        self.y = wrapper.b


def y_is_benign():
    wrapper = Wrapper(a=__test_source(), b=0)
    c = C(wrapper)
    __test_sink(c.y)
