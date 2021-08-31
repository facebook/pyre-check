# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import enum
from builtins import _test_sink, _test_source


def return_via_parameter_type(parameter):
    return 0


def test_strings():
    return return_via_parameter_type("A")


def test_numerals():
    return return_via_parameter_type(1)


def test_lists():
    return return_via_parameter_type(["a", "b"])


def meta(parameter):
    return return_via_parameter_type(parameter)


def test_via_type_of_does_not_propagate():
    return meta("Name")


def tito(parameter, other):
    pass


def test_tito():
    a = tito(_test_source(), [1, 2])
    return a


def sink_via_type_of(x, y):
    pass


def test_sink(element):
    return sink_via_type_of(element, 1)


def test_backwards_tito(parameter):
    return tito(parameter, "by_backwards")
