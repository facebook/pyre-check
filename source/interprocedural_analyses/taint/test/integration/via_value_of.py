# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import enum
from builtins import _test_sink, _test_source


def return_via_parameter_name(parameter=None):
    return 0


class MyEnum(enum.Enum):
    FOO = 1


def test_string_literals():
    return return_via_parameter_name("A")


def test_numerals():
    return return_via_parameter_name(1)


def test_bool():
    return return_via_parameter_name(False)


def test_enums():
    return return_via_parameter_name(MyEnum.FOO)


def test_missing():
    return return_via_parameter_name()


def meta(parameter):
    return return_via_parameter_name(parameter)


def test_via_value_of_does_not_propagate():
    return meta("Name")


def tito(parameter, other):
    pass


def test_tito():
    a = tito(_test_source(), "second")
    return a


def sink_via_value_of(x, y):
    pass


def test_sink(element):
    return sink_via_value_of(element, "second")


def test_backwards_tito(parameter):
    return tito(parameter, "by_backwards")


def meta_sink(parameter, value):
    sink_via_value_of(parameter, value)


def test_sinks_do_not_propagate(parameter):
    meta_sink(parameter, "not a feature")


def attach_to_source(parameter):
    return _test_source()


def test_attach_to_source():
    return attach_to_source("attached to source")


def attach_to_sink(parameter, feature):
    _test_sink(parameter)


def test_attach_to_sink(parameter):
    attach_to_sink(parameter, "attached to sink")


def return_including_name(parameter):
    return 0


def test_return_including_name():
    return return_including_name("parameter_value")


def return_via_second_parameter(first, second):
    return 0


def test_return_second_parameter():
    return return_via_second_parameter(1, 2)


def test_return_second_parameter_keyword():
    return return_via_second_parameter(second=2, first=1)
