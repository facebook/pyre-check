# flake8: noqa

import enum


def return_via_parameter_name(parameter):
    return 0


class MyEnum(enum.Enum):
    FOO = 1


def test_string_literals():
    return return_via_parameter_name("A")


def test_numerals():
    return return_via_parameter_name(1)


def test_enums():
    return return_via_parameter_name(MyEnum.FOO)


def meta(parameter):
    return return_via_parameter_name(parameter)


def test_via_value_of_does_not_propagate():
    return meta("Name")


def tito(parameter, other):
    ...


def test_tito():
    a = tito(__test_source(), "second")
    return a


def sink_via_value_of(x, y):
    ...


def test_sink(element):
    return sink_via_value_of(element, "second")


def test_backwards_tito(parameter):
    return tito(parameter, "by_backwards")


def meta_sink(parameter, value):
    sink_via_value_of(parameter, value)


def test_sinks_do_not_propagate(parameter):
    meta_sink(parameter, "not a feature")


def attach_to_source(parameter):
    return __test_source()


def test_attach_to_source():
    return attach_to_source("attached to source")


def attach_to_sink(parameter, feature):
    __test_sink(parameter)


def test_attach_to_sink(parameter):
    attach_to_sink(parameter, "attached to sink")
