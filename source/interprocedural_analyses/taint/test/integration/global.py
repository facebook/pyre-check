# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


class Object:
    pass


obj1 = Object()


def obj1_source():
    obj1.x = _test_source()


def obj1_sink():
    _test_sink(obj1.x)


def obj1_flow():
    # TODO(T145247918): False negative
    obj1_source()
    obj1_sink()


def obj1_no_flow():
    obj1_sink()
    obj1_source()


obj2 = Object()


def obj2_sink():
    _test_sink(obj2)


obj3 = Object()


def obj3_return():
    return obj3


def obj3_set(x):
    obj3.x = x


def obj3_flow():
    # TODO(T145247918): False negative
    obj3_set(_test_source())
    y = obj3_return()
    _test_sink(y.x)


obj4 = _test_source()


def obj4_flow():
    # TODO(T145247918): False negative
    _test_sink(obj4)


def create_global_source():
    global z
    z = _test_source()


create_global_source()


def return_global_source():
    # TODO(T123109154): We should see a model here, because
    # global variable z is a source
    return z


obj6 = Object()


def obj6_source():
    global obj6
    obj6 = _test_source()


def obj6_sink():
    _test_sink(obj6)


def obj6_flow():
    # TODO(T145247918): False negative
    obj6_source()
    obj6_sink()


def obj7_source():
    # This created a new variable obj7 in the global frame
    global obj7
    obj7 = _test_source()


def obj7_sink():
    _test_sink(obj7)


def obj7_flow():
    # TODO(T145247918): False negative
    obj7_source()
    obj7_sink()


obj8 = Object()


def obj8_return():
    return obj8


def obj8_set(x):
    global obj8
    obj8 = x


def obj8_flow():
    # TODO(T145247918): False negative
    obj8_set(_test_source())
    y = obj8_return()
    _test_sink(y)
