# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import enum
from builtins import _test_sink, _test_source
from typing import Annotated, Any, Dict, List


class Test1_C:
    x: int = 0
    y: str = "y"
    z: Annotated[str, "test1"] = "z"


def test1_alarm1():
    # always-via-type:int
    c = Test1_C(_test_source())
    _test_sink(c.x)


def test1_alarm2():
    # always-via-type:str
    c = Test1_C(_test_source())
    _test_sink(c.y)


def test1_alarm3():
    # always-via-type:typing.Annotated[str]
    c = Test1_C(_test_source())
    _test_sink(c.z)


def test1_alarm4(foo):
    # via-type:int, via-type:str, via-type:typing.Annotated[str]
    c = Test1_C(_test_source())
    foo = c.x
    if 1:
        foo = c.y
    elif 2:
        foo = c.z
    _test_sink(foo)


class Test2_C:
    x: Dict[str, int] = {}
    y: List[str] = []
    z: Annotated[float, "test2"] = 0.0


def test2_alarm1():
    # always-via-type:Dict[str, int]
    c = Test2_C(_test_source())
    _test_sink(c.x)


def test2_alarm2():
    # always-via-type:List[str]
    c = Test2_C(_test_source())
    _test_sink(c.y)


def test2_alarm3():
    # always-via-type:float
    c = Test2_C(_test_source())
    _test_sink(c.z)


def test2_alarm4(foo):
    # via-type:Dict[str, int], via-type:List[str], via-type:float
    c = Test2_C(_test_source())
    foo = c.x
    if 1:
        foo = c.y
    elif 2:
        foo = c.z
    _test_sink(foo)


class Test3_Foo:
    ...


class Test3_C:
    x: Dict[str, List[int]] = {}
    y: Test3_Foo = Test3_Foo()
    z: Annotated[List[List[str]], "test3"] = []


def test3_alarm1(c: Test3_C):
    # always-via-type:Dict[str, List[int]]
    _test_sink(c.x)


def test3_alarm2(c: Test3_C):
    # always-via-type:Test3_Foo
    _test_sink(c.y)


def test3_alarm3(c: Test3_C):
    # always-via-type:typing.Annotated[List[List[str]]
    _test_sink(c.z)


def test3_alarm4(c: Test3_C, foo):
    # via-type:Dict[str, List[int]],
    # via-type:Test3_Foo,
    # via-type:typing.Annotated[List[List[str]]
    foo = c.x
    if 1:
        foo = c.y
    elif 2:
        foo = c.z
    _test_sink(foo)


class Test4_C:
    x = ...
    y: Any = 0
    z: object = []


def test4_alarm1(c: Test4_C):
    # always-via-type:unknown
    c.x = _test_source()


def test4_alarm2(c: Test4_C):
    # always-via-type:Any
    c.y = _test_source()


def test4_alarm3(c: Test4_C):
    # always-via-type:object
    c.z = _test_source()


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
