# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def test1_d1(f, *args, **kwargs):
    pass


def test1_d2(f, *args, **kwargs):
    pass


def test2_d1(f, *args, **kwargs):
    pass


def test2_d2(f, *args, **kwargs):
    pass


def test3_d1(f, *args, **kwargs):
    pass


def test4_d1(f, *args, **kwargs):
    pass


def test5_d1(f, *args, **kwargs):
    pass


@test1_d1
def test1_alarm1():
    return None


@test1_d2
def test1_noalarm1():
    return None


@test2_d1
def test2_alarm1():
    return None


@test2_d2
def test2_noalarm1():
    return None


arg1 = None


@test3_d1(arg1, 2, arg3="Foo", arg4="Bar")
def test3_alarm1():
    return None


@test3_d1(arg1, 2, "bar", arg3="Foo")
def test3_alarm2():
    return None


@test3_d1(arg1, 2, arg3="Foo")
def test3_alarm3():
    return None


@test3_d1()
def test3_noalarm1():
    return None


@test3_d1(arg3="Foo")
def test3_noalarm2():
    return None


@test4_d1(arg1, 2, arg3="Foo")
def test4_alarm1():
    return None


@test4_d1(arg1, 2, arg3="Foo", arg4="Bar")
def test4_noalarm1():
    return None


@test4_d1()
def test4_noalarm2():
    return None


@test4_d1(arg3="Foo")
def test4_noalarm3():
    return None


@test5_d1(arg1, 2, arg3="Foo")
def test5_alarm1():
    return None


@test5_d1(arg1, 2, 3, 4, arg4="Bar", arg3="Foo")
def test5_alarm2():
    return None


@test5_d1(2, arg1, arg3="Foo")
def test5_noalarm1():
    return None


@test5_d1(arg1, 3, 2, 4, arg3="Foo")
def test5_noalarm2():
    return None
