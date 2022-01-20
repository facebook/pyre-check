# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import typing
from builtins import _test_sink, _test_source


def test_del_keyword():
    val = _test_source()
    my_dict = {"key": val}
    del my_dict["key"]
    _test_sink(my_dict["key"])


def return_dict_with_bad_key():
    val = _test_source()
    my_dict = {"key": val}
    del my_dict["key"]
    return my_dict


def take_dict_with_bad_key(my_dict: typing.Dict[str, str]):
    del my_dict["key"]
    return my_dict["key"]


def pop_dict_with_bad_key(my_dict: typing.Dict[str, str]):
    my_dict.pop("key")
    return my_dict["key"]


def pop_key(my_dict: typing.Dict[str, str]):
    return my_dict.pop("key")


def dict_into_sink(my_dict: typing.Dict[str, str]):
    del my_dict["key"]
    _test_sink(my_dict["key"])


def test_pop_method():
    val = _test_source()
    my_dict = {"key": val}
    my_dict.pop("key")
    _test_sink(my_dict["key"])
