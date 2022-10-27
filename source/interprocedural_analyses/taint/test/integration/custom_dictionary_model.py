# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def _test_source_2():
    return "I'M TAINTED"


def untainted_dictionary():
    d = {}
    d["a"] = "I'm not tainted!"
    _test_sink(d)


def sink_dictionary_value():
    d = {}
    d["a"] = _test_source_2()


def sink_dictionary_key():
    d = {}
    d[_test_source_2()] = "b"


def tainted_dictionary_value_sink():
    d = {}
    d["a"] = _test_source()
    _test_sink(d)


def tainted_dictionary_key_sink():
    d = {}
    d[_test_source()] = 1
    _test_sink(d)
