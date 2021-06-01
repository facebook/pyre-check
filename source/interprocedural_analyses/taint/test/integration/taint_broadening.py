# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import __test_sink, __test_source


def tito(x):
    return x


def sink_a(d):
    __test_sink(d["a"])


def tito_collapse_issue():
    # issue because of collapsing when applying tito
    a = {"a": __test_source(), "b": "b"}
    b = tito(a)
    __test_sink(b["b"])


def tito_collapse_sink(a):
    b = tito(a)
    __test_sink(b["b"])


def tito_collapse_source():
    a = {"a": __test_source(), "b": "b"}
    return tito(a)


def issue_collapse():
    # issue because of source-sink match collapsing
    a = {"a": __test_source(), "b": "b"}
    __test_sink(a)


def simplify_collapse_source(c):
    d = {}
    if c:
        d["1.1"] = __test_source()
        d["1.2"] = __test_source()
        d["1.3"] = __test_source()
        d["1.4"] = __test_source()
        d["1.5"] = __test_source()
        d["1.6"] = __test_source()
        d["1.7"] = __test_source()
        d["1.8"] = __test_source()
        d["1.9"] = __test_source()
        d["1.10"] = __test_source()
        d["1.11"] = __test_source()
        d["1.12"] = __test_source()
        d["1.13"] = __test_source()
        d["1.14"] = __test_source()
        d["1.15"] = __test_source()
    else:
        d["2.1"] = __test_source()
        d["2.2"] = __test_source()
        d["2.3"] = __test_source()
        d["2.4"] = __test_source()
        d["2.5"] = __test_source()
        d["2.6"] = __test_source()
        d["2.7"] = __test_source()
        d["2.8"] = __test_source()
        d["2.9"] = __test_source()
        d["2.10"] = __test_source()
        d["2.11"] = __test_source()
        d["2.12"] = __test_source()
        d["2.13"] = __test_source()
        d["2.14"] = __test_source()
        d["2.15"] = __test_source()
    # collapsed into a single source for `d` during model simplification.
    return d


def widen_collapse_source(c):
    d = {}
    if c:
        d["a"]["a"]["a"]["a"]["1"] = __test_source()
    else:
        d["a"]["a"]["a"]["a"]["2"] = __test_source()
    # collapsed into a source for `d["a"]["a"]["a"]["a"]` during widening.
    return d


def widen_collapse_sink(c, d):
    # collapsed into a sink for `d["a"]["a"]["a"]["a"]` during widening.
    if c:
        __test_sink(d["a"]["a"]["a"]["a"]["1"])
    else:
        __test_sink(d["a"]["a"]["a"]["a"]["2"])
