# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import Dict, Optional, Union


def tito(x):
    return x


def sink_a(d):
    _test_sink(d["a"])


def tito_collapse_issue():
    # issue because of collapsing when applying tito
    a = {"a": _test_source(), "b": "b"}
    b = tito(a)
    _test_sink(b["b"])


def tito_collapse_sink(a):
    b = tito(a)
    _test_sink(b["b"])


def tito_collapse_source():
    a = {"a": _test_source(), "b": "b"}
    return tito(a)


def issue_collapse():
    # issue because of source-sink match collapsing
    a = {"a": _test_source(), "b": "b"}
    _test_sink(a)


def simplify_collapse_source(c):
    d = {}
    if c:
        d["1.1"] = _test_source()
        d["1.2"] = _test_source()
        d["1.3"] = _test_source()
        d["1.4"] = _test_source()
        d["1.5"] = _test_source()
        d["1.6"] = _test_source()
        d["1.7"] = _test_source()
        d["1.8"] = _test_source()
        d["1.9"] = _test_source()
        d["1.10"] = _test_source()
        d["1.11"] = _test_source()
        d["1.12"] = _test_source()
        d["1.13"] = _test_source()
        d["1.14"] = _test_source()
        d["1.15"] = _test_source()
    else:
        d["2.1"] = _test_source()
        d["2.2"] = _test_source()
        d["2.3"] = _test_source()
        d["2.4"] = _test_source()
        d["2.5"] = _test_source()
        d["2.6"] = _test_source()
        d["2.7"] = _test_source()
        d["2.8"] = _test_source()
        d["2.9"] = _test_source()
        d["2.10"] = _test_source()
        d["2.11"] = _test_source()
        d["2.12"] = _test_source()
        d["2.13"] = _test_source()
        d["2.14"] = _test_source()
        d["2.15"] = _test_source()
    # collapsed into a single source for `d` during model simplification.
    return d


def widen_collapse_source(c):
    d = {}
    if c:
        d["a"]["a"]["a"]["a"]["1"] = _test_source()
    else:
        d["a"]["a"]["a"]["a"]["2"] = _test_source()
    # collapsed into a source for `d["a"]["a"]["a"]["a"]` during widening.
    return d


def widen_collapse_sink(c, d):
    # collapsed into a sink for `d["a"]["a"]["a"]["a"]` during widening.
    if c:
        _test_sink(d["a"]["a"]["a"]["a"]["1"])
    else:
        _test_sink(d["a"]["a"]["a"]["a"]["2"])


def convert_2_update_dict_widening(source) -> Dict[str, Union[str, Optional[int]]]:
    kvs: Dict[str, Union[str, Optional[int]]] = {}
    kvs["1"] = source.f1
    kvs["2"] = source.f2
    kvs["3"] = source.f3
    kvs["4"] = source.f4
    kvs["5"] = source.f5
    kvs["6"] = source.f6
    # TODO(T116671305): Widening causes the entire dictionary to be tainted,
    # as opposed to only the values' being tainted. By contrast, fewer
    # entries in the dictionary do not seem to trigger broadening (see below).
    return kvs


def convert_2_update_dict_no_widening(source) -> Dict[str, Union[str, Optional[int]]]:
    kvs: Dict[str, Union[str, Optional[int]]] = {}
    kvs["1"] = source.f1
    kvs["2"] = source.f2
    kvs["3"] = source.f3
    kvs["4"] = source.f4
    kvs["5"] = source.f5
    return kvs


def untainted_keys():
    source = _test_source()
    kvs = convert_2_update_dict_widening(source)
    _test_sink(
        f"""
            {", ".join(kvs.keys())}  # False positive here
        """
    )
    kvs2 = convert_2_update_dict_no_widening(source)
    _test_sink(
        f"""
            {", ".join(kvs2.keys())}  # No issue here
        """
    )
