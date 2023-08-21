# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import Dict, Optional, Union, TypeVar


def tito(x):
    ...  # Type stubs are treated as taint-in-taint-out with collapse depth 0.


def sink_a(parameter):
    _test_sink(parameter["a"])


def tito_collapse_issue():
    # issue because of collapsing when applying tito
    a = {"a": _test_source(), "b": "b"}
    b = tito(a)
    _test_sink(b["b"])


def tito_collapse_sink(parameter):
    b = tito(parameter)
    _test_sink(b["b"])


def tito_collapse_source():
    a = {"a": _test_source(), "b": "b"}
    return tito(a)


def issue_collapse():
    # issue because of source-sink match collapsing
    a = {"a": _test_source(), "b": "b"}
    _test_sink(a)


def model_broadening_collapse_source_width(c):
    result = {}
    if c:
        result["o1.1"] = _test_source()
        result["o1.2"] = _test_source()
        result["o1.3"] = _test_source()
        result["o1.4"] = _test_source()
        result["o1.5"] = _test_source()
        result["o1.6"] = _test_source()
        result["o1.7"] = _test_source()
        result["o1.8"] = _test_source()
        result["o1.9"] = _test_source()
        result["o1.10"] = _test_source()
        result["o1.11"] = _test_source()
        result["o1.12"] = _test_source()
        result["o1.13"] = _test_source()
        result["o1.14"] = _test_source()
        result["o1.15"] = _test_source()
    else:
        result["o2.1"] = _test_source()
        result["o2.2"] = _test_source()
        result["o2.3"] = _test_source()
        result["o2.4"] = _test_source()
        result["o2.5"] = _test_source()
        result["o2.6"] = _test_source()
        result["o2.7"] = _test_source()
        result["o2.8"] = _test_source()
        result["o2.9"] = _test_source()
        result["o2.10"] = _test_source()
        result["o2.11"] = _test_source()
        result["o2.12"] = _test_source()
        result["o2.13"] = _test_source()
        result["o2.14"] = _test_source()
        result["o2.15"] = _test_source()
    # collapsed into a single source for `result` during model broadening.
    return result


def model_broadening_collapse_sink_width(parameter, condition):
    # collapsed into a single sink on `parameter` during model broadening.
    if condition:
        _test_sink(parameter["i1.1"])
        _test_sink(parameter["i1.2"])
        _test_sink(parameter["i1.3"])
        _test_sink(parameter["i1.4"])
        _test_sink(parameter["i1.5"])
        _test_sink(parameter["i1.6"])
        _test_sink(parameter["i1.7"])
        _test_sink(parameter["i1.8"])
        _test_sink(parameter["i1.9"])
        _test_sink(parameter["i1.10"])
        _test_sink(parameter["i1.11"])
        _test_sink(parameter["i1.12"])
        _test_sink(parameter["i1.13"])
        _test_sink(parameter["i1.14"])
        _test_sink(parameter["i1.15"])
    else:
        _test_sink(parameter["i2.1"])
        _test_sink(parameter["i2.2"])
        _test_sink(parameter["i2.3"])
        _test_sink(parameter["i2.4"])
        _test_sink(parameter["i2.5"])
        _test_sink(parameter["i2.6"])
        _test_sink(parameter["i2.7"])
        _test_sink(parameter["i2.8"])
        _test_sink(parameter["i2.9"])
        _test_sink(parameter["i2.10"])
        _test_sink(parameter["i2.11"])
        _test_sink(parameter["i2.12"])
        _test_sink(parameter["i2.13"])
        _test_sink(parameter["i2.14"])
        _test_sink(parameter["i2.15"])


def model_broadening_source_no_collapse_depth(condition):
    result = {}
    if condition:
        result["a"]["a"]["a"]["a"]["1"] = _test_source()
    else:
        result["a"]["a"]["a"]["a"]["2"] = _test_source()
    # no collapsing here
    # collapsing by depth is only applied during widening, within a loop.
    return result


def source_taint_widening_collapse_depth():
    result = {}
    for _ in range(1000000):
        result = {"a": result, "b": _test_source()}
        # collapsed into a source on result[a][a][a][a] during widening.
    return result


def model_broadening_sink_no_collapse_depth(condition, parameter):
    # no collapsing here
    # collapsing by depth is only applied during widening, within a loop.
    if condition:
        _test_sink(parameter["a"]["a"]["a"]["a"]["1"])
    else:
        _test_sink(parameter["a"]["a"]["a"]["a"]["2"])


def sink_taint_widening_collapse_depth(parameter):
    for _ in range(1000000):
        _test_sink(parameter["b"])
        parameter = parameter["a"]
        # collapsed into a sink on parameter[a][a][a][a] during widening.


def recursive_sink_parent(obj):
    if obj.parent is not None:
        recursive_sink_parent(obj.parent)
    else:
        _test_sink(obj)


def recursive_sink_parent_attribute(obj):
    if obj.parent is not None:
        recursive_sink_parent_attribute(obj.parent)
    else:
        _test_sink(obj.attribute)


def tito_broaden_input_and_output_paths(
    parameter,
) -> Dict[str, Union[str, Optional[int]]]:
    result: Dict[str, Union[str, Optional[int]]] = {}
    result["o1"] = parameter.i1
    result["o2"] = parameter.i2
    result["o3"] = parameter.i3
    result["o4"] = parameter.i4
    result["o5"] = parameter.i5
    result["o6"] = parameter.i6
    result["o7"] = parameter.i7
    result["o8"] = parameter.i8
    result["o9"] = parameter.i9
    result["o10"] = parameter.i10
    result["o11"] = parameter.i11
    # Model broadening leads to taint collapsing here.
    # We infer a tito from `parameter` to `result`.
    return result


def tito_broaden_input_paths_but_not_output_path(
    parameter,
) -> Dict[str, Union[str, Optional[int]]]:
    result: Dict[str, Union[str, Optional[int]]] = {}
    result["o1"] = parameter.i1
    result["o2"] = parameter.i2
    result["o3"] = parameter.i3
    result["o4"] = parameter.i4
    result["o5"] = parameter.i5
    result["o6"] = parameter.i6
    result["o7"] = parameter.i7
    result["o8"] = parameter.i8
    result["o9"] = parameter.i9
    result["o10"] = parameter.i10
    # We are below the model broadening threshold for output paths.
    # However, we are above the threshold for input paths.
    # We infer TITO from parameter to result[o1], result[o2], etc.
    return result


def random_tito(parameter, condition):
    if condition == 0:
        return parameter.i1
    elif condition == 1:
        return parameter.i2
    elif condition == 2:
        return parameter.i3
    else:
        return parameter.i4


def tito_broaden_output_paths_but_not_input_path(
    parameter, condition
) -> Dict[str, Union[str, Optional[int]]]:
    result: Dict[str, Union[str, Optional[int]]] = {}
    result["o1"] = random_tito(parameter, condition)
    result["o2"] = random_tito(parameter, condition)
    result["o3"] = random_tito(parameter, condition)
    result["o4"] = random_tito(parameter, condition)
    result["o5"] = random_tito(parameter, condition)
    result["o6"] = random_tito(parameter, condition)
    result["o7"] = random_tito(parameter, condition)
    result["o8"] = random_tito(parameter, condition)
    result["o9"] = random_tito(parameter, condition)
    result["o10"] = random_tito(parameter, condition)
    result["o11"] = random_tito(parameter, condition)
    result["o12"] = random_tito(parameter, condition)
    result["o13"] = random_tito(parameter, condition)
    result["o14"] = random_tito(parameter, condition)
    result["o15"] = random_tito(parameter, condition)
    # We are below the model broadening threshold for input paths.
    # However, we are above the threshold for output paths.
    # We infer TITO from parameter[i1], parameter[i2], etc. to result
    return result


def test_different_tito_broadenings():
    source = _test_source()
    kvs = tito_broaden_input_and_output_paths(source)
    _test_sink(
        f"""
            {", ".join(kvs.keys())}  # False positive here
        """
    )
    kvs2 = tito_broaden_input_paths_but_not_output_path(source)
    _test_sink(
        f"""
            {", ".join(kvs2.keys())}  # No issue here
        """
    )


def tito_broaden_input_and_output_paths_single_statement(x):
    # Same as `tito_broaden_input_and_output_paths` but with a single statement.
    return {
        "a": x.a,
        "b": x.b,
        "c": x.c,
        "d": x.d,
        "e": x.e,
        "f": x.f,
        "g": x.g,
        "h": x.h,
        "j": x.j,
        "k": x.k,
        "l": x.l,
    }


def tito_broaden_input_path_common_prefix(x):
    # Input paths all have the same common prefix [y].
    # We infer TITO from x.y to result
    return {
        "a": x.y.a,
        "b": x.y.b,
        "c": x.y.c,
        "d": x.y.d,
        "e": x.y.e,
        "f": x.y.f,
        "g": x.y.g,
        "h": x.y.h,
        "j": x.y.j,
        "k": x.y.k,
        "l": x.y.l,
    }


def tito_broaden_output_path_common_prefix(x):
    # Output paths all have the same common prefix [a].
    # We infer TITO from x to result[a]
    return {
        "a": {
            "a": x.a,
            "b": x.b,
            "c": x.c,
            "d": x.d,
            "e": x.e,
            "f": x.f,
            "g": x.g,
            "h": x.h,
            "j": x.j,
            "k": x.k,
            "l": x.l,
        }
    }


# Test for the @SkipModelBroadening mode.

T = TypeVar("T")


# see taint_broadening.py.pysa
def skip_model_broadening(f: T) -> T:
    return f


@skip_model_broadening
def model_broadening_no_collapse_source_width(c):
    result = {}
    if c:
        result["o1.1"] = _test_source()
        result["o1.2"] = _test_source()
        result["o1.3"] = _test_source()
        result["o1.4"] = _test_source()
        result["o1.5"] = _test_source()
        result["o1.6"] = _test_source()
        result["o1.7"] = _test_source()
        result["o1.8"] = _test_source()
        result["o1.9"] = _test_source()
        result["o1.10"] = _test_source()
        result["o1.11"] = _test_source()
        result["o1.12"] = _test_source()
        result["o1.13"] = _test_source()
        result["o1.14"] = _test_source()
        result["o1.15"] = _test_source()
    else:
        result["o2.1"] = _test_source()
        result["o2.2"] = _test_source()
        result["o2.3"] = _test_source()
        result["o2.4"] = _test_source()
        result["o2.5"] = _test_source()
        result["o2.6"] = _test_source()
        result["o2.7"] = _test_source()
        result["o2.8"] = _test_source()
        result["o2.9"] = _test_source()
        result["o2.10"] = _test_source()
        result["o2.11"] = _test_source()
        result["o2.12"] = _test_source()
        result["o2.13"] = _test_source()
        result["o2.14"] = _test_source()
        result["o2.15"] = _test_source()
    return result


@skip_model_broadening
def model_broadening_no_collapse_sink_width(parameter, condition):
    if condition:
        _test_sink(parameter["i1.1"])
        _test_sink(parameter["i1.2"])
        _test_sink(parameter["i1.3"])
        _test_sink(parameter["i1.4"])
        _test_sink(parameter["i1.5"])
        _test_sink(parameter["i1.6"])
        _test_sink(parameter["i1.7"])
        _test_sink(parameter["i1.8"])
        _test_sink(parameter["i1.9"])
        _test_sink(parameter["i1.10"])
        _test_sink(parameter["i1.11"])
        _test_sink(parameter["i1.12"])
        _test_sink(parameter["i1.13"])
        _test_sink(parameter["i1.14"])
        _test_sink(parameter["i1.15"])
    else:
        _test_sink(parameter["i2.1"])
        _test_sink(parameter["i2.2"])
        _test_sink(parameter["i2.3"])
        _test_sink(parameter["i2.4"])
        _test_sink(parameter["i2.5"])
        _test_sink(parameter["i2.6"])
        _test_sink(parameter["i2.7"])
        _test_sink(parameter["i2.8"])
        _test_sink(parameter["i2.9"])
        _test_sink(parameter["i2.10"])
        _test_sink(parameter["i2.11"])
        _test_sink(parameter["i2.12"])
        _test_sink(parameter["i2.13"])
        _test_sink(parameter["i2.14"])
        _test_sink(parameter["i2.15"])


@skip_model_broadening
def tito_no_broadening_input_and_output_paths(
    parameter,
) -> Dict[str, Union[str, Optional[int]]]:
    result: Dict[str, Union[str, Optional[int]]] = {}
    result["o1"] = parameter.i1
    result["o2"] = parameter.i2
    result["o3"] = parameter.i3
    result["o4"] = parameter.i4
    result["o5"] = parameter.i5
    result["o6"] = parameter.i6
    result["o7"] = parameter.i7
    result["o8"] = parameter.i8
    result["o9"] = parameter.i9
    result["o10"] = parameter.i10
    result["o11"] = parameter.i11
    return result
