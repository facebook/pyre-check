# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import Dict


def sink_on_0(x: Dict) -> None:
    _test_sink(x["0"])


def sink_on_0_and_star(x: Dict, i: int) -> None:
    _test_sink(x["0"])
    _test_sink(x[i])


def issue_source_on_0_and_star_to_sink_on_0_and_star(i: int) -> None:
    x = {}
    x[i] = _test_source()
    x["0"] = _test_source()
    sink_on_0_and_star(x, i)


def issue_source_on_0_to_sink_on_0_and_star(i: int) -> None:
    x = {}
    x["0"] = _test_source()
    sink_on_0_and_star(x, i)


def issue_source_on_0_and_star_to_sink_on_0(i: int) -> None:
    x = {}
    x[i] = _test_source()
    x["0"] = _test_source()
    sink_on_0(x)


def issue_source_on_0_to_sink_on_0() -> None:
    x = {}
    x["0"] = _test_source()
    sink_on_0(x)


def issue_source_on_1_to_sink_on_0_and_star(i: int) -> None:
    x = {}
    x["1"] = _test_source()
    sink_on_0_and_star(x, i)


def no_issue_source_on_1_to_sink_on_0() -> None:
    x = {}
    x["1"] = _test_source()
    sink_on_0(x)


def issue_source_on_star_to_sink_on_0_and_star(i: int) -> None:
    x = {}
    x[i] = _test_source()
    sink_on_0_and_star(x, i)


def issue_source_on_star_to_sink_on_0(i: int) -> None:
    x = {}
    x[i] = _test_source()
    sink_on_0(x)
