# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from dataclasses import dataclass


class TitoAttributes:
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z


def test_tito_attribute_x():
    c = TitoAttributes(**_test_source())
    _test_sink(c.x)


def test_tito_attribute_y():
    c = TitoAttributes(**_test_source())
    _test_sink(c.y)


def test_tito_attribute_z_with_tag():
    c = TitoAttributes(**_test_source())
    _test_sink(c.z)


def test_tito_attribute_join():
    c = TitoAttributes(**_test_source())
    foo = c.x
    if 1:
        foo = c.y
    elif 2:
        foo = c.z
    _test_sink(foo)


@dataclass
class SourceAttributes:
    x: str = ""
    y: str = ""
    z: str = ""


def test_source_attribute_x(c: SourceAttributes):
    _test_sink(c.x)


def test_source_attribute_y(c: SourceAttributes):
    _test_sink(c.y)


def test_source_attribute_z(c: SourceAttributes):
    _test_sink(c.z)


def test_source_attribute_join(c: SourceAttributes):
    foo = c.x
    if 1:
        foo = c.y
    elif 2:
        foo = c.z
    _test_sink(foo)


@dataclass
class SinkAttributes:
    x: str = ""
    y: str = ""
    z: str = ""


def test_sink_attribute_x(c: SinkAttributes):
    c.x = _test_source()


def test_sink_attribute_y(c: SinkAttributes):
    c.y = _test_source()


def test_sink_attribute_z(c: SinkAttributes):
    c.z = _test_source()


@dataclass
class TitoAttributeModelQuery:
    x: str = ""
    y: str = ""
    z: str = ""


def test_tito_attribute_model_query_x():
    _test_sink(TitoAttributeModelQuery(x=_test_source(), y="", z=""))


def test_tito_attribute_model_query_y():
    _test_sink(TitoAttributeModelQuery(x="", y=_test_source(), z=""))


def test_tito_attribute_model_query_z():
    _test_sink(TitoAttributeModelQuery(x="", y="", z=_test_source()))
