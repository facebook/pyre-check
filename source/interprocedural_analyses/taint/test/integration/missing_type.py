# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


class UnknownSourceDef:
    def source(self) -> None:
        pass

    unknown = source  # revealed type is `unknown`


def test_unknown_source_def(x: UnknownSourceDef) -> None:
    # TODO(T90322028): we don't find the flow here.
    y = x.unknown()
    _test_sink(y)


class UnknownSourceAttribute:
    def source(self) -> None:
        pass

    unknown = source  # revealed type is `unknown`


def test_unknown_source_attribute(x: UnknownSourceAttribute) -> None:
    y = x.unknown()
    _test_sink(y)


class UnknownSinkDef:
    def sink(self, x: str) -> None:
        pass

    unknown = sink  # revealed type is `unknown`


def test_unknown_sink_def(x: UnknownSinkDef) -> None:
    # TODO(T90322028): we don't find the flow here.
    x.unknown(_test_source())


class UnknownSinkAttribute:
    def sink(self, x: str) -> None:
        pass

    unknown = sink  # revealed type is `unknown`


def test_unknown_sink_attribute(x: UnknownSinkAttribute) -> None:
    # TODO(T90322028): we don't find the flow here.
    x.unknown(_test_source())
