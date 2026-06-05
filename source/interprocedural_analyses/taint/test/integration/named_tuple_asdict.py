# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import NamedTuple, Optional

from pysa import _test_sink


class ConnectionParams(NamedTuple):
    user: str
    host: str
    charset: Optional[str] = None


# We expect Pysa to infer a sink on connection_params.charset (formal parameter port).
def sink_on_charset_via_asdict_pop(connection_params: ConnectionParams) -> None:
    conn_params = connection_params._asdict()
    charset = conn_params.pop("charset", None)
    _test_sink(charset)


# Baseline: direct field access. Should clearly infer a sink on connection_params.charset.
def sink_on_charset_direct(connection_params: ConnectionParams) -> None:
    _test_sink(connection_params.charset)


# Isolation: _asdict() then subscript (no .pop). Helps tell whether taint is lost in
# _asdict() or in dict.pop.
def sink_on_charset_via_asdict_getitem(connection_params: ConnectionParams) -> None:
    conn_params = connection_params._asdict()
    _test_sink(conn_params["charset"])


# Isolation: _asdict() then .pop, whole dict to sink (no key narrowing).
def sink_on_asdict_pop_value(connection_params: ConnectionParams) -> None:
    conn_params = connection_params._asdict()
    _test_sink(conn_params.pop("charset", None))
