# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink
from typing import Any, MutableMapping
from typing_extensions import Self
from pyre_extensions import ReadOnly


class A:
    def __init__(self) -> None:
        self.B: str = ""
        self.mapping: MutableMapping[str, Any] = {}

    def self_readonly_str(
        self: ReadOnly[Self]
    ) -> None:
        _test_sink(self.B)

    def self_untyped_str(
        self
    ) -> None:
        _test_sink(self.B)

    def self_readonly_map(
        self: ReadOnly[Self]
    ) -> None:
        # pyre-ignore[3005]: Ignore ReadOnly Violation
        _test_sink(self.mapping.get(""))

    def self_untyped_map(
        self
    ) -> None:
        _test_sink(self.mapping.get(""))
