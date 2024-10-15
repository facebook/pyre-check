# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module contains annotations used in integration tests to
define where we expect to find issues.
"""

from typing import Optional, TypeVar

T = TypeVar("T")


def generic_test_source() -> None:
    pass


def generic_test_sink(param: str) -> None:
    pass


class ExpectIssue:
    def __new__(
        cls,
        *,
        code: int,
        line: Optional[int] = None,
        task: Optional[str] = None,
        currently_found: bool = True,
    ) -> "ExpectIssue":
        return super().__new__(cls)

    def __call__(self, f: T) -> T:
        return f


class ExpectNoIssue:
    def __new__(
        cls,
        *,
        code: int,
        line: Optional[int] = None,
        task: Optional[str] = None,
        currently_found: bool = False,
    ) -> "ExpectNoIssue":
        return super().__new__(cls)

    def __call__(self, f: T) -> T:
        return f
