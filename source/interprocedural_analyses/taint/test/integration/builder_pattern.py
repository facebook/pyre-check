# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import Optional, TypeVar


T = TypeVar("T")


class Builder:
    def __init__(self) -> None:
        self._saved: Optional[str] = None
        self._not_saved: Optional[str] = None

    def set_saved(self, saved: str) -> "Builder":
        self._saved = saved
        return self

    def set_not_saved(self, not_saved: str) -> "Builder":
        self._not_saved = not_saved
        return self

    def async_save(self) -> None:
        _test_sink(self._saved)

    def set_saved_through_typevar(self: T, saved: str) -> T:
        self._saved = saved
        return self

    def set_not_saved_through_typevar(self: T, not_saved: str) -> T:
        self._not_saved = not_saved
        return self


def test_no_issue():
    builder = Builder()
    builder.set_not_saved(_test_source()).set_saved("benign").async_save()


def test_issue():
    builder = Builder()
    builder.set_not_saved("benign").set_saved(_test_source()).async_save()


def test_no_issue_with_type_var():
    builder = Builder()
    builder.set_not_saved_through_typevar(_test_source()).set_saved_through_typevar(
        "benign"
    ).async_save()


def test_issue_with_type_var():
    builder = Builder()
    builder.set_not_saved_through_typevar("benign").set_saved_through_typevar(
        _test_source()
    ).async_save()


class SubBuilder(Builder):
    pass


def test_no_issue_with_sub_builder():
    builder = SubBuilder()
    builder.set_not_saved_through_typevar(_test_source()).set_saved_through_typevar(
        "benign"
    ).async_save()


def test_issue_with_sub_builder():
    builder = SubBuilder()
    builder.set_not_saved_through_typevar("benign").set_saved_through_typevar(
        _test_source()
    ).async_save()
