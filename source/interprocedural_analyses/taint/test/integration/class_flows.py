# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from contextlib import contextmanager
from typing import List, Optional, Type


class C:
    tainted_attribute: List[int] = []
    tainted_class_attribute: List[int] = []
    not_tainted = 2


class D(C):
    pass


def tainted_attribute_flow_issue(c: C) -> None:
    c.tainted_attribute = _test_source()


def untainted_flow_not_issue(c: C) -> None:
    c.not_tainted = _test_source()


def tainted_attribute_for_class_not_issue(c: Type[C]) -> None:
    c.tainted_attribute = _test_source()


def tainted_attribute_through_inheritance_issue(d: D) -> None:
    d.tainted_attribute = _test_source()


def tainted_class_attribute_through_instance_not_issue(c: C) -> None:
    c.tainted_class_attribute = _test_source()


def tainted_class_attribute_through_class_issue(class_object: Type[C]) -> None:
    class_object.tainted_class_attribute = _test_source()


def tainted_class_attribute_through_double_underscore_class_issue(c: C) -> None:
    c.__class__.tainted_class_attribute = _test_source()


def tainted_class_attribute_through_optional_class_issue(
    class_object: Optional[Type[C]],
) -> None:
    if class_object is not None:
        class_object.tainted_class_attribute = _test_source()


def global_class_attribute_issue() -> None:
    C.tainted_class_attribute = _test_source()


class HasClassmethods:
    @classmethod
    def _async_results_for_non_empty_query_from_db(cls, locale: str):
        if not locale:
            emojis = cls._get_single_word_results(locale)
        else:
            emojis = cls._get_multi_word_results(locale)

    @classmethod
    def _get_multi_word_results(cls, locale: str):
        _test_sink(locale)
        return ""

    @classmethod
    def _get_single_word_results(cls, locale: str):
        return ""


def test():
    HasClassmethods._async_results_for_non_empty_query_from_db(_test_source())


class HasDecoratedClassmethod:
    @classmethod
    @contextmanager
    def to_sink(self, x):
        _test_sink(x)


def test_decorated_classmethod():
    HasDecoratedClassmethod.to_sink(_test_source())
