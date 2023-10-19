# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

# Integration test for cache invalidation, see `run_cache_test.py`.

from integration_test.taint import source, sink


def ignore_decorator(f):
    return f


def test_ignore_decorator():
    @ignore_decorator
    def decorated_sink(x):
        sink(x)

    # This issue is found when using `@IgnoreDecorator` on `ignore_decorator`.
    decorated_sink(source())


class Base:
    def method(self, x):
        pass


class Override(Base):
    def method(self, x):
        sink(x)


def test_skip_overrides(instance: Base):
    instance.method(source())


class AnotherBase:
    def method(self, x):
        pass


class AnotherOverride(AnotherBase):
    def method(self, x):
        sink(x)


def test_overrides_cap(instance: AnotherBase):
    instance.method(source())


def test_skip_analysis():
    sink(source())


class Token:
    token: str = ""


def test_attribute(token: Token) -> None:
    sink(token.token)


class YetAnotherBase:
    def method(self, x):
        pass


class YetAnotherOverride1(YetAnotherBase):
    def method(self, x):
        sink(x)


class YetAnotherOverride2(YetAnotherBase):
    def method(self, x):
        pass


class YetAnotherOverride3(YetAnotherBase):
    def method(self, x):
        pass


def test_analyze_all_overrides(b: YetAnotherBase):
    b.method(source())
