# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
import functools
import multiprocessing
import typing
from builtins import _test_sink, _test_source


def a_flows_to_sink(a, b):
    _test_sink(a)


def partial_application_with_tainted():
    x = _test_source()
    functools.partial(a_flows_to_sink, x)


def partial_application_with_benign():
    x = 1
    functools.partial(a_flows_to_sink, x)


def partial_application_with_named_a():
    x = _test_source()
    functools.partial(a_flows_to_sink, a=x)


def partial_application_with_named_b():
    x = _test_source()
    functools.partial(a_flows_to_sink, b=x)


def multiprocessing_tainted():
    multiprocessing.Process(target=a_flows_to_sink, args=(_test_source(), 1))


def multiprocessing_not_tainted():
    multiprocessing.Process(target=a_flows_to_sink, args=(1, _test_source()))


class PartialDecorator:
    def __init__(self, func: typing.Callable) -> None:
        self._func = func

    def __get__(self, instance, owner) -> functools.partial[None]:
        return functools.partial(self.__call__, instance=instance)

    def __call__(self, *args, **kwargs) -> None:
        instance = kwargs.pop("instance")
        return self._func(instance, *args, **kwargs)


class PartialConstructor:
    @PartialDecorator
    def __init__(self, x: str, y: str) -> None:
        self.x = x
        self.y = y


def dunder_call_partial_constructor(x: str, y: str) -> C:
    # pyre-ignore: Type[PartialConstructor] is not a function.
    return PartialConstructor(x, y)


class NestedDefineDecorator:
    def __init__(self, func: typing.Callable) -> None:
        self._func = func

    def __get__(self, instance, owner):
        def wrapper(*args, **kwargs):
            kwargs["instance"] = instance
            return self.__call__(*args, **kwargs)

        return wrapper

    def __call__(self, *args, **kwargs) -> None:
        instance = kwargs.pop("instance")
        return self._func(instance, *args, **kwargs)


class NestedDefineConstructor:
    @NestedDefineDecorator
    def __init__(self, x: str, y: str) -> None:
        self.x = x
        self.y = y


def dunder_call_nested_define_constructor(x: str, y: str) -> C:
    return NestedDefineConstructor(x, y)
