# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa
import functools
import multiprocessing
import typing
from builtins import _test_sink, _test_source


class MyClass:
    def __init__(self, foo: str = "", bar: str = "") -> None:
        self.foo: str = foo
        self.bar: str = bar

    def sink_on_foo(self, a=None) -> None:
        _test_sink(self.foo)

    def set_foo(self, value: str) -> None:
        self.foo = value


def sink_on_foo(x: MyClass) -> None:
    _test_sink(x.foo)


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


def partial_application_bound_method_sink(x: MyClass):
    functools.partial(x.sink_on_foo, 0)  # TODO(T226554045): Wrong port for sink


def partial_application_bound_method_tito(x: MyClass):
    functools.partial(x.set_foo, _test_source())
    return x


def multiprocessing_tainted():
    multiprocessing.Process(target=a_flows_to_sink, args=(_test_source(), 1))


def multiprocessing_not_tainted():
    multiprocessing.Process(target=a_flows_to_sink, args=(1, _test_source()))


def multiprocessing_infer_sinks(x):
    multiprocessing.Process(target=a_flows_to_sink, args=(x, 0))


def multiprocessing_bound_method_issue(x: MyClass):
    x.foo = _test_source()
    multiprocessing.Process(target=x.sink_on_foo, args=())
    # TODO(T226554045): False negative due to using the taint of `x.sink_on_foo`
    # instead of `x` in the shim.


def multiprocessing_bound_method_sink(x: MyClass):
    multiprocessing.Process(target=x.sink_on_foo, args=())
    # TODO(T226554045): Wrong port for sink.


def multiprocessing_shim_fail(x: MyClass):
    args = (_test_source(), 1)
    multiprocessing.Process(target=a_flows_to_sink, args=args)
    # Shimming fails because `args` is not inlined in the call,
    # but we still find an issue thanks to the higher order parameter heuristic.


def multiprocessing_nested_sink(x):
    d = {"a": MyClass(foo=x)}
    multiprocessing.Process(target=sink_on_foo, args=(d["a"]))


def multiprocessing_no_sink(x):
    d = {"a": MyClass(bar=x)}
    multiprocessing.Process(target=sink_on_foo, args=(d["a"]))


def multiprocessing_tainted_access_path():
    multiprocessing.Process(target=sink_on_foo, args=(MyClass(foo=_test_source()), 1))  # Issue.
    multiprocessing.Process(target=sink_on_foo, args=(MyClass(bar=_test_source()), 1))  # No issue.


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
