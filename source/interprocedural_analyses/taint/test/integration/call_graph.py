# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import Mapping, TypeVar, Callable, Union
from typing_extensions import TypeGuard

# Demonstrate a (currently fixed) false positive due to type resolution.

class IsSource:
    def __init__(self) -> None:
        pass

    def method(self) -> str:
        return _test_source()


class NotSource:
    def __init__(self) -> None:
        pass

    def method(self) -> IsSource:
        pass


def test_chained_assign():
    x = NotSource()
    x = y = x.method()
    # Treating it as x = x.method(); x = y would lead to a false positive.
    _test_sink(x)  # No source
    _test_sink(y)  # No source


def test_default_parameters(x: str = _test_source() + "") -> None:
    _test_sink(x)


class LogRecord:
    args: tuple[object, ...] | Mapping[str, object] | None = None

    def __init__(self) -> None: ...


def is_dict(obj: object) -> TypeGuard[dict[object, object]]:
    return isinstance(obj, dict)


def test_chained_assign_subscript(record: LogRecord):
    if is_dict(record.args) and "headers" in record.args and is_dict(record.args["headers"]):
        headers = record.args["headers"] = {**record.args["headers"]} # pyre-ignore
        # Treated as:
        # ```
        # headers = {**record.args["headers"]}
        # record.args["headers"] = {**record.args["headers"]}
        # ```
        # This is a corner case where the type resolution leads to different
        # callees in the right hand side expression


def test_localized_target():
    if 1 < 2:
        f = lambda: None
    else:
        def f() -> None:
            return

    f()


T = TypeVar("T")


def no_op_decorator_factory(x: int) -> Callable[[T], T]: # pyre-ignore
    def inner(original: T) -> T:
        setattr(original, "foo", "bar")
        return original

    return inner


def no_op_decorator(f: T) -> T:
    return f


class CallableKindConfusion:
    @no_op_decorator
    @no_op_decorator_factory(1)
    def foo(self) -> None:
        # In previous versions of Pysa, the Call graph for
        # CallableKindConfusion.foo@decorated would contain a call to
        # `CallableKindConfusion.foo (fun)` instead of
        # `CallableKindConfusion.foo (method)`. This is now fixed.
        return


class namespace:
    class A:
        def __init__(self, x): pass
        def __call__(self): pass

    class B:
        def __init__(self, x): pass
        def __call__(self): pass

    class C:
        def __init__(self, x): pass
        def __call__(self): pass


def test_match_type_of(x: Union[namespace.A, namespace.B, namespace.C]):
    # The `match` is currently translated into:
    # ```
    # if type(x) == A: ..
    # elif type(x) == B: ...
    # elif type(x) == C: ...
    # ```
    # After each implicit `else`, we learn that the type is not A, or B, or C
    # Hence each time we compute the call graph for `type(x)`, we add different
    # higher order parameters (first one has all A,B,C, second one has B,C, etc.)
    # This leads to having different callees for the same original expression,
    # breaking a call graph invariant.
    match type(x):
        case namespace.A:
            _test_sink(x)
        case namespace.B:
            _test_sink(x)
        case namespace.C:
            _test_sink(x)
