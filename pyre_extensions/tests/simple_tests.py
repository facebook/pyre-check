# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-ignore-all-errors
import unittest
from typing import Any, Callable, Iterable, Iterator, List, TypeVar


class BasicTestCase(unittest.TestCase):
    def test_parameter_specification(self) -> None:
        try:
            from .. import ParameterSpecification

            TParams = ParameterSpecification("TParams")
            TReturn = TypeVar("T")

            def listify(
                f: Callable[TParams, TReturn]
            ) -> Callable[TParams, List[TReturn]]:
                def wrapped(*args: TParams.args, **kwargs: TParams.kwargs):
                    return [f(*args, **kwargs)]

                return wrapped

            def foo():
                return 9

            listify(foo)

        except Exception:
            self.fail("ParameterSpecification missing or broken")

    def test_list_variadics(self) -> None:
        try:
            from .. import ListVariadic
            from ..type_variable_operators import Map

            TReturn = TypeVar("T")
            Ts = ListVariadic("Ts")

            def better_map(
                func: Callable[[Ts], TReturn], *args: Map[Iterable, Ts]
            ) -> Iterator[TReturn]:
                return map(func, *args)

        except Exception:
            self.fail("ListVariadics missing or broken")

    def test_none_throws(self) -> None:
        try:
            from .. import none_throws

            none_throws(0)
            none_throws(0, "custom message")
        except Exception:
            self.fail("none_throws missing or broken")

    def test_safe_cast(self) -> None:
        try:
            from .. import safe_cast

            safe_cast(float, 1)
            safe_cast(1, float)
            safe_cast(Any, "string")
        except Exception:
            self.fail("safe_cast should not have runtime implications")

    def test_generic(self) -> None:
        try:
            from typing import TypeVar

            from .. import Generic, ListVariadic
            from ..type_variable_operators import Concatenate

            # permitted
            class Foo(Generic):
                pass

            # permitted
            class Bar(Generic[int]):
                pass

            # permitted
            class Baz(Generic[42]):
                pass

            Shape = ListVariadic("Shape")
            DType = TypeVar("DType")

            # intended use
            class Tensor(Generic[Concatenate[DType, Shape]]):
                pass

            # not handled in the backend ... yet
            X = Tensor[int, 7, 8, 9]  # noqa

        except Exception:
            self.fail("Generic/GenericMeta/Concatenate missing or broken")

    def test_variadic_tuple(self) -> None:
        try:
            from .. import TypeVarTuple, Unpack

            T = TypeVar("T")
            Ts = TypeVarTuple("Ts")

            def apply(f: Callable[[Unpack[Ts]], T], *args: Unpack[Ts]) -> T:
                return f(*args)

        except Exception:
            self.fail("Variadic tuples missing or broken")


if __name__ == "__main__":
    unittest.main()
