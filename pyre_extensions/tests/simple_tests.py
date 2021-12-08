# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-ignore-all-errors
import sys
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
            from typing import TypeVar  # usort: skip wants trailing whitespace
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

    def test_generic__metaclass_conflict(self) -> None:
        def try_generic_child_class() -> None:
            from abc import ABC, abstractmethod
            from typing import TypeVar

            from .. import Generic

            T1 = TypeVar("T1")
            T2 = TypeVar("T2")

            class Base(ABC):
                @abstractmethod
                def some_method(self) -> None:
                    ...

            # Python complains at runtime if a child class has a metaclass that
            # is not a strict subclass of the base metaclass.
            # There should be no such metaclass conflict when using Pyre's
            # custom Generic in a child class.
            class Child(Base, Generic[T1, T2]):
                def some_method(self) -> None:
                    ...

        try:
            if sys.version_info >= (3, 7):
                try_generic_child_class()
            else:
                # Older versions of Python will raise the above metaclass
                # conflict `TypeError`.
                with self.assertRaises(TypeError):
                    try_generic_child_class()

        except Exception as exception:
            self.fail(
                "Generic/GenericMeta/Concatenate missing or broken: "
                f"Got exception `{exception}`"
            )

    def test_variadic_tuple(self) -> None:
        try:
            from .. import TypeVarTuple, Unpack

            T = TypeVar("T")
            Ts = TypeVarTuple("Ts")

            def apply(f: Callable[[Unpack[Ts]], T], *args: Unpack[Ts]) -> T:
                return f(*args)

        except Exception:
            self.fail("Variadic tuples missing or broken")

    def test_json(self) -> None:
        try:
            from .. import JSON
        except Exception:
            self.fail("JSON missing or broken")

        def test_json(x: JSON) -> None:
            try:
                # pyre-fixme: Pyre should complain about this
                y = x + 1
            except TypeError:
                # TypeError is anticipated for some instance where
                # Pyre would alert us to an issue if it wasn't
                # set to fixme
                pass

            # Pyre should not complain about these
            if isinstance(x, int):
                y = x + 1
            elif isinstance(x, float):
                y = x + 1.1
            elif isinstance(x, bool):
                y = x or True
            elif isinstance(x, str):
                y = x + "hello"
            elif isinstance(x, list):
                y = x + [4]  # noqa
            elif isinstance(x, dict):
                x["key"] = "value"

        test_json(3)
        test_json(3.5)
        test_json("test_string")
        test_json({"test": "dict"})
        test_json(["test_list"])


if __name__ == "__main__":
    unittest.main()
