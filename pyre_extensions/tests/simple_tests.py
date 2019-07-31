# pyre-ignore-all-errors
import unittest
from typing import Callable, Iterable, Iterator, List, TypeVar


class BasicTestCase(unittest.TestCase):
    def test_parameter_specification(self):
        try:
            from .. import ParameterSpecification
            from ..type_variable_operators import (
                KeywordArgumentsOf,
                PositionalArgumentsOf,
            )

            TParams = ParameterSpecification("TParams")
            TReturn = TypeVar("T")

            def listify(
                f: Callable[TParams, TReturn]
            ) -> Callable[TParams, List[TReturn]]:
                def wrapped(
                    *args: PositionalArgumentsOf[TParams],
                    **kwargs: KeywordArgumentsOf[TParams]
                ):
                    return [f(*args, **kwargs)]

                return wrapped

        except Exception:
            self.fail("ParameterSpecification missing or broken")

    def test_list_variadics(self):
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

    def test_none_throws(self):
        try:
            from .. import none_throws

            none_throws(0)
            none_throws(0, "custom message")
        except Exception:
            self.fail("none_throws missing or broken")


if __name__ == "__main__":
    unittest.main()
