# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-ignore-all-errors

import unittest
from io import StringIO
from typing import Any, Dict, List, Optional, Type, TypeVar, Union

import typing_extensions

from .. import safe_json

T = TypeVar("T")


class Movie(typing_extensions.TypedDict):
    name: str
    year: int


class MovieWithRating(Movie):
    rating: float


class MovieWithArbitraryDictionary(Movie):
    dictionary: Dict[str, Any]


class MovieWithUnion(Movie):
    int_or_str: Union[int, str]


class MovieWithNonRequiredField(Movie, total=False):
    not_required: str


class MovieAlternative(typing_extensions.TypedDict):
    name: str
    year: int


class BasicTestCase(unittest.TestCase):
    def _assert_loads(self, input: str, target_type: Type[T], output: T) -> None:
        self.assertEqual(
            safe_json.loads(
                input,
                target_type,
            ),
            output,
        )

    def _assert_loads_fails(self, input: str, target_type: Type[T]) -> None:
        with self.assertRaises(safe_json.InvalidJson):
            safe_json.loads(
                input,
                target_type,
            )

    def test_loads(self) -> None:
        # Primitives.
        self.assertEqual(safe_json.loads("1", int), 1)
        self.assertEqual(safe_json.loads("true", bool), True)
        self.assertEqual(safe_json.loads("1.1", float), 1.1)
        self.assertEqual(safe_json.loads('"string"', str), "string")

        with self.assertRaises(safe_json.InvalidJson):
            safe_json.loads("1", bool)

        with self.assertRaises(safe_json.InvalidJson):
            safe_json.loads("1", float)

        with self.assertRaises(safe_json.InvalidJson):
            safe_json.loads("1", str)

        with self.assertRaises(safe_json.InvalidJson):
            safe_json.loads("true", float)

        with self.assertRaises(safe_json.InvalidJson):
            safe_json.loads("true", str)

        with self.assertRaises(safe_json.InvalidJson):
            safe_json.loads("1.1", int)

        with self.assertRaises(safe_json.InvalidJson):
            safe_json.loads("1.1", bool)

        with self.assertRaises(safe_json.InvalidJson):
            safe_json.loads("1.1", str)

        with self.assertRaises(safe_json.InvalidJson):
            safe_json.loads("hello", int)

        with self.assertRaises(safe_json.InvalidJson):
            safe_json.loads("hello", bool)

        with self.assertRaises(safe_json.InvalidJson):
            safe_json.loads("hello", float)

        # Lists.
        self.assertEqual(safe_json.loads("[]", List[int]), [])
        self.assertEqual(safe_json.loads("[1]", List[int]), [1])
        self.assertEqual(safe_json.loads("[1, 2]", List[int]), [1, 2])

        self.assertEqual(
            safe_json.loads('[{"1": 1}]', List[Dict[str, int]]), [{"1": 1}]
        )

        with self.assertRaises(safe_json.InvalidJson):
            safe_json.loads("[1, 'string']", List[int])

        # Dictionaries.
        self.assertEqual(safe_json.loads("{}", Dict[int, str]), {})
        self.assertEqual(safe_json.loads('{"1": 1}', Dict[str, int]), {"1": 1})

        with self.assertRaises(safe_json.InvalidJson):
            safe_json.loads('{"1": "string"}', Dict[str, int])
        with self.assertRaises(safe_json.InvalidJson):
            safe_json.loads('{"1": 1, "2": "2"}', Dict[str, int])

        self.assertEqual(
            safe_json.loads('{"1": {"2": 3}}', Dict[str, Dict[str, int]]),
            {"1": {"2": 3}},
        )

        # Typed dictionaries.
        self.assertEqual(
            safe_json.loads('{"name": "The Matrix", "year": 1999}', Movie),
            {"name": "The Matrix", "year": 1999},
        )
        with self.assertRaises(safe_json.InvalidJson):
            safe_json.loads('{"name": "The Matrix", "year": ""}', Movie)

        # Any.
        self.assertEqual(safe_json.loads("[1]", List[Any]), [1])
        self.assertEqual(safe_json.loads('[{"1": 1}]', List[Any]), [{"1": 1}])

        # Optionals.
        self.assertEqual(safe_json.loads("[1]", List[Optional[int]]), [1])
        self.assertEqual(safe_json.loads("[null, 2]", List[Optional[int]]), [None, 2])

        # Validation can be turned off.
        self.assertEqual(safe_json.loads("[1]", List[str], validate=False), [1])

    def test_validate(self) -> None:
        # Lists.
        parsedListStr = ["1", "2"]

        self.assertEqual(safe_json.validate(parsedListStr, List[str]), parsedListStr)
        with self.assertRaises(safe_json.InvalidJson):
            safe_json.validate(parsedListStr, List[int])

        # Dictionaries.
        parsedDictBasic = {"1": 1}

        self.assertEqual(
            safe_json.validate(parsedDictBasic, Dict[str, int]), parsedDictBasic
        )
        with self.assertRaises(safe_json.InvalidJson):
            safe_json.validate(parsedDictBasic, List[Any])

        parsedDictNested = {"1": {"2": 3}}

        self.assertEqual(
            safe_json.validate(parsedDictNested, Dict[str, Dict[str, int]]),
            parsedDictNested,
        )
        with self.assertRaises(safe_json.InvalidJson):
            safe_json.validate(parsedDictNested, Dict[str, int])

        # Typed dictionaries.
        parsedDictTyped = {"name": "The Matrix", "year": 1999}
        parsedDictTypedFailing = {"name": "The Matrix", "year": ""}

        self.assertEqual(safe_json.validate(parsedDictTyped, Movie), parsedDictTyped)
        with self.assertRaises(safe_json.InvalidJson):
            safe_json.validate(parsedDictTypedFailing, Movie)

        # Any.
        parsedAny = [{"1": 1}]

        self.assertEqual(safe_json.validate(parsedAny, List[Any]), parsedAny)

        # Optionals.

        parsedOptionals = [2, None, 4]
        self.assertEqual(
            safe_json.validate(parsedOptionals, List[Optional[int]]), parsedOptionals
        )

    def test_load(self) -> None:
        f = StringIO('{"1": {"2": 3}}')

        self.assertEqual(safe_json.load(f, Dict[str, Dict[str, int]]), {"1": {"2": 3}})
        with self.assertRaises(safe_json.InvalidJson):
            safe_json.load(f, Dict[int, Dict[int, int]])

    def test_loads_typed_dictionary(self) -> None:
        # Field that is not present in the TypedDict.
        self._assert_loads(
            '{"name": "The Matrix Reloaded", "year": 1999, "extra_field": "hello"}',
            Movie,
            {"name": "The Matrix Reloaded", "year": 1999, "extra_field": "hello"},
        )

        # TypedDict inheriting from another.
        self._assert_loads(
            '{"name": "The Matrix", "year": 1999, "rating": 9.0}',
            MovieWithRating,
            {"name": "The Matrix", "year": 1999, "rating": 9.0},
        )

        self._assert_loads_fails(
            '{"name": "The Matrix", "year": 1999, "rating": "not a float"}',
            MovieWithRating,
        )

        # TypedDict with a field accepting an arbitrary dictionary.
        self._assert_loads(
            '{"name": "The Matrix", "year": 1999,'
            + ' "dictionary": {"foo": "bar", "baz": {}}}',
            MovieWithArbitraryDictionary,
            {
                "name": "The Matrix",
                "year": 1999,
                "dictionary": {"foo": "bar", "baz": {}},
            },
        )

        self._assert_loads_fails(
            '{"name": "The Matrix", "year": 1999, "dictionary": [1, 2]}',
            MovieWithArbitraryDictionary,
        )

        # TODO(T92804673): Unions are not supported.
        self._assert_loads_fails(
            '{"name": "The Matrix", "year": 1999, "int_or_str": 1}',
            MovieWithUnion,
        )

        self._assert_loads(
            '{"name": "The Matrix", "year": 1999, "not_required": "hello"}',
            MovieWithNonRequiredField,
            {"name": "The Matrix", "year": 1999, "not_required": "hello"},
        )

        # TODO(T92805077): Missing non-required field should not be an error.
        self._assert_loads_fails(
            '{"name": "The Matrix", "year": 1999}',
            MovieWithNonRequiredField,
        )

        # `typing_extensions.TypedDict` should also work
        self._assert_loads(
            '{"name": "The Matrix", "year": 1999}',
            MovieAlternative,
            {"name": "The Matrix", "year": 1999},
        )


if __name__ == "__main__":
    unittest.main()
