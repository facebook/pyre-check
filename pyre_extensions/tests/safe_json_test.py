# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-ignore-all-errors

import unittest
from io import StringIO
from typing import Any, Dict, List, Optional

from typing_extensions import TypedDict

from .. import safe_json


class ExampleTypedDict(TypedDict):
    string: str
    integer: int


class BasicTestCase(unittest.TestCase):
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
            safe_json.loads('{"string": "", "integer": 1}', ExampleTypedDict),
            {"string": "", "integer": 1},
        )
        with self.assertRaises(safe_json.InvalidJson):
            safe_json.loads('{"string": "", "integer": ""}', ExampleTypedDict)

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
        parsedDictTyped = {"string": "", "integer": 1}
        parsedDictTypedFailing = {"string": "", "integer": ""}

        self.assertEqual(
            safe_json.validate(parsedDictTyped, ExampleTypedDict), parsedDictTyped
        )
        with self.assertRaises(safe_json.InvalidJson):
            safe_json.validate(parsedDictTypedFailing, ExampleTypedDict)

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


if __name__ == "__main__":
    unittest.main()
