# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from pathlib import Path
from typing import Any, Dict
from unittest.mock import patch

from .. import __name__ as client
from ..error import (
    Error,
    ErrorParsingFailure,
    LegacyError,
    ModelVerificationError,
    TaintConfigurationError,
)


class ErrorTest(unittest.TestCase):
    fake_error = {
        "line": 4,
        "column": 11,
        "stop_line": 4,
        "stop_column": 21,
        "path": "c.py",
        "code": -1,
        "name": "Revealed type",
        "description": "Fake error",
        "define": "c.$toplevel",
    }

    def test_json_parsing(self) -> None:
        def assert_parsed(json: Dict[str, Any], expected: Error) -> None:
            self.assertEqual(Error.from_json(json), expected)

        def assert_not_parsed(json: Dict[str, Any]) -> None:
            with self.assertRaises(ErrorParsingFailure):
                Error.from_json(json)

        assert_not_parsed({})
        assert_not_parsed({"derp": 42})
        assert_not_parsed({"line": "abc", "column": []})
        assert_not_parsed({"line": 1, "column": 1})

        assert_parsed(
            {
                "line": 1,
                "column": 1,
                "stop_line": 2,
                "stop_column": 2,
                "path": "test.py",
                "code": 1,
                "name": "Some name",
                "description": "Some description",
            },
            expected=Error(
                line=1,
                column=1,
                stop_line=2,
                stop_column=2,
                path=Path("test.py"),
                code=1,
                name="Some name",
                description="Some description",
            ),
        )
        assert_parsed(
            {
                "line": 2,
                "column": 2,
                "stop_line": 3,
                "stop_column": 3,
                "path": Path("test.py"),
                "code": 2,
                "name": "Some name",
                "description": "Some description",
                "long_description": "Some long description",
            },
            expected=Error(
                line=2,
                column=2,
                stop_line=3,
                stop_column=3,
                path=Path("test.py"),
                code=2,
                name="Some name",
                description="Some description",
                long_description="Some long description",
            ),
        )
        assert_parsed(
            {
                "line": 3,
                "column": 3,
                "stop_line": 4,
                "stop_column": 4,
                "path": Path("test.py"),
                "code": 3,
                "name": "Some name",
                "description": "Some description",
                "concise_description": "Some concise description",
            },
            expected=Error(
                line=3,
                column=3,
                stop_line=4,
                stop_column=4,
                path=Path("test.py"),
                code=3,
                name="Some name",
                description="Some description",
                concise_description="Some concise description",
            ),
        )

    def test_repr(self) -> None:
        error = LegacyError.create(self.fake_error)

        with patch("{}.terminal.is_capable".format(client), return_value=True):
            self.assertEqual(
                repr(error),
                "\x1b[31mc.py\x1b[0m:\x1b[33m4\x1b[0m:\x1b[33m11\x1b[0m Fake error",
            )

        with patch("{}.terminal.is_capable".format(client), return_value=False):
            self.assertEqual(repr(error), "c.py:4:11 Fake error")

    def test_key_with_color(self) -> None:
        error = LegacyError.create(self.fake_error)

        self.assertEqual(
            error._key_with_color(),
            "\x1b[31mc.py\x1b[0m:\x1b[33m4\x1b[0m:\x1b[33m11\x1b[0m",
        )

    def test_long_description(self) -> None:
        error = LegacyError.create(self.fake_error)
        self.assertEqual(error.error.long_description, "")

        error_with_long_description = self.fake_error
        error_with_long_description[
            "long_description"
        ] = "Fake error.\nAnd this is why this is an error."
        error = LegacyError.create(error_with_long_description)
        self.assertEqual(
            error.error.long_description,
            "Fake error.\nAnd this is why this is an error.",
        )

    def test_taint_configuration_error_parsing(self) -> None:
        def assert_parsed(
            json: Dict[str, Any], expected: TaintConfigurationError
        ) -> None:
            self.assertEqual(TaintConfigurationError.from_json(json), expected)

        def assert_not_parsed(json: Dict[str, Any]) -> None:
            with self.assertRaises(ErrorParsingFailure):
                TaintConfigurationError.from_json(json)

        assert_not_parsed({})
        assert_not_parsed({"derp": 42})
        assert_not_parsed({"line": "abc", "column": []})
        assert_not_parsed({"line": 1, "column": 1})

        assert_parsed(
            {
                "path": "test.py",
                "description": "Some description",
                "code": 1001,
            },
            expected=TaintConfigurationError(
                path=Path("test.py"),
                description="Some description",
                code=1001,
            ),
        )
        assert_parsed(
            {
                "path": None,
                "description": "Some description",
                "code": 1001,
            },
            expected=TaintConfigurationError(
                path=None,
                description="Some description",
                code=1001,
            ),
        )

    def test_model_verification_error_parsing(self) -> None:
        def assert_parsed(
            json: Dict[str, Any], expected: ModelVerificationError
        ) -> None:
            self.assertEqual(ModelVerificationError.from_json(json), expected)

        def assert_not_parsed(json: Dict[str, Any]) -> None:
            with self.assertRaises(ErrorParsingFailure):
                ModelVerificationError.from_json(json)

        assert_not_parsed({})
        assert_not_parsed({"derp": 42})
        assert_not_parsed({"line": "abc", "column": []})
        assert_not_parsed({"line": 1, "column": 1})

        assert_parsed(
            {
                "line": 1,
                "column": 1,
                "stop_line": 2,
                "stop_column": 3,
                "path": "test.py",
                "description": "Some description",
                "code": 1001,
            },
            expected=ModelVerificationError(
                line=1,
                column=1,
                stop_line=2,
                stop_column=3,
                path=Path("test.py"),
                description="Some description",
                code=1001,
            ),
        )
        assert_parsed(
            {
                "line": 1,
                "column": 1,
                "stop_line": 2,
                "stop_column": 3,
                "path": None,
                "description": "Some description",
                "code": 1001,
            },
            expected=ModelVerificationError(
                line=1,
                column=1,
                stop_line=2,
                stop_column=3,
                path=None,
                description="Some description",
                code=1001,
            ),
        )
