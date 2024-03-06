# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import json
from pathlib import Path

import testslide

from ... import error
from .. import incremental


class IncrementalTest(testslide.TestCase):
    def test_parse_response(self) -> None:
        def assert_parsed(response: str, expected: incremental.TypeErrors) -> None:
            self.assertEqual(incremental.parse_type_error_response(response), expected)

        def assert_not_parsed(response: str) -> None:
            with self.assertRaises(incremental.InvalidServerResponse):
                incremental.parse_type_error_response(response)

        assert_not_parsed("derp")
        assert_not_parsed("{}")
        assert_not_parsed("[]")
        assert_not_parsed('["Error"]')
        assert_not_parsed('["TypeErrors"]')
        assert_not_parsed('["TypeErrors", "derp"]')
        assert_not_parsed('["TypeErrors", {"errors": 42}]')
        assert_not_parsed('["TypeErrors", {"errors": [], "build_failure": 4.2}]')

        assert_parsed('["TypeErrors", []]', incremental.TypeErrors())
        assert_parsed('["TypeErrors", {}]', incremental.TypeErrors())
        assert_not_parsed('["TypeErrors", ["derp"]]')
        assert_not_parsed('["TypeErrors", [{}]]')
        assert_parsed(
            json.dumps(
                [
                    "TypeErrors",
                    [
                        {
                            "line": 1,
                            "column": 1,
                            "stop_line": 3,
                            "stop_column": 3,
                            "path": "test.py",
                            "code": 42,
                            "name": "Fake name",
                            "description": "Fake description",
                        },
                        {
                            "line": 2,
                            "column": 2,
                            "stop_line": 4,
                            "stop_column": 4,
                            "path": "test.py",
                            "code": 43,
                            "name": "Fake name 2",
                            "description": "Fake description 2",
                            "concise_description": "Concise description 2",
                        },
                    ],
                ]
            ),
            expected=incremental.TypeErrors(
                errors=[
                    error.Error(
                        line=1,
                        column=1,
                        stop_line=3,
                        stop_column=3,
                        path=Path("test.py"),
                        code=42,
                        name="Fake name",
                        description="Fake description",
                    ),
                    error.Error(
                        line=2,
                        column=2,
                        stop_line=4,
                        stop_column=4,
                        path=Path("test.py"),
                        code=43,
                        name="Fake name 2",
                        description="Fake description 2",
                        concise_description="Concise description 2",
                    ),
                ],
                build_failure=None,
            ),
        )
        assert_parsed(
            json.dumps(
                [
                    "TypeErrors",
                    {
                        "errors": [
                            {
                                "line": 1,
                                "column": 1,
                                "stop_line": 3,
                                "stop_column": 3,
                                "path": "test.py",
                                "code": 42,
                                "name": "Fake name",
                                "description": "Fake description",
                            }
                        ],
                        "build_failure": "Build failure description",
                    },
                ]
            ),
            expected=incremental.TypeErrors(
                errors=[
                    error.Error(
                        line=1,
                        column=1,
                        stop_line=3,
                        stop_column=3,
                        path=Path("test.py"),
                        code=42,
                        name="Fake name",
                        description="Fake description",
                    ),
                ],
                build_failure="Build failure description",
            ),
        )
