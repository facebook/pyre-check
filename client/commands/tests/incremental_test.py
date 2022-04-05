# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import json
from pathlib import Path
from typing import Iterable

import testslide

from ...error import Error
from ..incremental import (
    compute_error_statistics_per_code,
    InvalidServerResponse,
    parse_type_error_response,
)


class IncrementalTest(testslide.TestCase):
    def test_parse_response(self) -> None:
        def assert_parsed(response: str, expected: Iterable[Error]) -> None:
            self.assertListEqual(parse_type_error_response(response), list(expected))

        def assert_not_parsed(response: str) -> None:
            with self.assertRaises(InvalidServerResponse):
                parse_type_error_response(response)

        assert_not_parsed("derp")
        assert_not_parsed("{}")
        assert_not_parsed("[]")
        assert_not_parsed('["Error"]')
        assert_not_parsed('["TypeErrors"]')
        assert_not_parsed('["TypeErrors", "derp"]')
        assert_not_parsed('["TypeErrors", {}]')

        assert_parsed('["TypeErrors", []]', [])
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
                            "long_description": "Long description 2",
                        },
                    ],
                ]
            ),
            expected=[
                Error(
                    line=1,
                    column=1,
                    stop_line=3,
                    stop_column=3,
                    path=Path("test.py"),
                    code=42,
                    name="Fake name",
                    description="Fake description",
                ),
                Error(
                    line=2,
                    column=2,
                    stop_line=4,
                    stop_column=4,
                    path=Path("test.py"),
                    code=43,
                    name="Fake name 2",
                    description="Fake description 2",
                    concise_description="Concise description 2",
                    long_description="Long description 2",
                ),
            ],
        )

    def test_compute_error_statistics_per_code(self) -> None:
        error_42 = Error(
            line=1,
            column=1,
            stop_line=3,
            stop_column=3,
            path=Path("test.py"),
            code=42,
            name="Fake name",
            description="Fake description",
        )
        error_43 = dataclasses.replace(error_42, code=43)
        self.assertListEqual(
            [
                {"code": 42, "count": 2},
                {"code": 43, "count": 1},
            ],
            list(compute_error_statistics_per_code([error_42, error_43, error_42])),
        )
