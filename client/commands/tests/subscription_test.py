# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
from pathlib import Path

import testslide

from ... import error
from ..incremental import InvalidServerResponse
from ..subscription import Error, Response, StatusUpdate, TypeErrors


class SubscriptionTest(testslide.TestCase):
    def test_parse_response(self) -> None:
        def assert_parsed(response: str, expected: Response) -> None:
            self.assertEqual(
                Response.parse(response),
                expected,
            )

        def assert_not_parsed(response: str) -> None:
            with self.assertRaises(InvalidServerResponse):
                Response.parse(response)

        assert_not_parsed("derp")
        assert_not_parsed("{}")
        assert_not_parsed("[]")
        assert_not_parsed('["Error"]')
        assert_not_parsed('{"name": "foo", "no_body": []}')
        assert_not_parsed('{"body": [], "no_name": "foo"}')
        assert_not_parsed('{"name": "foo", "body": ["Malformed"]}')
        assert_not_parsed('{"name": "foo", "body": ["TypeErrors", {}]}')
        assert_not_parsed('{"name": "foo", "body": ["StatusUpdate", 42]}')
        assert_not_parsed('{"name": "foo", "body": ["StatusUpdate", []]}')

        assert_parsed(
            json.dumps({"name": "foo", "body": ["TypeErrors", []]}),
            expected=Response(body=TypeErrors()),
        )
        assert_parsed(
            json.dumps(
                {
                    "name": "foo",
                    "body": [
                        "TypeErrors",
                        [
                            {
                                "line": 1,
                                "column": 1,
                                "stop_line": 2,
                                "stop_column": 2,
                                "path": "test.py",
                                "code": 42,
                                "name": "Fake name",
                                "description": "Fake description",
                            },
                        ],
                    ],
                }
            ),
            expected=Response(
                body=TypeErrors(
                    [
                        error.Error(
                            line=1,
                            column=1,
                            stop_line=2,
                            stop_column=2,
                            path=Path("test.py"),
                            code=42,
                            name="Fake name",
                            description="Fake description",
                        ),
                    ]
                ),
            ),
        )
        assert_parsed(
            json.dumps(
                {
                    "name": "foo",
                    "body": ["StatusUpdate", ["derp"]],
                }
            ),
            expected=Response(
                body=StatusUpdate(kind="derp"),
            ),
        )
        assert_parsed(
            json.dumps(
                {
                    "name": "foo",
                    "body": ["Error", "rip and tear!"],
                }
            ),
            expected=Response(
                body=Error(message="rip and tear!"),
            ),
        )

    def test_parse_code_navigation_response(self) -> None:
        def assert_parsed(response: str, expected: Response) -> None:
            self.assertEqual(
                Response.parse_code_navigation_response(response),
                expected,
            )

        def assert_not_parsed(response: str) -> None:
            with self.assertRaises(InvalidServerResponse):
                Response.parse_code_navigation_response(response)

        assert_not_parsed("derp")
        assert_not_parsed("{}")
        assert_not_parsed("[]")
        assert_not_parsed('["Error"]')
        assert_not_parsed('["ServerStatus", {}, "Extra"]')
        assert_not_parsed('["ServerStatus", 42]')

        assert_parsed(
            json.dumps(["ServerStatus", ["BusyChecking"]]),
            expected=Response(body=StatusUpdate(kind="BusyChecking")),
        )
        assert_parsed(
            json.dumps(
                [
                    "TypeErrors",
                    [
                        {
                            "line": 1,
                            "column": 1,
                            "stop_line": 2,
                            "stop_column": 2,
                            "path": "test.py",
                            "code": 42,
                            "name": "Fake name",
                            "description": "Fake description",
                        },
                    ],
                ]
            ),
            expected=Response(
                body=TypeErrors(
                    [
                        error.Error(
                            line=1,
                            column=1,
                            stop_line=2,
                            stop_column=2,
                            path=Path("test.py"),
                            code=42,
                            name="Fake name",
                            description="Fake description",
                        ),
                    ]
                ),
            ),
        )
        assert_parsed(
            json.dumps(["Error", "Needs more cowbell"]),
            expected=Response(
                body=Error(message="Needs more cowbell"),
            ),
        )
