# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import json
from pathlib import Path

import testslide

from .. import code_navigation_request, protocol as lsp


class CodeNavigationRequestsTest(testslide.TestCase):
    def test_serialize_request(self) -> None:
        hover_request = code_navigation_request.HoverRequest(
            path=Path("/a/b.py"),
            overlay_id=None,
            position=lsp.PyrePosition(line=1, character=2),
        )
        self.assertEqual(
            hover_request.to_json(),
            [
                "Hover",
                {
                    "module": ["OfPath", "/a/b.py"],
                    "overlay_id": None,
                    "position": {"line": 1, "column": 2},
                },
            ],
        )

        hover_request = code_navigation_request.HoverRequest(
            path=Path("/a/b.py"),
            overlay_id="overlay_key",
            position=lsp.PyrePosition(line=1, character=2),
        )
        self.assertEqual(
            hover_request.to_json(),
            [
                "Hover",
                {
                    "module": ["OfPath", "/a/b.py"],
                    "overlay_id": "overlay_key",
                    "position": {"line": 1, "column": 2},
                },
            ],
        )
        definition_request = code_navigation_request.LocationOfDefinitionRequest(
            path=Path("/a/b.py"),
            overlay_id="overlay_key",
            position=lsp.PyrePosition(line=1, character=2),
        )
        self.assertEqual(
            definition_request.to_json(),
            [
                "LocationOfDefinition",
                {
                    "module": ["OfPath", "/a/b.py"],
                    "overlay_id": "overlay_key",
                    "position": {"line": 1, "column": 2},
                },
            ],
        )

    def test_parse_raw_response(self) -> None:
        raw_response = json.dumps(
            [
                "NotHover",
                {"contents": [{"kind": ["PlainText"], "value": "`int`"}]},
            ]
        )
        self.assertEqual(
            code_navigation_request.parse_raw_response(
                raw_response,
                expected_response_kind="Hover",
                response_type=code_navigation_request.HoverResponse,
            ),
            code_navigation_request.ErrorResponse(
                f"Invalid response {raw_response} to hover request."
            ),
        )

        raw_response = json.dumps(
            [
                "Hover",
                {"contents": [{"kind": ["PlainText"], "value": "`int`"}]},
                "ExtraField",
            ]
        )

        self.assertEqual(
            code_navigation_request.parse_raw_response(
                raw_response,
                expected_response_kind="Hover",
                response_type=code_navigation_request.HoverResponse,
            ),
            code_navigation_request.ErrorResponse(
                f"Invalid response {raw_response} to hover request."
            ),
        )

    def test_hover_response(self) -> None:
        response = {"contents": [{"kind": ["PlainText"], "value": "`int`"}]}
        self.assertEqual(
            code_navigation_request.parse_response(
                response, response_type=code_navigation_request.HoverResponse
            ),
            code_navigation_request.HoverResponse(
                contents=[
                    code_navigation_request.HoverContent(
                        kind=["PlainText"], value="`int`"
                    )
                ]
            ),
        )

        # Note that there's a type error here in the TypedDict, but we happily parse it in our json_mixins, even
        # with the cached_schema().
        response = {"contents": [{"kind": ["PlainText"], "value": 32}]}
        self.assertEqual(
            code_navigation_request.parse_response(
                response, response_type=code_navigation_request.HoverResponse
            ),
            code_navigation_request.HoverResponse(
                contents=[
                    # pyre-ignore[6]: This is documenting a known type error, see comments in test above.
                    code_navigation_request.HoverContent(kind=["PlainText"], value=32)
                ]
            ),
        )

    def test_definition_response(self) -> None:
        response = {
            "definitions": [
                {
                    "path": "/a/b.py",
                    "range": {
                        "start": {"line": 1, "column": 2},
                        "stop": {"line": 1, "column": 6},
                    },
                }
            ]
        }
        self.assertEqual(
            code_navigation_request.parse_response(
                response,
                response_type=code_navigation_request.LocationOfDefinitionResponse,
            ),
            code_navigation_request.LocationOfDefinitionResponse(
                definitions=[
                    code_navigation_request.DefinitionResponse(
                        path="/a/b.py",
                        range=code_navigation_request.CodeNavigationRange(
                            code_navigation_request.CodeNavigationPosition(
                                line=1, column=2
                            ),
                            code_navigation_request.CodeNavigationPosition(
                                line=1, column=6
                            ),
                        ),
                    )
                ]
            ),
        )
