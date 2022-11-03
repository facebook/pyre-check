# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


from pathlib import Path

import testslide

from .. import code_navigation_request, protocol as lsp


class CodeNavigationRequestsTest(testslide.TestCase):
    def test_serialize(self) -> None:
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
