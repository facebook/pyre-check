#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import testslide

from ...language_server import protocol as lsp

from .. import pyre_language_server


class SourceCodeContextTest(testslide.TestCase):
    def test_source_code_context_for_position(self) -> None:
        self.assertEqual(
            pyre_language_server.SourceCodeContext.from_source_and_position(
                source="\n".join(f"line {i}" for i in range(1, 10)),
                position=lsp.LspPosition(line=2, character=5),
            ),
            "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\nline 9",
        )

        self.assertEqual(
            pyre_language_server.SourceCodeContext.from_source_and_position(
                source="\n".join(f"line {i}" for i in range(1, 10)),
                position=lsp.LspPosition(line=2, character=5),
                max_lines_before_or_after=2,
            ),
            "line 1\nline 2\nline 3\nline 4\nline 5",
        )

        self.assertEqual(
            pyre_language_server.SourceCodeContext.from_source_and_position(
                source="\n".join(f"line {i}" for i in range(1, 10)),
                position=lsp.LspPosition(line=2, character=5),
                max_lines_before_or_after=3000,
            ),
            "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\nline 9",
        )

        self.assertEqual(
            pyre_language_server.SourceCodeContext.from_source_and_position(
                source="\n".join(f"line {i}" for i in range(1, 10)),
                position=lsp.LspPosition(line=50, character=5),
            ),
            None,
        )
