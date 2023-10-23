#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import testslide

from ...language_server import protocol as lsp

from ..source_code_context import SourceCodeContext


class SourceCodeContextTest(testslide.TestCase):
    def test_source_code_context_for_position(self) -> None:
        self.assertEqual(
            SourceCodeContext.from_source_and_position(
                source="\n".join(f"line {i}" for i in range(1, 10)),
                position=lsp.LspPosition(line=2, character=5),
            ),
            "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\nline 9",
        )

        self.assertEqual(
            SourceCodeContext.from_source_and_position(
                source="\n".join(f"line {i}" for i in range(1, 10)),
                position=lsp.LspPosition(line=2, character=5),
                max_lines_before_or_after=2,
            ),
            "line 1\nline 2\nline 3\nline 4\nline 5",
        )

        self.assertEqual(
            SourceCodeContext.from_source_and_position(
                source="\n".join(f"line {i}" for i in range(1, 10)),
                position=lsp.LspPosition(line=2, character=5),
                max_lines_before_or_after=3000,
            ),
            "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\nline 9",
        )

        self.assertEqual(
            SourceCodeContext.from_source_and_position(
                source="\n".join(f"line {i}" for i in range(1, 10)),
                position=lsp.LspPosition(line=50, character=5),
            ),
            None,
        )

    def test_character_at_position(self) -> None:
        self.assertEqual(
            SourceCodeContext.character_at_position(
                "", lsp.LspPosition(line=0, character=1)
            ),
            None,
        )
        self.assertEqual(
            SourceCodeContext.character_at_position(
                " ", lsp.LspPosition(line=1, character=0)
            ),
            None,
        )
        self.assertEqual(
            SourceCodeContext.character_at_position(
                " ", lsp.LspPosition(line=0, character=0)
            ),
            " ",
        )
        self.assertEqual(
            SourceCodeContext.character_at_position(
                "\nt", lsp.LspPosition(line=1, character=0)
            ),
            "t",
        )

    def test_text_at_range(self) -> None:
        test_text = """
import bar

def foo() -> None:
    print("Hello")
"""
        self.assertEqual(
            SourceCodeContext.text_at_range(
                test_text,
                lsp.LspRange(
                    start=lsp.LspPosition(line=1, character=0),
                    end=lsp.LspPosition(line=1, character=6),
                ),
            ),
            "import",
        )
        self.assertEqual(
            SourceCodeContext.text_at_range(
                test_text,
                lsp.LspRange(
                    start=lsp.LspPosition(line=1, character=0),
                    end=lsp.LspPosition(line=2, character=0),
                ),
            ),
            "import bar\n",
        )
        self.assertEqual(
            SourceCodeContext.text_at_range(
                test_text,
                lsp.LspRange(
                    start=lsp.LspPosition(line=1, character=0),
                    end=lsp.LspPosition(line=3, character=3),
                ),
            ),
            "import bar\n\ndef",
        )
        self.assertEqual(
            SourceCodeContext.text_at_range(
                test_text,
                lsp.LspRange(
                    start=lsp.LspPosition(line=-1, character=0),
                    end=lsp.LspPosition(line=1, character=6),
                ),
            ),
            None,
        )
        self.assertEqual(
            SourceCodeContext.text_at_range(
                test_text,
                lsp.LspRange(
                    start=lsp.LspPosition(line=1, character=0),
                    end=lsp.LspPosition(line=1, character=27),
                ),
            ),
            None,
        )
        self.assertEqual(
            SourceCodeContext.text_at_range(
                test_text,
                lsp.LspRange(
                    start=lsp.LspPosition(line=0, character=6),
                    end=lsp.LspPosition(line=0, character=4),
                ),
            ),
            None,
        )
