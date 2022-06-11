# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import sys
import textwrap
from typing import List, Optional

import testslide

from ..find_symbols import parse_source_and_collect_symbols, UnparseableError
from ..language_server_protocol import (
    DocumentSymbolsResponse,
    LspPosition,
    LspRange,
    SymbolKind,
)


def make_document_symbol(
    name: str,
    detail: str,
    kind: SymbolKind,
    range: LspRange,
    children: Optional[List[DocumentSymbolsResponse]] = None,
) -> DocumentSymbolsResponse:
    return DocumentSymbolsResponse(
        name=name,
        detail=detail,
        kind=kind,
        range=range,
        selection_range=range,
        children=children if children else [],
    )


# Prior to 3.8, the ast module doesn't support some of the functions we use.
#
# As a result, finding symbols doesn't work well for ranges. We have other tests
# that verify we degrade gracefully.
if (sys.version_info.major, sys.version_info.minor) >= (3, 8):

    class FindSymbolTests(testslide.TestCase):
        def assert_collected_symbols(
            self, source: str, expected_symbols: List[DocumentSymbolsResponse]
        ) -> None:
            self.maxDiff = None
            self.assertListEqual(
                parse_source_and_collect_symbols(textwrap.dedent(source)),
                expected_symbols,
            )

        def test_parse_source_and_collect_symbols_function(self) -> None:
            self.assert_collected_symbols(
                """
                def foo(x):
                    pass
                """,
                [
                    make_document_symbol(
                        name="foo",
                        detail="",
                        kind=SymbolKind.FUNCTION,
                        range=LspRange(
                            start=LspPosition(line=1, character=0),
                            end=LspPosition(line=2, character=len("    pass")),
                        ),
                    )
                ],
            )

        def test_parse_source_and_collect_symbols_multiple_functions(self) -> None:
            self.assert_collected_symbols(
                """
                def foo(x):
                    return x
                def bar(y):
                    return y
                bar(None)
                """,
                [
                    make_document_symbol(
                        name="foo",
                        detail="",
                        kind=SymbolKind.FUNCTION,
                        range=LspRange(
                            start=LspPosition(line=1, character=0),
                            end=LspPosition(line=2, character=len("    return x")),
                        ),
                    ),
                    make_document_symbol(
                        name="bar",
                        detail="",
                        kind=SymbolKind.FUNCTION,
                        range=LspRange(
                            start=LspPosition(line=3, character=0),
                            end=LspPosition(line=4, character=len("    return y")),
                        ),
                    ),
                ],
            )

        def test_parse_source_and_collect_symbols_multiple_classes(self) -> None:
            self.assert_collected_symbols(
                """
                class foo:
                    x = 1
                class bar:
                    y = 2
                """,
                [
                    make_document_symbol(
                        name="foo",
                        detail="",
                        kind=SymbolKind.CLASS,
                        range=LspRange(
                            start=LspPosition(line=1, character=0),
                            end=LspPosition(line=2, character=len("    x = 1")),
                        ),
                    ),
                    make_document_symbol(
                        name="bar",
                        detail="",
                        kind=SymbolKind.CLASS,
                        range=LspRange(
                            start=LspPosition(line=3, character=0),
                            end=LspPosition(line=4, character=len("    y = 2")),
                        ),
                    ),
                ],
            )

        def test_parse_source_and_collect_symbols_method(self) -> None:
            self.maxDiff = None
            self.assert_collected_symbols(
                """
                class foo:
                    def bar(self):
                        return self
                """,
                [
                    make_document_symbol(
                        name="foo",
                        detail="",
                        kind=SymbolKind.CLASS,
                        range=LspRange(
                            start=LspPosition(line=1, character=0),
                            end=LspPosition(
                                line=3, character=len("        return self")
                            ),
                        ),
                        children=[
                            make_document_symbol(
                                name="bar",
                                detail="",
                                kind=SymbolKind.FUNCTION,
                                range=LspRange(
                                    start=LspPosition(line=2, character=4),
                                    end=LspPosition(
                                        line=3, character=len("        return self")
                                    ),
                                ),
                            )
                        ],
                    ),
                ],
            )

        def test_parse_source_and_collect_symbols_nested_classes(self) -> None:
            self.assert_collected_symbols(
                """
                class foo:
                    class bar:
                        def foobar(self):
                            return self
                """,
                [
                    make_document_symbol(
                        name="foo",
                        detail="",
                        kind=SymbolKind.CLASS,
                        range=LspRange(
                            start=LspPosition(line=1, character=0),
                            end=LspPosition(
                                line=4, character=len("            return self")
                            ),
                        ),
                        children=[
                            make_document_symbol(
                                name="bar",
                                detail="",
                                kind=SymbolKind.CLASS,
                                range=LspRange(
                                    start=LspPosition(line=2, character=4),
                                    end=LspPosition(
                                        line=4, character=len("            return self")
                                    ),
                                ),
                                children=[
                                    make_document_symbol(
                                        name="foobar",
                                        detail="",
                                        kind=SymbolKind.FUNCTION,
                                        range=LspRange(
                                            start=LspPosition(line=3, character=8),
                                            end=LspPosition(
                                                line=4,
                                                character=len(
                                                    "            return self"
                                                ),
                                            ),
                                        ),
                                    )
                                ],
                            )
                        ],
                    ),
                ],
            )

        def test_parse_source_and_collect_symbols_nested_funcs(self) -> None:
            self.assert_collected_symbols(
                """
                def foo(x):
                    def bar(y):
                        def foobar(xy):
                            return x * y * xy
                        foobar(y)
                    return bar(x)
                """,
                [
                    make_document_symbol(
                        name="foo",
                        detail="",
                        kind=SymbolKind.FUNCTION,
                        range=LspRange(
                            start=LspPosition(line=1, character=0),
                            end=LspPosition(line=6, character=len("    return bar(x)")),
                        ),
                        children=[
                            make_document_symbol(
                                name="bar",
                                detail="",
                                kind=SymbolKind.FUNCTION,
                                range=LspRange(
                                    start=LspPosition(line=2, character=4),
                                    end=LspPosition(
                                        line=5, character=len("        foobar(y)")
                                    ),
                                ),
                                children=[
                                    make_document_symbol(
                                        name="foobar",
                                        detail="",
                                        kind=SymbolKind.FUNCTION,
                                        range=LspRange(
                                            start=LspPosition(line=3, character=8),
                                            end=LspPosition(
                                                line=4,
                                                character=len(
                                                    "            return x * y * xy"
                                                ),
                                            ),
                                        ),
                                    )
                                ],
                            )
                        ],
                    ),
                ],
            )

        def test_parse_source_and_collect_async_funcs(self) -> None:
            self.assert_collected_symbols(
                """
                async def  foo(x):
                    await x
                """,
                [
                    make_document_symbol(
                        name="foo",
                        detail="",
                        kind=SymbolKind.FUNCTION,
                        range=LspRange(
                            start=LspPosition(line=1, character=0),
                            end=LspPosition(line=2, character=len("    await x")),
                        ),
                    )
                ],
            )

        def test_parse_source_and_collect_symbols_invalid_syntax(self) -> None:
            self.assertRaises(
                UnparseableError,
                parse_source_and_collect_symbols,
                "thisIsNotValidPython x = x",
            )

        def test_parse_source_and_collect_symbols_multiple_calls(self) -> None:
            for _ in range(2):
                self.assert_collected_symbols(
                    """
                            def foo(x):
                                pass
                            """,
                    [
                        make_document_symbol(
                            name="foo",
                            detail="",
                            kind=SymbolKind.FUNCTION,
                            range=LspRange(
                                start=LspPosition(line=1, character=0),
                                end=LspPosition(line=2, character=len("    pass")),
                            ),
                        )
                    ],
                )
