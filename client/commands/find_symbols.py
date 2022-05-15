# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import ast
from typing import List, Union

from .language_server_protocol import (
    DocumentSymbolsResponse,
    LspRange,
    Position,
    SymbolKind,
)


def _node_to_symbol(
    node: Union[ast.FunctionDef, ast.ClassDef, ast.AsyncFunctionDef], kind: SymbolKind
) -> DocumentSymbolsResponse:
    start = Position(line=node.lineno, character=node.col_offset)
    end_lineno, end_col_offset = (node.end_lineno, node.end_col_offset)
    if end_lineno is not None and end_col_offset is not None:
        end = Position(line=end_lineno, character=end_col_offset)
    else:
        end = Position(line=node.lineno, character=node.col_offset + len(node.name))
    visitor = _SymbolsCollector()
    visitor.generic_visit(node)
    return DocumentSymbolsResponse(
        name=node.name,
        # TODO(114362484): add docstrings to details
        detail="",
        kind=kind,
        range=LspRange(
            start=start.to_lsp_position(),
            end=end.to_lsp_position(),
        ),
        selection_range=LspRange(
            start=start.to_lsp_position(),
            end=end.to_lsp_position(),
        ),
        children=visitor.symbols,
    )


class _SymbolsCollector(ast.NodeVisitor):
    symbols: List[DocumentSymbolsResponse]

    def __init__(self) -> None:
        super().__init__()
        self.symbols = []

    def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
        self.symbols.append(_node_to_symbol(node, SymbolKind.FUNCTION))

    def visit_AsyncFunctionDef(self, node: ast.AsyncFunctionDef) -> None:
        self.symbols.append(_node_to_symbol(node, SymbolKind.FUNCTION))

    def visit_ClassDef(self, node: ast.ClassDef) -> None:
        self.symbols.append(_node_to_symbol(node, SymbolKind.CLASS))


class UnparseableError(Exception):
    pass


# TODO(114362484): 1) Support details filled with docstrings/comments 2) incremental re-parsing via tree-sitter.
def parse_source_and_collect_symbols(source: str) -> List[DocumentSymbolsResponse]:
    try:
        ast_tree = ast.parse(source=source, mode="exec")
    except Exception as e:
        raise UnparseableError(e)
    visitor = _SymbolsCollector()
    visitor.visit(ast_tree)
    return visitor.symbols
