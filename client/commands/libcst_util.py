# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
from collections import defaultdict
from pathlib import Path
from typing import DefaultDict, List

import libcst
from libcst import metadata

from ..language_server import protocol as lsp

LOG: logging.Logger = logging.getLogger(__name__)

BUILTINS_BLOCKLIST = [
    "builtins.True",
    "builtins.False",
]


class QualifiedNameWithPositionVisitor(libcst.CSTVisitor):
    """
    A simple visitor that finds all the fully qualified names within a module.

    During the single passthrough, this visitor stores three things:
    1. All symbol positions per line
    2. For a given position of a symbol, a list of all the associated qualified names
    3. For a given qualified name, a list of all its associated positions.

    There are three main reasons for this:
    1. Storing position per line helps avoid having to re-traverse the tree to find the exact node.
    2. There are extremely rare scenarios a symbol can have multiple qualified names, which we need to aggregate for.
    3. Keeping a list of ranges for each qualified name allows constant time lookup of all references.

    Given its currently primary usecase is symbol renaming, the stored information is shortlived.

    TODO:
    1. Handle Import nodes. T164821394
    """

    METADATA_DEPENDENCIES = [
        metadata.FullyQualifiedNameProvider,
        metadata.PositionProvider,
    ]

    def __init__(self, position: lsp.LspPosition) -> None:
        self.name_to_ranges: DefaultDict[str, List[lsp.LspRange]] = defaultdict(list)
        self.qualified_names: List[str] = []
        self.symbol_range: lsp.LspRange = lsp.LspRange(
            start=lsp.LspPosition(line=0, character=0),
            end=lsp.LspPosition(line=0, character=0),
        )
        self.position: lsp.LspPosition = position

    def visit_Name(  # noqa: B906 - these are leaf nodes, no need to have recursive visits
        self, node: libcst.Name
    ) -> None:
        if self.get_metadata(metadata.PositionProvider, node):
            cst_position = self.get_metadata(metadata.PositionProvider, node)
            line = cst_position.start.line - 1
            lsp_range = lsp.LspRange(
                start=lsp.LspPosition(line=line, character=cst_position.start.column),
                end=lsp.LspPosition(line=line, character=cst_position.end.column),
            )

            if (
                line == self.position.line
                and lsp_range.start.character <= self.position.character
                and self.position.character <= lsp_range.end.character
            ):
                self.qualified_names = [
                    qualified_name.name
                    for qualified_name in self.get_metadata(
                        metadata.FullyQualifiedNameProvider, node
                    )
                ]
                self.symbol_range = lsp_range
            for qualified_name in self.get_metadata(
                metadata.FullyQualifiedNameProvider, node
            ):
                self.name_to_ranges[qualified_name.name].append(lsp_range)

    def find_references(self, include_builtins: bool = False) -> List[lsp.LspRange]:
        results = []
        for qualified_name in self.qualified_names:
            if include_builtins or qualified_name not in BUILTINS_BLOCKLIST:
                results.extend(self.name_to_ranges[qualified_name])
        return results

    def find_symbol_range(self) -> lsp.LspRange:
        return self.symbol_range


def generate_qualified_name_with_position_visitor(
    path: Path,
    global_root: Path,
    code: str,
    position: lsp.PyrePosition,
) -> QualifiedNameWithPositionVisitor:
    parsed = libcst.parse_module(code)
    fully_qualified_name = metadata.FullyQualifiedNameProvider.gen_cache(
        global_root, [str(path)]
    )
    wrapper = libcst.MetadataWrapper(
        parsed,
        cache={metadata.FullyQualifiedNameProvider: fully_qualified_name[str(path)]},
    )
    wrapper.resolve(metadata.FullyQualifiedNameProvider)
    wrapper.resolve(metadata.PositionProvider)
    visitor = QualifiedNameWithPositionVisitor(position.to_lsp_position())
    wrapper.visit(visitor)
    return visitor


def find_references(
    path: Path,
    global_root: Path,
    code: str,
    position: lsp.PyrePosition,
) -> List[lsp.LspLocation]:
    visitor = generate_qualified_name_with_position_visitor(
        path, global_root, code, position
    )
    references = visitor.find_references()
    return [
        lsp.LspLocation(
            uri=path.as_uri(),
            range=reference,
        )
        for reference in references
    ]


def find_symbol_range(
    path: Path,
    global_root: Path,
    code: str,
    position: lsp.PyrePosition,
) -> lsp.LspRange:
    visitor = generate_qualified_name_with_position_visitor(
        path, global_root, code, position
    )
    return visitor.find_symbol_range()
