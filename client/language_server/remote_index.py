# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module is used as an abstract layer for fetching information
from a remote code index/database.
"""

import abc
from pathlib import Path
from typing import List, Optional

from . import protocol as lsp


class AbstractRemoteIndex(abc.ABC):
    def __init__(
        self,
    ) -> None:
        return

    @abc.abstractmethod
    async def definition(
        self, path: Path, position: lsp.PyrePosition
    ) -> List[lsp.LspLocation]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def hover(
        self, path: Path, position: lsp.PyrePosition
    ) -> Optional[lsp.LspHoverResponse]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def symbol_search(self, query: str) -> Optional[lsp.WorkspaceSymbolResponse]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def references(
        self, path: Path, position: lsp.PyrePosition
    ) -> List[lsp.LspLocation]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def prepare_call_hierarchy(
        self,
        path: Path,
        position: lsp.PyrePosition,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> List[lsp.CallHierarchyItem]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def call_hierarchy_from_item(
        self,
        path: Path,
        item: lsp.CallHierarchyItem,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> List[lsp.CallHierarchyItem]:
        raise NotImplementedError()


class EmptyRemoteIndex(AbstractRemoteIndex):
    async def definition(
        self, path: Path, position: lsp.PyrePosition
    ) -> List[lsp.LspLocation]:
        return []

    async def references(
        self, path: Path, position: lsp.PyrePosition
    ) -> List[lsp.LspLocation]:
        return []

    async def hover(
        self, path: Path, position: lsp.PyrePosition
    ) -> Optional[lsp.LspHoverResponse]:
        return None

    async def symbol_search(self, query: str) -> Optional[lsp.WorkspaceSymbolResponse]:
        return None

    async def prepare_call_hierarchy(
        self,
        path: Path,
        position: lsp.PyrePosition,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> List[lsp.CallHierarchyItem]:
        return []

    async def call_hierarchy_from_item(
        self,
        path: Path,
        item: lsp.CallHierarchyItem,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> List[lsp.CallHierarchyItem]:
        return []
