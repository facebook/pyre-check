# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module is used as an abstract layer for fetching information
from a remote code index/database.
"""

import abc
from pathlib import Path
from typing import List

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
    ) -> List[lsp.LspLocation]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def references(
        self, path: Path, position: lsp.PyrePosition
    ) -> List[lsp.LspLocation]:
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
    ) -> List[lsp.LspLocation]:
        return []
