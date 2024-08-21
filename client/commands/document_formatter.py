# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import abc
from pathlib import Path

from ..language_server import protocol as lsp


class AbstractDocumentFormatter(abc.ABC):
    @abc.abstractmethod
    def __init__(self) -> None:
        pass

    @abc.abstractmethod
    async def format_document(
        self, document_path: Path
    ) -> lsp.DocumentFormattingResponse:
        raise NotImplementedError("Document formatting not yet implemented")
