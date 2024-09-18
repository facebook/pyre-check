# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module provides shared logic for publishing diagnostics for type errors
"""

import dataclasses
import logging
from pathlib import Path
from typing import Collection, Dict, List, Optional

from .. import backend_arguments, error, json_rpc
from ..language_server import connections, protocol as lsp
from . import error_code_to_link_mapper, server_state

PYRE_DOCUMENTATION_LINK = "https://pyre-check.org/docs/errors/#"
LOG: logging.Logger = logging.getLogger(__name__)


async def _publish_diagnostics(
    output_channel: connections.AsyncTextWriter,
    path: Path,
    diagnostics: Collection[lsp.Diagnostic],
) -> None:
    LOG.debug(f"Publish diagnostics for {path}: {diagnostics}")
    await lsp.write_json_rpc(
        output_channel,
        json_rpc.Request(
            method="textDocument/publishDiagnostics",
            parameters=json_rpc.ByNameParameters(
                {
                    "uri": lsp.DocumentUri.from_file_path(path).unparse(),
                    "diagnostics": [diagnostic.to_dict() for diagnostic in diagnostics],
                }
            ),
        ),
    )


def type_errors_to_diagnostics(
    type_errors: Collection[error.Error],
) -> Dict[Path, List[lsp.Diagnostic]]:
    result: Dict[Path, List[lsp.Diagnostic]] = {}
    for type_error in type_errors:
        result.setdefault(type_error.path, []).append(
            type_error_to_diagnostic(type_error)
        )
    return result


def type_error_to_diagnostic(type_error: error.Error) -> lsp.Diagnostic:
    code_description = _get_code_description(type_error)
    return lsp.Diagnostic(
        range=lsp.LspRange(
            start=lsp.LspPosition(
                line=type_error.line - 1, character=type_error.column
            ),
            end=lsp.LspPosition(
                line=type_error.stop_line - 1, character=type_error.stop_column
            ),
        ),
        message=type_error.description,
        severity=lsp.DiagnosticSeverity.ERROR,
        code=None if code_description is None else "pyre (documentation link)",
        code_description=code_description,
        source="Pyre",
    )


def _get_code_description(type_error: error.Error) -> lsp.CodeDescription:
    code = type_error.code
    fragment = error_code_to_link_mapper.error_code_to_fragment.get(code, "")
    href = PYRE_DOCUMENTATION_LINK + fragment
    return lsp.CodeDescription(href=href)


@dataclasses.dataclass(frozen=True)
class ClientTypeErrorHandler:
    client_output_channel: connections.AsyncTextWriter
    server_state: server_state.ServerState
    remote_logging: Optional[backend_arguments.RemoteLogging] = None

    def update_type_errors(self, type_errors: Collection[error.Error]) -> None:
        LOG.info(
            "Refreshing type errors received from Pyre server. "
            f"Total number of type errors is {len(type_errors)}."
        )
        self.server_state.diagnostics = type_errors_to_diagnostics(type_errors)

    async def clear_type_errors_for_client(self) -> None:
        for path in self.server_state.diagnostics:
            await _publish_diagnostics(self.client_output_channel, path, [])

    async def show_type_errors_to_client(self) -> None:
        for path, diagnostics in self.server_state.diagnostics.items():
            await _publish_diagnostics(self.client_output_channel, path, diagnostics)

    async def show_overlay_type_errors(
        self,
        path: Path,
        type_errors: Collection[error.Error],
    ) -> None:
        LOG.info(
            f"Refreshing type errors at path {path}. "
            f"Total number of type errors is {len(type_errors)}."
        )
        diagnostics_by_path = type_errors_to_diagnostics(type_errors)
        diagnostics = diagnostics_by_path.get(path, [])
        await _publish_diagnostics(self.client_output_channel, path, diagnostics)
