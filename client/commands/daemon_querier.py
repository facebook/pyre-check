# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
The request handler provides an interface and implementation for LSP related
queries (such as hover & definition). The current implementation of the request
handler involves a synchronous query to the Pyre server via a daemon connection,
but since the request handler also provides an interface (AbstractDaemonQuerier),
the request handler implementation can be mocked.
"""

import abc
import dataclasses
import json
import logging
import os
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Union

from .. import dataclasses_json_extensions as json_mixins, error
from ..language_server import (
    daemon_connection,
    protocol as lsp,
)
from . import (
    daemon_query,
    expression_level_coverage,
    server_state as state,
)

LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class QueryModulesOfPathResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: List[str]


@dataclasses.dataclass(frozen=True)
class DaemonQueryFailure(json_mixins.CamlCaseAndExcludeJsonMixin):
    error_message: str
    error_source: Optional[Exception] = None
    duration: float = 0


def file_not_typechecked_coverage_result() -> lsp.TypeCoverageResponse:
    return lsp.TypeCoverageResponse(
        covered_percent=0.0,
        uncovered_ranges=[
            lsp.Diagnostic(
                range=lsp.LspRange(
                    start=lsp.LspPosition(
                        line=0,
                        character=0,
                    ),
                    end=lsp.LspPosition(line=1, character=0),
                ),
                message="This file is not type checked by Pyre.",
            )
        ],
        default_message="",
    )


def path_to_expression_coverage_response(
    strict_default: bool,
    expression_coverage: expression_level_coverage.ExpressionLevelCoverageResponse,
) -> lsp.TypeCoverageResponse:
    path_coverage = expression_coverage.response[0]
    if isinstance(path_coverage, expression_level_coverage.ErrorAtPathResponse):
        uncovered_expressions_diagnostics: List[lsp.Diagnostic] = []
        covered_percent = 0
    else:
        uncovered_expressions_diagnostics = (
            expression_level_coverage.get_uncovered_expression_diagnostics(
                expression_coverage
            )
        )
        covered_percent = expression_level_coverage.get_percent_covered_per_path(
            path_coverage
        )
    return lsp.TypeCoverageResponse(
        covered_percent=covered_percent,
        uncovered_ranges=uncovered_expressions_diagnostics,
        default_message="Consider adding type annotations.",
    )


class AbstractDaemonQuerier(abc.ABC):
    @abc.abstractmethod
    async def get_type_errors(
        self,
        paths: Iterable[Path],
    ) -> Union[DaemonQueryFailure, Dict[Path, List[error.Error]]]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_type_coverage(
        self,
        path: Path,
    ) -> Union[DaemonQueryFailure, Optional[lsp.TypeCoverageResponse]]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def handle_file_opened(
        self,
        path: Path,
        code: str,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def handle_file_closed(
        self,
        path: Path,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def update_overlay(
        self,
        path: Path,
        code: str,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def handle_register_client(
        self,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def handle_dispose_client(
        self,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        raise NotImplementedError()


class ServerStateBackedDaemonQuerier(AbstractDaemonQuerier):
    def __init__(
        self,
        server_state: state.ServerState,
    ) -> None:
        self.server_state = server_state

    def get_socket_path(self) -> Path:
        return self.server_state.server_options.get_socket_path()

    def is_unsaved_changes_enabled(self) -> bool:
        return self.server_state.server_options.language_server_features.unsaved_changes.is_enabled()


# TODO(T184611575) Clean up dead code in PersistentDaemonQuerier
class PersistentDaemonQuerier(ServerStateBackedDaemonQuerier):
    async def _query_modules_of_path(
        self,
        path: Path,
    ) -> Union[DaemonQueryFailure, QueryModulesOfPathResponse]:
        overlay_id = str(path) if self.is_unsaved_changes_enabled() else None
        return await daemon_query.attempt_typed_async_query(
            response_type=QueryModulesOfPathResponse,
            socket_path=self.get_socket_path(),
            query_text=f"modules_of_path('{path}')",
            overlay_id=overlay_id,
        )

    async def _query_is_typechecked(
        self,
        path: Path,
    ) -> Optional[bool]:
        response = await self._query_modules_of_path(
            path,
        )
        if isinstance(response, DaemonQueryFailure):
            return None
        else:
            return len(response.response) > 0

    async def get_type_errors(
        self,
        paths: Iterable[Path],
    ) -> Union[DaemonQueryFailure, Dict[Path, List[error.Error]]]:
        errors: Dict[Path, List[error.Error]] = {}
        for path in paths:
            overlay_id = self._get_overlay_id(path)
            if overlay_id is None:
                return DaemonQueryFailure(
                    "Invalid attempt to run a get_type_errors overlay request"
                    "in a language server without unsaved changes support."
                )
            result = await daemon_query.attempt_async_overlay_type_errors(
                socket_path=self.get_socket_path(),
                source_code_path=path,
                overlay_id=overlay_id,
            )
            if isinstance(result, DaemonQueryFailure):
                return result
            errors[path] = result
        return errors

    async def get_type_coverage(
        self,
        path: Path,
    ) -> Union[DaemonQueryFailure, Optional[lsp.TypeCoverageResponse]]:
        is_typechecked = await self._query_is_typechecked(path)
        if is_typechecked is None:
            return None
        elif not is_typechecked:
            return file_not_typechecked_coverage_result()
        strict_by_default = self.server_state.server_options.strict_default
        response = await daemon_query.attempt_async_query(
            socket_path=self.get_socket_path(),
            query_text=f"expression_level_coverage('{path}')",
        )
        if isinstance(response, DaemonQueryFailure):
            return response
        else:
            return path_to_expression_coverage_response(
                strict_by_default,
                expression_level_coverage._make_expression_level_coverage_response(
                    response.payload
                ),
            )

    async def handle_file_opened(
        self,
        path: Path,
        code: str,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return "Ok"

    async def handle_file_closed(
        self,
        path: Path,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return "Ok"

    async def update_overlay(
        self,
        path: Path,
        code: str,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        source_path = f"{path}"
        overlay_update_json = [
            "OverlayUpdate",
            {
                "overlay_id": self._get_overlay_id(path),
                "source_path": source_path,
                "code_update": ["NewCode", code],
            },
        ]
        # Response is only used in the event that it is a DaemonConnectionFailure
        daemon_response = await daemon_connection.attempt_send_async_raw_request(
            socket_path=self.get_socket_path(),
            request=json.dumps(overlay_update_json),
        )
        return daemon_response

    async def handle_register_client(
        self,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return "Ok"

    async def handle_dispose_client(
        self,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return "Ok"

    def _get_overlay_id(self, path: Path) -> Optional[str]:
        unsaved_changes_enabled = self.is_unsaved_changes_enabled()
        return f"{path}, pid_{os.getpid()}" if unsaved_changes_enabled else None
