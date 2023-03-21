# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

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
from typing import List, Optional, Union

from .. import dataclasses_json_extensions as json_mixins, error

from ..language_server import (
    code_navigation_request,
    daemon_connection,
    features,
    protocol as lsp,
)
from . import daemon_query, expression_level_coverage, server_state as state

LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class HoverResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: lsp.PyreHoverResponse


@dataclasses.dataclass(frozen=True)
class DefinitionLocationResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: List[lsp.PyreDefinitionResponse]


@dataclasses.dataclass(frozen=True)
class ReferencesResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: List[lsp.ReferencesResponse]


@dataclasses.dataclass(frozen=True)
class QueryModulesOfPathResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: List[str]


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
    def __init__(
        self,
        server_state: state.ServerState,
    ) -> None:
        self.server_state = server_state
        self.socket_path: Path = server_state.server_options.get_socket_path()

    @abc.abstractmethod
    async def get_type_errors(
        self,
        path: Path,
    ) -> Union[daemon_query.DaemonQueryFailure, List[error.Error]]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_type_coverage(
        self,
        path: Path,
    ) -> Union[daemon_query.DaemonQueryFailure, Optional[lsp.TypeCoverageResponse]]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_hover(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[daemon_query.DaemonQueryFailure, lsp.LspHoverResponse]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_definition_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[daemon_query.DaemonQueryFailure, List[lsp.LspLocation]]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_reference_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[daemon_query.DaemonQueryFailure, List[lsp.LspLocation]]:
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

    def get_language_server_features(self) -> features.LanguageServerFeatures:
        return self.server_state.server_options.language_server_features


class PersistentDaemonQuerier(AbstractDaemonQuerier):
    async def _query_modules_of_path(
        self,
        path: Path,
    ) -> Union[daemon_query.DaemonQueryFailure, QueryModulesOfPathResponse]:
        overlay_id = (
            str(path)
            if self.get_language_server_features().unsaved_changes.is_enabled()
            else None
        )
        return await daemon_query.attempt_typed_async_query(
            response_type=QueryModulesOfPathResponse,
            socket_path=self.socket_path,
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
        if isinstance(response, daemon_query.DaemonQueryFailure):
            return None
        else:
            return len(response.response) > 0

    async def get_type_errors(
        self,
        path: Path,
    ) -> Union[daemon_query.DaemonQueryFailure, List[error.Error]]:
        overlay_id = self._get_overlay_id(path)
        if overlay_id is None:
            return daemon_query.DaemonQueryFailure(
                "Invalid attempt to run a get_type_errors overlay request"
                "in a language server without unsaved changes support."
            )
        return await daemon_query.attempt_async_overlay_type_errors(
            socket_path=self.socket_path,
            source_code_path=path,
            overlay_id=overlay_id,
        )

    async def get_type_coverage(
        self,
        path: Path,
    ) -> Union[daemon_query.DaemonQueryFailure, Optional[lsp.TypeCoverageResponse]]:
        is_typechecked = await self._query_is_typechecked(path)
        if is_typechecked is None:
            return None
        elif not is_typechecked:
            return file_not_typechecked_coverage_result()
        strict_by_default = self.server_state.server_options.strict_default
        response = await daemon_query.attempt_async_query(
            socket_path=self.socket_path,
            query_text=f"expression_level_coverage('{path}')",
        )
        if isinstance(response, daemon_query.DaemonQueryFailure):
            return response
        else:
            return path_to_expression_coverage_response(
                strict_by_default,
                expression_level_coverage._make_expression_level_coverage_response(
                    response.payload
                ),
            )

    async def get_hover(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[daemon_query.DaemonQueryFailure, lsp.LspHoverResponse]:
        path_string = f"'{path}'"
        query_text = (
            f"hover_info_for_position(path={path_string},"
            f" line={position.line}, column={position.character})"
        )
        daemon_response = await daemon_query.attempt_typed_async_query(
            response_type=HoverResponse,
            socket_path=self.socket_path,
            query_text=query_text,
            overlay_id=self._get_overlay_id(path),
        )
        if isinstance(daemon_response, daemon_query.DaemonQueryFailure):
            return daemon_response
        else:
            return daemon_response.response.to_lsp_hover_response()

    async def get_definition_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[daemon_query.DaemonQueryFailure, List[lsp.LspLocation]]:
        path_string = f"'{path}'"
        query_text = (
            f"location_of_definition(path={path_string},"
            f" line={position.line}, column={position.character})"
        )
        daemon_response = await daemon_query.attempt_typed_async_query(
            response_type=DefinitionLocationResponse,
            socket_path=self.socket_path,
            query_text=query_text,
            overlay_id=self._get_overlay_id(path),
        )
        if isinstance(daemon_response, daemon_query.DaemonQueryFailure):
            return daemon_response
        else:
            return [
                response.to_lsp_definition_response()
                for response in daemon_response.response
            ]

    async def get_reference_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[daemon_query.DaemonQueryFailure, List[lsp.LspLocation]]:
        path_string = f"'{path}'"
        query_text = (
            f"find_references(path={path_string},"
            f" line={position.line}, column={position.character})"
        )
        daemon_response = await daemon_query.attempt_typed_async_query(
            response_type=ReferencesResponse,
            socket_path=self.socket_path,
            query_text=query_text,
            overlay_id=self._get_overlay_id(path),
        )
        if isinstance(daemon_response, daemon_query.DaemonQueryFailure):
            return daemon_response
        else:
            result = [
                response.to_lsp_definition_response()
                for response in daemon_response.response
            ]
            return result

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
            socket_path=self.socket_path,
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
        unsaved_changes_enabled = (
            self.get_language_server_features().unsaved_changes.is_enabled()
        )
        return f"{path}, pid_{os.getpid()}" if unsaved_changes_enabled else None


class CodeNavigationDaemonQuerier(AbstractDaemonQuerier):
    async def get_type_errors(
        self,
        path: Path,
    ) -> Union[daemon_query.DaemonQueryFailure, List[error.Error]]:
        raise NotImplementedError()

    async def get_type_coverage(
        self,
        path: Path,
    ) -> Optional[lsp.TypeCoverageResponse]:
        raise NotImplementedError()

    async def get_hover(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[daemon_query.DaemonQueryFailure, lsp.LspHoverResponse]:
        hover_request = code_navigation_request.HoverRequest(
            path=str(path),
            client_id=self._get_client_id(),
            position=position,
        )
        response = await code_navigation_request.async_handle_hover_request(
            self.socket_path,
            hover_request,
        )
        if isinstance(response, code_navigation_request.HoverResponse):
            return lsp.LspHoverResponse(
                "\n".join(
                    [
                        hover.to_lsp_hover_response().contents
                        for hover in response.contents
                    ]
                )
            )
        return daemon_query.DaemonQueryFailure(response.message)

    async def get_definition_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[daemon_query.DaemonQueryFailure, List[lsp.LspLocation]]:
        definition_request = code_navigation_request.LocationOfDefinitionRequest(
            path=str(path),
            client_id=self._get_client_id(),
            position=position,
        )
        response = await code_navigation_request.async_handle_definition_request(
            self.socket_path,
            definition_request,
        )
        if isinstance(response, code_navigation_request.ErrorResponse):
            return daemon_query.DaemonQueryFailure(response.message)
        return [
            definition_location.to_lsp_definition_response()
            for definition_location in response.definitions
        ]

    async def get_reference_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[daemon_query.DaemonQueryFailure, List[lsp.LspLocation]]:
        raise NotImplementedError()

    async def update_overlay(
        self,
        path: Path,
        code: str,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        client_id = self._get_client_id()
        local_update = code_navigation_request.LocalUpdate(
            client_id=client_id,
            path=str(path),
            content=code,
        )
        return await code_navigation_request.async_handle_local_update(
            self.socket_path, local_update
        )

    async def handle_file_opened(
        self,
        path: Path,
        code: str,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        client_id = self._get_client_id()
        file_opened = code_navigation_request.FileOpened(
            path=path,
            client_id=client_id,
            content=code,
        )
        return await code_navigation_request.async_handle_file_opened(
            self.socket_path, file_opened
        )

    async def handle_file_closed(
        self,
        path: Path,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        client_id = self._get_client_id()
        file_closed = code_navigation_request.FileClosed(client_id=client_id, path=path)
        return await code_navigation_request.async_handle_file_closed(
            self.socket_path, file_closed
        )

    def _get_client_id(self) -> str:
        return f"codenav_pid_{os.getpid()}"

    async def handle_register_client(
        self,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        client_id = self._get_client_id()
        register_client = code_navigation_request.RegisterClient(client_id=client_id)
        return await code_navigation_request.async_handle_register_client(
            self.socket_path, register_client
        )

    async def handle_dispose_client(
        self,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        client_id = self._get_client_id()
        dispose_client = code_navigation_request.DisposeClient(client_id=client_id)
        return await code_navigation_request.async_handle_dispose_client(
            self.socket_path, dispose_client
        )
