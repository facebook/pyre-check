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
import enum
import json
import logging
import os
from pathlib import Path
from typing import Dict, List, Optional, Union

from .. import dataclasses_json_extensions as json_mixins, error

from ..language_server import (
    code_navigation_request,
    daemon_connection,
    features,
    protocol as lsp,
    remote_index,
)
from . import (
    daemon_query,
    expression_level_coverage,
    libcst_util,
    server_state as state,
)

LOG: logging.Logger = logging.getLogger(__name__)


class DaemonQuerierSource(str, enum.Enum):
    PYRE_DAEMON: str = "PYRE_DAEMON"
    GLEAN_INDEXER: str = "GLEAN_INDEXER"


@dataclasses.dataclass(frozen=True)
class HoverResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: lsp.PyreHoverResponse


@dataclasses.dataclass(frozen=True)
class DefinitionLocationResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: List[lsp.DefinitionResponse]


@dataclasses.dataclass(frozen=True)
class DocumentSymbolsResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: Optional[List[lsp.DocumentSymbol]]


@dataclasses.dataclass(frozen=True)
class ReferencesResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: List[lsp.ReferencesResponse]


@dataclasses.dataclass(frozen=True)
class QueryModulesOfPathResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: List[str]


@dataclasses.dataclass(frozen=True)
class GetDefinitionLocationsResponse:
    source: DaemonQuerierSource
    data: List[lsp.LspLocation]


@dataclasses.dataclass(frozen=True)
class GetHoverResponse:
    source: DaemonQuerierSource
    data: Optional[lsp.LspHoverResponse]


@dataclasses.dataclass(frozen=True)
class GetSymbolSearchResponse:
    source: DaemonQuerierSource
    data: Optional[lsp.WorkspaceSymbolResponse]


@dataclasses.dataclass(frozen=True)
class GetDocumentFormattingResponse:
    source: DaemonQuerierSource
    data: Optional[lsp.DocumentFormattingResponse]


@dataclasses.dataclass(frozen=True)
class DaemonQueryFailure(json_mixins.CamlCaseAndExcludeJsonMixin):
    error_message: str
    error_source: Optional[Exception] = None
    fallback_result: Optional[GetDefinitionLocationsResponse] = None


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


def is_server_unavailable(server_state: state.ServerState) -> bool:
    return server_state.status_tracker.get_status().connection_status in {
        state.ConnectionStatus.DISCONNECTED,
        state.ConnectionStatus.NOT_CONNECTED,
    }


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
        paths: List[Path],
    ) -> Union[DaemonQueryFailure, Dict[Path, List[error.Error]]]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_type_coverage(
        self,
        path: Path,
    ) -> Union[DaemonQueryFailure, Optional[lsp.TypeCoverageResponse]]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_hover(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, GetHoverResponse]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_document_symbols(
        self,
        path: Path,
    ) -> Union[DaemonQueryFailure, DocumentSymbolsResponse]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_symbol_search(
        self,
        query: str,
    ) -> Union[DaemonQueryFailure, GetSymbolSearchResponse]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_document_formatting(
        self, text_document: lsp.TextDocumentIdentifier
    ) -> Union[DaemonQueryFailure, GetDocumentFormattingResponse]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_definition_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, GetDefinitionLocationsResponse]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_completions(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, List[lsp.CompletionItem]]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_reference_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, List[lsp.LspLocation]]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_init_call_hierarchy(
        self,
        path: Path,
        position: lsp.PyrePosition,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> Union[DaemonQueryFailure, List[lsp.CallHierarchyItem]]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_call_hierarchy_from_item(
        self,
        path: Path,
        call_hierarchy_item: lsp.CallHierarchyItem,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> Union[DaemonQueryFailure, List[lsp.CallHierarchyItem]]:
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


class EmptyQuerier(AbstractDaemonQuerier):
    async def get_type_errors(
        self,
        paths: List[Path],
    ) -> Union[DaemonQueryFailure, Dict[Path, List[error.Error]]]:
        raise NotImplementedError()

    async def get_type_coverage(
        self,
        path: Path,
    ) -> Union[DaemonQueryFailure, Optional[lsp.TypeCoverageResponse]]:
        raise NotImplementedError()

    async def get_document_symbols(
        self,
        path: Path,
    ) -> Union[DaemonQueryFailure, DocumentSymbolsResponse]:
        raise NotImplementedError()

    async def get_symbol_search(
        self,
        query: str,
    ) -> Union[DaemonQueryFailure, GetSymbolSearchResponse]:
        raise NotImplementedError()

    async def get_document_formatting(
        self, text_document: lsp.TextDocumentIdentifier
    ) -> Union[DaemonQueryFailure, GetDocumentFormattingResponse]:
        raise NotImplementedError()

    async def get_hover(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, GetHoverResponse]:
        raise NotImplementedError()

    async def get_definition_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, GetDefinitionLocationsResponse]:
        raise NotImplementedError()

    async def get_completions(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, List[lsp.CompletionItem]]:
        raise NotImplementedError()

    async def get_reference_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, List[lsp.LspLocation]]:
        raise NotImplementedError()

    async def get_init_call_hierarchy(
        self,
        path: Path,
        position: lsp.PyrePosition,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> Union[DaemonQueryFailure, List[lsp.CallHierarchyItem]]:
        raise NotImplementedError()

    async def get_call_hierarchy_from_item(
        self,
        path: Path,
        call_hierarchy_item: lsp.CallHierarchyItem,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> Union[DaemonQueryFailure, List[lsp.CallHierarchyItem]]:
        raise NotImplementedError()

    async def handle_file_opened(
        self,
        path: Path,
        code: str,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        raise NotImplementedError()

    async def handle_file_closed(
        self,
        path: Path,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        raise NotImplementedError()

    async def update_overlay(
        self,
        path: Path,
        code: str,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        raise NotImplementedError()

    async def handle_register_client(
        self,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        raise NotImplementedError()

    async def handle_dispose_client(
        self,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        raise NotImplementedError()


class PersistentDaemonQuerier(AbstractDaemonQuerier):
    async def _query_modules_of_path(
        self,
        path: Path,
    ) -> Union[DaemonQueryFailure, QueryModulesOfPathResponse]:
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
        if isinstance(response, DaemonQueryFailure):
            return None
        else:
            return len(response.response) > 0

    async def get_type_errors(
        self,
        paths: List[Path],
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
                socket_path=self.socket_path,
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
            socket_path=self.socket_path,
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

    async def get_hover(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, GetHoverResponse]:
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
        if isinstance(daemon_response, DaemonQueryFailure):
            return daemon_response
        else:
            return GetHoverResponse(
                source=DaemonQuerierSource.PYRE_DAEMON,
                data=daemon_response.response.to_lsp_hover_response(),
            )

    async def get_definition_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, GetDefinitionLocationsResponse]:
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
        if isinstance(daemon_response, DaemonQueryFailure):
            return daemon_response
        else:
            return GetDefinitionLocationsResponse(
                source=DaemonQuerierSource.PYRE_DAEMON,
                data=[
                    response.to_lsp_definition_response()
                    for response in daemon_response.response
                ],
            )

    async def get_document_symbols(
        self,
        path: Path,
    ) -> Union[DaemonQueryFailure, DocumentSymbolsResponse]:
        return DaemonQueryFailure(
            "Document Symbol is not supported in the pyre persistent client. Please use code-navigation. "
        )

    async def get_symbol_search(
        self,
        query: str,
    ) -> Union[DaemonQueryFailure, GetSymbolSearchResponse]:
        return DaemonQueryFailure(
            "Symbol Search is not supported in the pyre persistent client. Please use code-navigation. "
        )

    async def get_document_formatting(
        self, text_document: lsp.TextDocumentIdentifier
    ) -> Union[DaemonQueryFailure, GetDocumentFormattingResponse]:
        return DaemonQueryFailure(
            "Document formatting is not supported in the pyre persistent client. Please use code-navigation. "
        )

    async def get_completions(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, List[lsp.CompletionItem]]:
        return DaemonQueryFailure(
            "Completions is not supported in the pyre persistent client. Please use code-navigation. "
        )

    async def get_reference_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, List[lsp.LspLocation]]:
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
        if isinstance(daemon_response, DaemonQueryFailure):
            return daemon_response
        else:
            result = [
                response.to_lsp_references_response()
                for response in daemon_response.response
            ]
            return result

    async def get_init_call_hierarchy(
        self,
        path: Path,
        position: lsp.PyrePosition,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> Union[DaemonQueryFailure, List[lsp.CallHierarchyItem]]:
        return DaemonQueryFailure(
            "Call hierarchy is not supported in the pyre persistent client. Please use code-navigation. "
        )

    async def get_call_hierarchy_from_item(
        self,
        path: Path,
        call_hierarchy_item: lsp.CallHierarchyItem,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> Union[DaemonQueryFailure, List[lsp.CallHierarchyItem]]:
        return DaemonQueryFailure(
            "Call hierarchy (from item) is not supported in the pyre persistent client. Please use code-navigation. "
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


class FailableDaemonQuerier(AbstractDaemonQuerier):
    """
    We may need to fail fast and return a DaemonQueryFailure instead
    of querying the Pyre server backend - by passing a AbstractDaemonQueryFailer
    to the constructor of the `FailableDaemonQuerier` we have the option to do this
    """

    def __init__(
        self,
        base_querier: AbstractDaemonQuerier,
    ) -> None:
        super().__init__(base_querier.server_state)
        self.base_querier: AbstractDaemonQuerier = base_querier

    async def get_hover(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, GetHoverResponse]:
        return await self.base_querier.get_hover(path, position)

    async def get_type_errors(
        self,
        paths: List[Path],
    ) -> Union[DaemonQueryFailure, Dict[Path, List[error.Error]]]:
        return await self.base_querier.get_type_errors(paths)

    async def get_definition_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, GetDefinitionLocationsResponse]:
        return await self.base_querier.get_definition_locations(path, position)

    async def get_completions(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, List[lsp.CompletionItem]]:
        return await self.base_querier.get_completions(path, position)

    async def get_reference_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, List[lsp.LspLocation]]:
        return await self.base_querier.get_reference_locations(path, position)

    async def get_init_call_hierarchy(
        self,
        path: Path,
        position: lsp.PyrePosition,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> Union[DaemonQueryFailure, List[lsp.CallHierarchyItem]]:
        return await self.base_querier.get_init_call_hierarchy(
            path, position, relation_direction
        )

    async def get_call_hierarchy_from_item(
        self,
        path: Path,
        call_hierarchy_item: lsp.CallHierarchyItem,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> Union[DaemonQueryFailure, List[lsp.CallHierarchyItem]]:
        return await self.base_querier.get_call_hierarchy_from_item(
            path, call_hierarchy_item, relation_direction
        )

    async def get_type_coverage(
        self,
        path: Path,
    ) -> Union[DaemonQueryFailure, Optional[lsp.TypeCoverageResponse]]:
        return await self.base_querier.get_type_coverage(path)

    async def handle_file_opened(
        self,
        path: Path,
        code: str,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return await self.base_querier.handle_file_opened(path, code)

    async def handle_file_closed(
        self,
        path: Path,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return await self.base_querier.handle_file_closed(path)

    async def update_overlay(
        self,
        path: Path,
        code: str,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return await self.base_querier.update_overlay(path, code)

    async def handle_register_client(
        self,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return await self.base_querier.handle_register_client()

    async def handle_dispose_client(
        self,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return await self.base_querier.handle_dispose_client()

    async def get_document_symbols(
        self,
        path: Path,
    ) -> Union[DaemonQueryFailure, DocumentSymbolsResponse]:
        return await self.base_querier.get_document_symbols(path)

    async def get_symbol_search(
        self,
        query: str,
    ) -> Union[DaemonQueryFailure, GetSymbolSearchResponse]:
        return await self.base_querier.get_symbol_search(query)

    async def get_document_formatting(
        self, text_document: lsp.TextDocumentIdentifier
    ) -> Union[DaemonQueryFailure, GetDocumentFormattingResponse]:
        return await self.base_querier.get_document_formatting(text_document)


class CodeNavigationDaemonQuerier(AbstractDaemonQuerier):
    async def get_type_errors(
        self,
        paths: List[Path],
    ) -> Union[DaemonQueryFailure, Dict[Path, List[error.Error]]]:
        type_errors_request = code_navigation_request.TypeErrorsRequest(
            paths=[str(path) for path in paths], client_id=self._get_client_id()
        )
        response = await code_navigation_request.async_handle_type_errors_request(
            self.socket_path, type_errors_request
        )
        if isinstance(response, code_navigation_request.ErrorResponse):
            return DaemonQueryFailure(
                response.message, error_source=response.error_source
            )
        result: Dict[Path, List[error.Error]] = {}
        for response_error in response.to_errors_response():
            if response_error.code == 0:
                continue
            file_with_error = response_error.path
            if file_with_error not in result:
                result[file_with_error] = []
            result[file_with_error].append(response_error)

        # TODO(T165048078): determine if this error should be kept for code navigation (are we committing to unsafe mode in codenav?)
        return result

    async def get_type_coverage(
        self,
        path: Path,
    ) -> Optional[lsp.TypeCoverageResponse]:
        raise NotImplementedError()

    async def get_hover(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, GetHoverResponse]:
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
            return GetHoverResponse(
                source=DaemonQuerierSource.PYRE_DAEMON,
                data=lsp.LspHoverResponse.from_pyre_hover_responses(response.contents),
            )
        return DaemonQueryFailure(response.message)

    async def get_definition_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, GetDefinitionLocationsResponse]:
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
            return DaemonQueryFailure(
                response.message, error_source=response.error_source
            )
        return GetDefinitionLocationsResponse(
            source=DaemonQuerierSource.PYRE_DAEMON,
            data=[
                definition_location.to_lsp_definition_response()
                for definition_location in response.definitions
            ],
        )

    async def get_document_symbols(
        self,
        path: Path,
    ) -> Union[DaemonQueryFailure, DocumentSymbolsResponse]:
        document_symbol_request = lsp.DocumentSymbolRequest(
            path=str(path),
            client_id=self._get_client_id(),
        )

        response = await code_navigation_request.async_handle_document_symbol_request(
            self.socket_path,
            document_symbol_request,
        )

        if isinstance(response, code_navigation_request.ErrorResponse):
            return DaemonQueryFailure(
                response.message, error_source=response.error_source
            )

        else:
            return DocumentSymbolsResponse(
                [
                    document_symbols.to_lsp_document_symbol()
                    for document_symbols in response.symbols
                ]
            )

    async def get_symbol_search(
        self,
        query: str,
    ) -> Union[DaemonQueryFailure, GetSymbolSearchResponse]:
        # Note: it is okay to return None here because there is currently no plan to support symbol search using the backend.
        raise NotImplementedError("Method not yet implemented")

    async def get_document_formatting(
        self, text_document: lsp.TextDocumentIdentifier
    ) -> Union[DaemonQueryFailure, GetDocumentFormattingResponse]:
        raise NotImplementedError("Method not yet implemented in the backend")

    async def get_completions(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, List[lsp.CompletionItem]]:
        completions_request = lsp.CompletionRequest(
            path=str(path),
            client_id=self._get_client_id(),
            position=position,
        )
        response = await code_navigation_request.async_handle_completion_request(
            self.socket_path, completions_request
        )
        if isinstance(response, code_navigation_request.ErrorResponse):
            return DaemonQueryFailure(
                response.message, error_source=response.error_source
            )
        return [
            completion_item.to_lsp_completion_item()
            for completion_item in response.completions
        ]

    async def get_reference_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, List[lsp.LspLocation]]:
        if path not in self.server_state.opened_documents.keys():
            return []
        code = self.server_state.opened_documents[path].code
        global_root = Path(
            self.server_state.server_options.start_arguments.base_arguments.global_root
        )
        return libcst_util.find_references(path, global_root, code, position)

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

    async def get_init_call_hierarchy(
        self,
        path: Path,
        position: lsp.PyrePosition,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> Union[DaemonQueryFailure, List[lsp.CallHierarchyItem]]:
        return []

    async def get_call_hierarchy_from_item(
        self,
        path: Path,
        call_hierarchy_item: lsp.CallHierarchyItem,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> Union[DaemonQueryFailure, List[lsp.CallHierarchyItem]]:
        return []

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


class RemoteIndexBackedQuerier(AbstractDaemonQuerier):
    def __init__(
        self,
        base_querier: AbstractDaemonQuerier,
        index: remote_index.AbstractRemoteIndex,
    ) -> None:
        self.base_querier: AbstractDaemonQuerier = base_querier
        self.index: remote_index.AbstractRemoteIndex = index

    async def get_type_errors(
        self,
        paths: List[Path],
    ) -> Union[DaemonQueryFailure, Dict[Path, List[error.Error]]]:
        return await self.base_querier.get_type_errors(paths)

    async def get_type_coverage(
        self,
        path: Path,
    ) -> Union[DaemonQueryFailure, Optional[lsp.TypeCoverageResponse]]:
        return await self.base_querier.get_type_coverage(path)

    async def get_hover(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, GetHoverResponse]:
        if is_server_unavailable(self.base_querier.server_state):
            index_result = await self.index.hover(path, position)
            return GetHoverResponse(
                source=DaemonQuerierSource.GLEAN_INDEXER, data=index_result
            )
        return await self.base_querier.get_hover(path, position)

    async def get_document_symbols(
        self,
        path: Path,
    ) -> Union[DaemonQueryFailure, DocumentSymbolsResponse]:
        raise NotImplementedError()

    async def get_symbol_search(
        self,
        query: str,
    ) -> Union[DaemonQueryFailure, GetSymbolSearchResponse]:
        index_result = await self.index.symbol_search(query)
        return GetSymbolSearchResponse(
            source=DaemonQuerierSource.GLEAN_INDEXER, data=index_result
        )

    async def get_document_formatting(
        self, text_document: lsp.TextDocumentIdentifier
    ) -> Union[DaemonQueryFailure, GetDocumentFormattingResponse]:
        raise NotImplementedError()

    async def get_definition_locations_from_glean(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> GetDefinitionLocationsResponse:
        indexed_result = await self.index.definition(path, position)

        return GetDefinitionLocationsResponse(
            source=(DaemonQuerierSource.GLEAN_INDEXER),
            data=indexed_result,
        )

    async def get_definition_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, GetDefinitionLocationsResponse]:
        if is_server_unavailable(self.base_querier.server_state):
            return await self.get_definition_locations_from_glean(path, position)
        base_results = await self.base_querier.get_definition_locations(path, position)

        # If pyre throws an exception and might not require restarting due to that exception  - then fall back to glean
        if (
            isinstance(base_results, DaemonQueryFailure)
            and base_results.error_source is None
        ):
            LOG.warn(
                f"Pyre threw exception: {base_results.error_message} - falling back to glean"
            )
            fallback_result = await self.get_definition_locations_from_glean(
                path, position
            )
            return DaemonQueryFailure(
                base_results.error_message, base_results.error_source, fallback_result
            )

        return base_results

    async def get_completions(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, List[lsp.CompletionItem]]:
        return await self.base_querier.get_completions(path, position)

    async def get_reference_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> Union[DaemonQueryFailure, List[lsp.LspLocation]]:
        return await self.index.references(path, position)

    async def get_init_call_hierarchy(
        self,
        path: Path,
        position: lsp.PyrePosition,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> Union[DaemonQueryFailure, List[lsp.CallHierarchyItem]]:
        return await self.index.prepare_call_hierarchy(
            path, position, relation_direction
        )

    async def get_call_hierarchy_from_item(
        self,
        path: Path,
        call_hierarchy_item: lsp.CallHierarchyItem,
        relation_direction: lsp.PyreCallHierarchyRelationDirection,
    ) -> Union[DaemonQueryFailure, List[lsp.CallHierarchyItem]]:
        return await self.index.call_hierarchy_from_item(
            path, call_hierarchy_item, relation_direction
        )

    async def handle_file_opened(
        self,
        path: Path,
        code: str,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return await self.base_querier.handle_file_opened(path, code)

    async def handle_file_closed(
        self,
        path: Path,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return await self.base_querier.handle_file_closed(path)

    async def update_overlay(
        self,
        path: Path,
        code: str,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return await self.base_querier.update_overlay(path, code)

    async def handle_register_client(
        self,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return await self.base_querier.handle_register_client()

    async def handle_dispose_client(
        self,
    ) -> Union[daemon_connection.DaemonConnectionFailure, str]:
        return await self.base_querier.handle_dispose_client()
