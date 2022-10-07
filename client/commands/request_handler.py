# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import abc
import dataclasses
import json
import logging
from pathlib import Path
from typing import List, Optional

from libcst.metadata import CodeRange

from .. import dataclasses_json_extensions as json_mixins

from ..coverage_collector import coverage_collector_for_module, CoveredAndUncoveredLines

from . import (
    daemon_connection,
    daemon_query,
    expression_level_coverage,
    language_server_features as features,
    language_server_protocol as lsp,
    server_state as state,
    statistics,
)

LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class HoverResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: lsp.LspHoverResponse


@dataclasses.dataclass(frozen=True)
class DefinitionLocationResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: List[lsp.PyreDefinitionResponse]


@dataclasses.dataclass(frozen=True)
class ReferencesResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: List[lsp.ReferencesResponse]


@dataclasses.dataclass(frozen=True)
class QueryModulesOfPathResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: List[str]


def uncovered_range_to_diagnostic(uncovered_range: CodeRange) -> lsp.Diagnostic:
    return lsp.Diagnostic(
        range=lsp.LspRange(
            start=lsp.LspPosition(
                line=uncovered_range.start.line - 1,
                character=uncovered_range.start.column,
            ),
            end=lsp.LspPosition(
                line=uncovered_range.end.line - 1, character=uncovered_range.end.column
            ),
        ),
        message=(
            "This function is not type checked. "
            "Consider adding parameter or return type annotations."
        ),
    )


def to_coverage_result(
    covered_and_uncovered_lines: CoveredAndUncoveredLines,
    uncovered_ranges: List[CodeRange],
) -> lsp.TypeCoverageResponse:
    num_covered = len(covered_and_uncovered_lines.covered_lines)
    num_uncovered = len(covered_and_uncovered_lines.uncovered_lines)
    num_total = num_covered + num_uncovered
    if num_total == 0:
        return lsp.TypeCoverageResponse(
            covered_percent=100.0, uncovered_ranges=[], default_message=""
        )
    else:
        return lsp.TypeCoverageResponse(
            covered_percent=100.0 * num_covered / num_total,
            uncovered_ranges=[
                uncovered_range_to_diagnostic(uncovered_range)
                for uncovered_range in uncovered_ranges
            ],
            default_message="Consider adding type annotations.",
        )


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


def path_to_coverage_response(
    path: Path, strict_default: bool
) -> Optional[lsp.TypeCoverageResponse]:
    module = statistics.parse_path_to_module(path)
    if module is None:
        return None

    coverage_collector = coverage_collector_for_module(
        str(path), module, strict_default
    )
    covered_and_uncovered_lines = coverage_collector.covered_and_uncovered_lines()
    uncovered_ranges = [f.code_range for f in coverage_collector.uncovered_functions()]
    return to_coverage_result(covered_and_uncovered_lines, uncovered_ranges)


def path_to_expression_coverage_response(
    strict_default: bool,
    expression_coverage: expression_level_coverage.ExpressionLevelCoverageResponse,
) -> lsp.TypeCoverageResponse:
    path_coverage = expression_coverage.response[0]
    if isinstance(path_coverage, expression_level_coverage.ErrorAtPathResponse):
        uncovered_expressions_diagnostics = []
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


class AbstractRequestHandler(abc.ABC):
    @abc.abstractmethod
    async def get_type_coverage(
        self,
        path: Path,
    ) -> Optional[lsp.TypeCoverageResponse]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_hover(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> lsp.LspHoverResponse:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_definition_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> List[lsp.LspLocation]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def get_reference_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> List[lsp.LspLocation]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def update_overlay(
        self,
        path: Path,
        code: str,
    ) -> None:
        raise NotImplementedError()


class RequestHandler(AbstractRequestHandler):
    def __init__(
        self,
        server_state: state.ServerState,
    ) -> None:
        self.server_state = server_state
        self.socket_path: Path = server_state.server_options.get_socket_path()

    def get_language_server_features(self) -> features.LanguageServerFeatures:
        return self.server_state.server_options.language_server_features

    async def _query_modules_of_path(
        self,
        path: Path,
    ) -> Optional[QueryModulesOfPathResponse]:
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
        if response is None:
            return None
        else:
            return len(response.response) > 0

    def _get_overlay_id(self, path: Path) -> Optional[str]:
        unsaved_changes_enabled = (
            self.get_language_server_features().unsaved_changes.is_enabled()
        )
        return str(path) if unsaved_changes_enabled else None

    async def get_type_coverage(
        self,
        path: Path,
    ) -> Optional[lsp.TypeCoverageResponse]:
        is_typechecked = await self._query_is_typechecked(path)
        if is_typechecked is None:
            return None
        elif not is_typechecked:
            return file_not_typechecked_coverage_result()
        type_coverage = self.get_language_server_features().type_coverage
        strict_by_default = self.server_state.server_options.strict_default
        if type_coverage == features.TypeCoverageAvailability.EXPRESSION_LEVEL:
            response = await daemon_query.attempt_async_query(
                socket_path=self.socket_path,
                query_text=f"expression_level_coverage('{path}')",
            )
            if response is None:
                return None
            else:
                return path_to_expression_coverage_response(
                    strict_by_default,
                    expression_level_coverage._make_expression_level_coverage_response(
                        response.payload
                    ),
                )
        else:
            return path_to_coverage_response(path, strict_by_default)

    async def get_hover(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> lsp.LspHoverResponse:
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
        return (
            daemon_response.response
            if daemon_response
            else lsp.LspHoverResponse.empty()
        )

    async def get_definition_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> List[lsp.LspLocation]:
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
        definitions = (
            [
                response.to_lsp_definition_response()
                for response in daemon_response.response
            ]
            if daemon_response is not None
            else []
        )
        return definitions

    async def get_reference_locations(
        self,
        path: Path,
        position: lsp.PyrePosition,
    ) -> List[lsp.LspLocation]:
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
        return (
            [
                response.to_lsp_definition_response()
                for response in daemon_response.response
            ]
            if daemon_response is not None
            else []
        )

    async def update_overlay(
        self,
        path: Path,
        code: str,
    ) -> None:
        source_path = f"{path}"
        overlay_update_json = [
            "OverlayUpdate",
            {
                # TODO: T126924773 Include a language server identifier (e.g. PID of
                # the current process) in this overlay id.
                "overlay_id": source_path,
                "source_path": source_path,
                "code_update": ["NewCode", code],
            },
        ]
        # Drop the response (the daemon code will log it for us)
        await daemon_connection.attempt_send_async_raw_request(
            socket_path=self.socket_path,
            request=json.dumps(overlay_update_json),
        )
