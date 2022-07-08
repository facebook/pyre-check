# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Tuple, Union

import dataclasses_json

from .. import configuration as configuration_module, log
from . import (
    commands,
    coverage,
    frontend_configuration,
    language_server_protocol as lsp,
    query,
    server_connection,
)

LOG: logging.Logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class Pair(dataclasses_json.DataClassJsonMixin):
    line: int
    column: int


@dataclass(frozen=True)
class Location(dataclasses_json.DataClassJsonMixin):
    start: Pair
    stop: Pair


@dataclass(frozen=True)
class CoverageGap(dataclasses_json.DataClassJsonMixin):
    location: Location
    type_: str
    reason: List[str]


@dataclass(frozen=True)
class CoverageAtPath(dataclasses_json.DataClassJsonMixin):
    path: str
    total_expressions: int
    coverage_gaps: List[CoverageGap]


@dataclass(frozen=True)
class CoverageAtPathResponse(dataclasses_json.DataClassJsonMixin):
    CoverageAtPath: CoverageAtPath


@dataclass(frozen=True)
class ErrorAtPath(dataclasses_json.DataClassJsonMixin):
    path: str
    error: str


@dataclass(frozen=True)
class ErrorAtPathResponse(dataclasses_json.DataClassJsonMixin):
    ErrorAtPath: ErrorAtPath


@dataclass(frozen=True)
class ExpressionLevelCoverageResponse(dataclasses_json.DataClassJsonMixin):
    response: List[Union[CoverageAtPathResponse, ErrorAtPathResponse]]


class ErrorParsingFailure(Exception):
    pass


def _make_expression_level_coverage_response(
    json: object,
) -> ExpressionLevelCoverageResponse:
    def parse_path_response(
        path: List[object],
    ) -> Union[CoverageAtPathResponse, ErrorAtPathResponse]:
        if path[0] == "CoverageAtPath":
            return CoverageAtPathResponse(
                # pyre-ignore[6]: For 1st param expected `Union[None,
                #  List[typing.Any], Dict[typing.Any, typing.Any], bool, float, int,
                #  str]` but got `object`.
                CoverageAtPath=CoverageAtPath.from_dict(path[1])
            )
        else:
            # pyre-ignore[6]: For 1st param expected `Union[None, List[typing.Any],
            #  Dict[typing.Any, typing.Any], bool, float, int, str]` but got `object`.
            return ErrorAtPathResponse(ErrorAtPath=ErrorAtPath.from_dict(path[1]))

    try:
        if not isinstance(json, dict):
            raise ErrorParsingFailure(f"Error: expect a dictionary JSON but got {json}")
        response = [parse_path_response(path) for path in json["response"]]
        return ExpressionLevelCoverageResponse(response=response)
    except (AssertionError, AttributeError, KeyError, TypeError) as error:
        raise ErrorParsingFailure(f"Error: {error}") from error


def get_path_list(
    configuration: frontend_configuration.Base,
    working_directory: str,
    paths: Iterable[str],
) -> List[str]:
    working_directory_path = Path(working_directory)
    path_files = [file for file in paths if file[0] != "@"]
    absolute_path_files_string = []
    if not paths or path_files:
        absolute_path_files = coverage.get_module_paths(
            configuration,
            working_directory,
            path_files,
        )
        absolute_path_files_string = [
            str(file.resolve()) for file in absolute_path_files
        ]
    text_files = [file[1:] for file in paths if file[0] == "@"]
    absolute_text_files_string = []
    if text_files:
        absolute_text_files = [
            coverage.to_absolute_path(path, working_directory_path)
            for path in text_files
        ]
        absolute_text_files_string = ["@" + str(file) for file in absolute_text_files]
    return absolute_path_files_string + absolute_text_files_string


def _calculate_percent_covered(
    uncovered_expressions: int, total_expressions: int
) -> float:
    if total_expressions == 0:
        return 100.0
    return round(
        (total_expressions - uncovered_expressions) / total_expressions * 100, 2
    )


def _get_total_and_uncovered_expressions(
    coverage: CoverageAtPath,
) -> Tuple[int, int]:
    return coverage.total_expressions, len(coverage.coverage_gaps)


def get_percent_covered_per_path(path_response: CoverageAtPathResponse) -> float:
    total_expressions, uncovered_expressions = _get_total_and_uncovered_expressions(
        path_response.CoverageAtPath
    )
    return _calculate_percent_covered(uncovered_expressions, total_expressions)


def summary_expression_level(response: object) -> str:
    percent_output = ""
    overall_total_expressions = 0
    overall_uncovered_expressions = 0
    for path_response in _make_expression_level_coverage_response(response).response:
        if isinstance(path_response, ErrorAtPathResponse):
            continue
        expression_level_coverage = path_response.CoverageAtPath
        total_expressions, uncovered_expressions = _get_total_and_uncovered_expressions(
            expression_level_coverage
        )
        overall_total_expressions += total_expressions
        overall_uncovered_expressions += uncovered_expressions
        percent_covered = get_percent_covered_per_path(path_response)
        percent_output += f"{expression_level_coverage.path}: {percent_covered}% expressions are covered\n"
    percent_covered = _calculate_percent_covered(
        overall_uncovered_expressions, overall_total_expressions
    )
    percent_output += f"Overall: {percent_covered}% expressions are covered"
    return percent_output


def location_to_range(location: Location) -> lsp.Range:
    return lsp.Range(
        start=lsp.Position(
            line=location.start.line - 1, character=location.start.column
        ),
        end=lsp.Position(line=location.stop.line - 1, character=location.stop.column),
    )


def make_diagnostic_for_coverage_gap(coverage_gap: CoverageGap) -> lsp.Diagnostic:
    range = location_to_range(coverage_gap.location)
    return lsp.Diagnostic(range=range, message=coverage_gap.reason[0])


def get_uncovered_expression_diagnostics(
    expression_coverage: ExpressionLevelCoverageResponse,
) -> List[lsp.Diagnostic]:
    if not isinstance(expression_coverage.response[0], CoverageAtPathResponse):
        return []
    # pyre-ignore[16]: Pyre doesn't understand Union of dataclasses_json within dataclasses_json
    coverage_gaps = expression_coverage.response[0].CoverageAtPath.coverage_gaps
    return [
        make_diagnostic_for_coverage_gap(coverage_gap) for coverage_gap in coverage_gaps
    ]


def run_query(
    configuration: frontend_configuration.Base,
    query_text: str,
    print_summary: bool = False,
) -> commands.ExitCode:
    socket_path = server_connection.get_default_socket_path(
        project_root=configuration.get_global_root(),
        relative_local_root=configuration.get_relative_local_root(),
    )
    try:
        response = query.query_server(socket_path, query_text)
        if not print_summary:
            log.stdout.write(json.dumps(response.payload))
        else:
            LOG.warning(summary_expression_level(response.payload))
        return commands.ExitCode.SUCCESS
    except server_connection.ConnectionFailure:
        LOG.warning(
            "A running Pyre server is required for queries to be responded. "
            "Please run `pyre` first to set up a server."
        )
        return commands.ExitCode.SERVER_NOT_FOUND


def run(
    configuration: configuration_module.Configuration,
    query_text: str,
    print_summary: bool = False,
) -> commands.ExitCode:
    return run_query(
        frontend_configuration.OpenSource(configuration),
        query_text,
        print_summary,
    )
