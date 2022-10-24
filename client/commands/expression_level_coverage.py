# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import json
import logging
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple, Union

import dataclasses_json

from .. import (
    configuration as configuration_module,
    identifiers,
    log,
    statistics_logger,
)
from . import (
    commands,
    connections,
    coverage,
    daemon_query,
    daemon_socket,
    frontend_configuration,
    language_server_protocol as lsp,
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
    function_name: Optional[str]
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


def location_to_range(location: Location) -> lsp.LspRange:
    return lsp.LspRange(
        start=lsp.LspPosition(
            line=location.start.line - 1, character=location.start.column
        ),
        end=lsp.LspPosition(
            line=location.stop.line - 1, character=location.stop.column
        ),
    )


def make_diagnostic_for_coverage_gap(coverage_gap: CoverageGap) -> lsp.Diagnostic:
    range = location_to_range(coverage_gap.location)
    return lsp.Diagnostic(range=range, message=coverage_gap.reason[0])


def get_uncovered_expression_diagnostics(
    expression_level_coverage: ExpressionLevelCoverageResponse,
) -> List[lsp.Diagnostic]:
    if not isinstance(expression_level_coverage.response[0], CoverageAtPathResponse):
        return []
    # pyre-ignore[16]: Pyre doesn't understand Union of dataclasses_json within dataclasses_json
    coverage_gaps = expression_level_coverage.response[0].CoverageAtPath.coverage_gaps
    return [
        make_diagnostic_for_coverage_gap(coverage_gap) for coverage_gap in coverage_gaps
    ]


def _log_expression_level_coverage_to_remote(
    configuration: frontend_configuration.Base,
    response: object,
) -> None:
    logger = configuration.get_remote_logger()
    if logger is None:
        return
    run_id = str(time.time_ns())
    for path_response in _make_expression_level_coverage_response(response).response:
        if isinstance(path_response, ErrorAtPathResponse):
            continue
        expression_level_coverage = path_response.CoverageAtPath
        _log_number_expression_level_coverage(
            configuration, logger, run_id, expression_level_coverage
        )
        unannotated_functions = {}
        for coverage_gap in expression_level_coverage.coverage_gaps:
            function_name = coverage_gap.function_name
            if function_name is None:
                continue
            if function_name not in unannotated_functions:
                unannotated_functions[function_name] = 0
            unannotated_functions[function_name] += 1
        _log_unannotated_functions(
            configuration,
            logger,
            run_id,
            expression_level_coverage,
            unannotated_functions,
        )


def _log_number_expression_level_coverage(
    configuration: frontend_configuration.Base,
    logger: str,
    run_id: str,
    expression_level_coverage: CoverageAtPath,
) -> None:
    total_expressions = expression_level_coverage.total_expressions
    covered_expressions = total_expressions - len(
        expression_level_coverage.coverage_gaps
    )
    statistics_logger.log(
        category=statistics_logger.LoggerCategory.EXPRESSION_LEVEL_COVERAGE,
        logger=logger,
        integers={
            "total_expressions": total_expressions,
            "covered_expressions": covered_expressions,
        },
        normals={
            "run_id": run_id,
            "project_root": str(configuration.get_global_root()),
            "root": configuration.get_relative_local_root(),
            "path": expression_level_coverage.path,
            "binary": configuration.get_binary_version(),
        },
    )


def _log_unannotated_functions(
    configuration: frontend_configuration.Base,
    logger: str,
    run_id: str,
    expression_level_coverage: CoverageAtPath,
    unannotated_functions: Dict[str, int],
) -> None:
    for function_name, count in unannotated_functions.items():
        statistics_logger.log(
            category=statistics_logger.LoggerCategory.UNANNOTATED_FUNCTIONS,
            logger=logger,
            integers={
                "count": count,
            },
            normals={
                "function_name": function_name,
                "run_id": run_id,
                "project_root": str(configuration.get_global_root()),
                "root": configuration.get_relative_local_root(),
                "path": expression_level_coverage.path,
                "binary": configuration.get_binary_version(),
            },
        )


def run_query(
    configuration: frontend_configuration.Base,
    query_text: str,
    print_summary: bool = False,
) -> commands.ExitCode:
    socket_path = daemon_socket.get_socket_path(
        configuration.get_project_identifier(),
        flavor=identifiers.PyreFlavor.CLASSIC,
    )
    try:
        response = daemon_query.execute_query(socket_path, query_text)
        _log_expression_level_coverage_to_remote(configuration, response.payload)
        if not print_summary:
            log.stdout.write(json.dumps(response.payload))
        else:
            LOG.warning(summary_expression_level(response.payload))
        return commands.ExitCode.SUCCESS
    except connections.ConnectionFailure:
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
