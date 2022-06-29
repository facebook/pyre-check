# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Tuple, Union

from dataclasses_json import dataclass_json

from .. import configuration as configuration_module, log
from . import commands, coverage, frontend_configuration, query, server_connection

LOG: logging.Logger = logging.getLogger(__name__)


@dataclass_json
@dataclass(frozen=True)
class Pair:
    line: int
    column: int


@dataclass_json
@dataclass(frozen=True)
class Location:
    start: Pair
    stop: Pair


@dataclass_json
@dataclass(frozen=True)
class CoverageGap:
    location: Location
    type_: str
    reason: List[str]


@dataclass_json
@dataclass(frozen=True)
class CoverageAtPath:
    path: str
    total_expressions: int
    coverage_gaps: List[CoverageGap]


@dataclass_json
@dataclass(frozen=True)
class CoverageAtPathResponse:
    CoverageAtPath: CoverageAtPath


@dataclass_json
@dataclass(frozen=True)
class ErrorAtPath:
    path: str
    error: str


@dataclass_json
@dataclass(frozen=True)
class ErrorAtPathReponse:
    ErrorAtPath: ErrorAtPath


@dataclass_json
@dataclass(frozen=True)
class ExpressionLevelCoverageResponse:
    response: List[Union[CoverageAtPathResponse, ErrorAtPathReponse]]


class ErrorParsingFailure(Exception):
    pass


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


def _get_expression_level_coverage_response(
    response: object,
) -> List[object]:
    try:
        # pyre-ignore[16]: Pyre doesn't understand dataclasses_json
        return ExpressionLevelCoverageResponse.from_dict(response).response
    except AssertionError as error:
        raise ErrorParsingFailure(f"Error: {error}") from error
    except AttributeError as error:
        raise ErrorParsingFailure(f"Error: {error}") from error
    except KeyError as error:
        raise ErrorParsingFailure(f"Error: {error}") from error
    except TypeError as error:
        raise ErrorParsingFailure(f"Error: {error}") from error


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


def summary_expression_level(response: object) -> str:
    percent_output = ""
    overall_total_expressions = 0
    overall_uncovered_expressions = 0
    for expression_level_coverage_response in _get_expression_level_coverage_response(
        response
    ):
        # pyre-ignore[16]: Pyre doesn't understand dataclasses_json
        if "error" in expression_level_coverage_response[1]:
            continue
        # pyre-ignore[16]: Pyre doesn't understand dataclasses_json
        expression_level_coverage = CoverageAtPath.from_dict(
            expression_level_coverage_response[1]
        )
        total_expressions, uncovered_expressions = _get_total_and_uncovered_expressions(
            expression_level_coverage
        )
        overall_total_expressions += total_expressions
        overall_uncovered_expressions += uncovered_expressions
        percent_covered = _calculate_percent_covered(
            uncovered_expressions, total_expressions
        )
        percent_output += f"{expression_level_coverage.path}: {percent_covered}% expressions are covered\n"
    percent_covered = _calculate_percent_covered(
        overall_uncovered_expressions, overall_total_expressions
    )
    percent_output += f"Overall: {percent_covered}% expressions are covered"
    return percent_output


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
