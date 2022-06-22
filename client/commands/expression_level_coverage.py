# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
from dataclasses import dataclass
from typing import List, Tuple

from dataclasses_json import dataclass_json


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
class ExpressionLevelCoverage:
    path: str
    total_expressions: int
    coverage_gaps: List[CoverageGap]


@dataclass_json
@dataclass(frozen=True)
class ExpressionLevelCoverageResponse:
    response: List[ExpressionLevelCoverage]


class ErrorParsingFailure(Exception):
    pass


def _get_expression_level_coverage_response(
    response: object,
) -> List[ExpressionLevelCoverage]:
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
    coverage: ExpressionLevelCoverage,
) -> Tuple[int, int]:
    return coverage.total_expressions, len(coverage.coverage_gaps)


def summary_expression_level(response: object) -> str:
    percent_output = ""
    overall_total_expressions = 0
    overall_uncovered_expressions = 0
    for expression_level_coverage in _get_expression_level_coverage_response(response):
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
