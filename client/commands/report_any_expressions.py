# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module provides the entrypoint for `pyre report-any-expressions`, a command
to collect data about how often the Pyre type check for an expression will have
a safety gap

There are two main causes of safety gaps:
- `Any` appearing in the inferred type for the expression itself, e.g.
  due to a missing return type on some function we called.
- The expression being passed to a function whose parameters lack annotations.

"""
from __future__ import annotations

import dataclasses
import json
import logging
import tempfile
from pathlib import Path
from typing import List, Optional, Sequence, Union

from .. import (
    coverage_data,
    daemon_socket,
    dataclasses_json_extensions as json_mixins,
    frontend_configuration,
    identifiers,
    log,
)
from . import commands, daemon_query, expression_level_coverage, query_response

LOG: logging.Logger = logging.getLogger(__name__)


def relative_path(
    backend_absolute_path: str,
    root_path: Path,
) -> str:
    return str(Path(backend_absolute_path).relative_to(root_path))


@dataclasses.dataclass(frozen=True)
class AnyExpression(json_mixins.SnakeCaseAndExcludeJsonMixin):
    expression_type: str
    reasons: List[str]
    root_cause_function_name: Optional[str]
    location: coverage_data.Location

    @staticmethod
    def from_typed_backend_data(
        data: expression_level_coverage.CoverageGap,
    ) -> AnyExpression:
        return AnyExpression(
            expression_type=data.type_,
            reasons=data.reason,
            root_cause_function_name=data.function_name,
            location=coverage_data.Location(
                start_line=data.location.start.line,
                start_column=data.location.start.column,
                end_line=data.location.stop.line,
                end_column=data.location.stop.column,
            ),
        )


@dataclasses.dataclass(frozen=True)
class ExpressionStatistics(json_mixins.SnakeCaseAndExcludeJsonMixin):
    any_expression_count: int
    total_expression_count: int
    # Records cases where the backend couldn't process the module.
    error: Optional[str] = None

    @staticmethod
    def from_error(
        error: str,
    ) -> ExpressionStatistics:
        return ExpressionStatistics(
            any_expression_count=0,
            total_expression_count=0,
            error=error,
        )

    @staticmethod
    def from_coverage_at_path(
        coverage_at_path: expression_level_coverage.CoverageAtPath,
    ) -> ExpressionStatistics:
        return ExpressionStatistics(
            any_expression_count=len(coverage_at_path.coverage_gaps),
            total_expression_count=coverage_at_path.total_expressions,
        )


@dataclasses.dataclass(frozen=True)
class ModuleExpressionData(json_mixins.SnakeCaseAndExcludeJsonMixin):
    path: str
    expression_statistics: ExpressionStatistics
    any_expressions: List[AnyExpression]

    @staticmethod
    def from_typed_backend_data(
        data: Union[
            expression_level_coverage.CoverageAtPathResponse,
            expression_level_coverage.ErrorAtPathResponse,
        ],
        root_path: Path,
    ) -> ModuleExpressionData:
        if isinstance(data, expression_level_coverage.CoverageAtPathResponse):
            coverage_at_path = data.CoverageAtPath
            return ModuleExpressionData(
                path=relative_path(coverage_at_path.path, root_path),
                any_expressions=[
                    AnyExpression.from_typed_backend_data(coverage_gap)
                    for coverage_gap in coverage_at_path.coverage_gaps
                ],
                expression_statistics=ExpressionStatistics.from_coverage_at_path(
                    coverage_at_path
                ),
            )
        else:
            error_at_path = data.ErrorAtPath
            return ModuleExpressionData(
                path=relative_path(error_at_path.path, root_path),
                any_expressions=[],
                expression_statistics=ExpressionStatistics.from_error(
                    error_at_path.error
                ),
            )


def get_module_paths(
    configuration: frontend_configuration.Base,
    paths: Optional[List[Path]],
) -> List[Path]:
    if paths is None:
        paths = [
            configuration.get_local_root() or configuration.get_global_root(),
        ]
    return list(
        coverage_data.find_module_paths(
            paths=paths,
            excludes=configuration.get_excludes(),
        )
    )


def print_data_as_json(data: Sequence[ModuleExpressionData]) -> None:
    raw_data = [module_data.to_dict() for module_data in data]
    json.dump(raw_data, log.stdout)


def query_backend(
    configuration: frontend_configuration.Base,
    paths: Optional[List[Path]],
) -> query_response.Response:
    socket_path = daemon_socket.get_socket_path(
        configuration.get_project_identifier(),
        flavor=identifiers.PyreFlavor.CLASSIC,
    )
    module_paths = get_module_paths(
        configuration=configuration,
        paths=paths,
    )
    with tempfile.NamedTemporaryFile("w") as paths_file:
        paths_file.write("\n".join(str(path) for path in module_paths))
        paths_file.flush()
        query_string = f'expression_level_coverage("@{paths_file.name}")'
        return daemon_query.execute_query(socket_path, query_string)


def run(
    configuration: frontend_configuration.Base,
    paths: Optional[List[Path]],
) -> int:
    raw_response = query_backend(
        configuration=configuration,
        paths=paths,
    )
    typed_response = expression_level_coverage._make_expression_level_coverage_response(
        raw_response.payload
    )
    project_root = configuration.get_local_root() or configuration.get_global_root()
    report = [
        ModuleExpressionData.from_typed_backend_data(
            path_response,
            project_root,
        )
        for path_response in typed_response.response
    ]
    print_data_as_json(report)
    return commands.ExitCode.SUCCESS
