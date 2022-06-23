# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import json
import logging
import os
from pathlib import Path
from typing import Iterable, List, Optional

from .. import (
    command_arguments,
    configuration as configuration_module,
    coverage_collector as collector,
    log,
)
from . import commands, frontend_configuration, statistics

LOG: logging.Logger = logging.getLogger(__name__)


def to_absolute_path(given: str, working_directory: Path) -> Path:
    path = Path(given)
    return path if path.is_absolute() else working_directory / path


def find_root_path(local_root: Optional[Path], working_directory: Path) -> Path:
    return local_root if local_root is not None else working_directory


def collect_coverage_for_path(
    path: Path, working_directory: str, strict_default: bool
) -> Optional[collector.FileCoverage]:
    module = statistics.parse_path_to_module(path)
    relative_path = os.path.relpath(str(path), working_directory)
    return (
        collector.collect_coverage_for_module(relative_path, module, strict_default)
        if module is not None
        else None
    )


def collect_coverage_for_paths(
    paths: Iterable[Path], working_directory: str, strict_default: bool
) -> List[collector.FileCoverage]:
    result: List[collector.FileCoverage] = []
    for path in paths:
        coverage = collect_coverage_for_path(path, working_directory, strict_default)
        if coverage is not None:
            result.append(coverage)
    return result


def _print_summary(data: List[collector.FileCoverage]) -> None:
    for file_data in data:
        path = file_data.filepath
        covered_lines = len(file_data.covered_lines)
        uncovered_lines = len(file_data.uncovered_lines)
        total_lines = covered_lines + uncovered_lines
        if total_lines > 0:
            coverage_rate = round(
                covered_lines * 100.0 / (covered_lines + uncovered_lines), 2
            )
        else:
            coverage_rate = 100.00
        LOG.warning(f"{path}: {coverage_rate}% lines are type checked")


def get_module_paths(
    configuration: frontend_configuration.Base,
    working_directory: str,
    paths: Iterable[str],
) -> Iterable[Path]:
    working_directory_path = Path(working_directory)
    if paths:
        absolute_paths = [
            to_absolute_path(path, working_directory_path) for path in paths
        ]
    else:
        absolute_paths = [
            find_root_path(configuration.get_local_root(), working_directory_path)
        ]
    return statistics.find_paths_to_parse(
        absolute_paths, excludes=configuration.get_excludes()
    )


def run_coverage(
    configuration: frontend_configuration.Base,
    working_directory: str,
    paths: List[str],
    print_summary: bool,
) -> commands.ExitCode:
    module_paths = get_module_paths(
        configuration,
        working_directory,
        paths,
    )
    data = collect_coverage_for_paths(
        module_paths, working_directory, strict_default=configuration.is_strict()
    )
    if print_summary:
        _print_summary(data)
        return commands.ExitCode.SUCCESS

    log.stdout.write(json.dumps([dataclasses.asdict(entry) for entry in data]))
    return commands.ExitCode.SUCCESS


def run(
    configuration: configuration_module.Configuration,
    coverage_arguments: command_arguments.CoverageArguments,
) -> commands.ExitCode:
    return run_coverage(
        frontend_configuration.OpenSource(configuration),
        coverage_arguments.working_directory,
        coverage_arguments.paths,
        coverage_arguments.print_summary,
    )
