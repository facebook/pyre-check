# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module provides the logic for computing function-level coverage in Pyre.

The coverage command produces the number of lines of code considered covered
by Pyre using function-level coverage rules: a line is uncovered if:
- the module strict (in which case all lines are considered covered)
- or, the function has a return annotation or at least one parameter
  annotation

Note that function-level coverage is entirely determined by the AST, we are
not asking about which code has strong types that Pyre can analyze but rather
which code Pyre will attempt to analyze at all based on where annotations
were provided.
"""


import dataclasses
import json
import logging
import os
from pathlib import Path
from typing import Iterable, List, Optional, Set, Tuple

import libcst as cst

from .. import command_arguments, coverage_data, frontend_configuration, log
from . import commands

LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class CoveredAndUncoveredLines:
    covered_lines: Set[int]
    uncovered_lines: Set[int]


@dataclasses.dataclass(frozen=True)
class FileCoverage:
    filepath: str
    covered_lines: List[int]
    uncovered_lines: List[int]


class CoverageCollector(coverage_data.AnnotationCollector):
    def __init__(self, is_strict: bool) -> None:
        super().__init__()
        self.is_strict = is_strict

    def covered_functions(self) -> List[coverage_data.FunctionAnnotationInfo]:
        if self.is_strict:
            return self.functions
        else:
            return [f for f in self.functions if f.is_annotated]

    def uncovered_functions(self) -> List[coverage_data.FunctionAnnotationInfo]:
        if self.is_strict:
            return []
        else:
            return [f for f in self.functions if not f.is_annotated]

    def covered_and_uncovered_lines(self) -> CoveredAndUncoveredLines:
        def num_lines(
            location_and_is_covered: Tuple[coverage_data.Location, bool]
        ) -> int:
            location, _ = location_and_is_covered
            return location.end_line - location.start_line + 1

        # When the code ranges are nested, we want to respect the innermost
        # one. By processing in descending order of number of lines we can
        # ensure that.
        uncovered_lines = set()
        for location, is_covered in sorted(
            [
                *((f.location, False) for f in self.uncovered_functions()),
                *((f.location, True) for f in self.covered_functions()),
            ],
            key=num_lines,
            reverse=True,
        ):
            if is_covered:
                uncovered_lines -= _location_to_covered_lines(location)
            else:
                uncovered_lines |= _location_to_covered_lines(location)
        covered_lines = set(range(0, self.line_count)) - uncovered_lines
        return CoveredAndUncoveredLines(covered_lines, uncovered_lines)


def _coverage_collector_for_module(
    relative_path: str, module: cst.MetadataWrapper, strict_default: bool
) -> CoverageCollector:
    strict_count_collector = coverage_data.ModuleModeCollector(strict_default)
    try:
        module.visit(strict_count_collector)
    except RecursionError:
        LOG.warning(f"LibCST encountered recursion error in `{relative_path}`")
    coverage_collector = CoverageCollector(strict_count_collector.is_strict_module())
    try:
        module.visit(coverage_collector)
    except RecursionError:
        LOG.warning(f"LibCST encountered recursion error in `{relative_path}`")
    return coverage_collector


def collect_coverage_for_module(
    relative_path: str, module: cst.MetadataWrapper, strict_default: bool
) -> FileCoverage:
    coverage_collector = _coverage_collector_for_module(
        relative_path, module, strict_default
    )
    covered_and_uncovered_lines = coverage_collector.covered_and_uncovered_lines()
    return FileCoverage(
        filepath=relative_path,
        covered_lines=sorted(covered_and_uncovered_lines.covered_lines),
        uncovered_lines=sorted(covered_and_uncovered_lines.uncovered_lines),
    )


def _location_to_covered_lines(location: coverage_data.Location) -> Set[int]:
    return set(range(location.start_line - 1, location.end_line))


def to_absolute_path(given: str, working_directory: Path) -> Path:
    path = Path(given)
    return path if path.is_absolute() else working_directory / path


def find_root_path(local_root: Optional[Path], working_directory: Path) -> Path:
    return local_root if local_root is not None else working_directory


def collect_coverage_for_path(
    path: Path, working_directory: str, strict_default: bool
) -> Optional[FileCoverage]:
    module = coverage_data.module_from_path(path)
    relative_path = os.path.relpath(str(path), working_directory)
    return (
        collect_coverage_for_module(relative_path, module, strict_default)
        if module is not None
        else None
    )


def collect_coverage_for_paths(
    paths: Iterable[Path], working_directory: str, strict_default: bool
) -> List[FileCoverage]:
    result: List[FileCoverage] = []
    for path in paths:
        coverage = collect_coverage_for_path(path, working_directory, strict_default)
        if coverage is not None:
            result.append(coverage)
    return result


def _print_summary(data: List[FileCoverage]) -> None:
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
    return coverage_data.find_module_paths(
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
    configuration: frontend_configuration.Base,
    coverage_arguments: command_arguments.CoverageArguments,
) -> commands.ExitCode:
    return run_coverage(
        configuration,
        coverage_arguments.working_directory,
        coverage_arguments.paths,
        coverage_arguments.print_summary,
    )
