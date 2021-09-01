# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import dataclasses
import json
import logging
import os
from pathlib import Path
from typing import List, Mapping, Optional, Set

import libcst as cst
from libcst._exceptions import ParserSyntaxError
from libcst.metadata import MetadataWrapper

from .. import command_arguments
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from ..coverage_collector import collect_coverage_for_module
from .command import Command

LOG: logging.Logger = logging.getLogger(__name__)


def _get_paths(target_directory: Path) -> List[Path]:
    return [
        path
        for path in target_directory.glob("**/*.py")
        if not path.name.startswith("__") and not path.name.startswith(".")
    ]


def parse_path_to_module(path: Path) -> Optional[cst.Module]:
    try:
        return cst.parse_module(path.read_text())
    except (ParserSyntaxError, FileNotFoundError):
        return None


def parse_path_to_metadata_module(path: Path) -> Optional[MetadataWrapper]:
    module = parse_path_to_module(path)
    if module is not None:
        return MetadataWrapper(module)


def _parse_paths(paths: List[Path]) -> List[Path]:
    parsed_paths = []
    for path in paths:
        if path.is_dir():
            parsed_directory_paths = _get_paths(path)
            for path in parsed_directory_paths:
                parsed_paths.append(path)
        else:
            parsed_paths.append(path)
    return parsed_paths


def _pyre_configuration_directory(local_configuration: Optional[str]) -> Path:
    if local_configuration:
        return Path(local_configuration.replace(".pyre_configuration.local", ""))
    return Path.cwd()


def _find_paths(local_configuration: Optional[str], paths: Set[str]) -> List[Path]:
    pyre_configuration_directory = _pyre_configuration_directory(local_configuration)
    if paths:
        return [Path(path).absolute() for path in paths]
    return [pyre_configuration_directory]


@dataclasses.dataclass(frozen=True)
class FileCoverage:
    filepath: str
    covered_lines: List[int]
    uncovered_lines: List[int]


def _collect_coverage(modules: Mapping[str, cst.Module]) -> List[FileCoverage]:
    coverage = []
    for path, module in modules.items():
        coverage.append(collect_coverage_for_module(path, module))
    return coverage


class Coverage(Command):
    """Collect per-line type coverage."""

    NAME = "coverage"

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        original_directory: str,
        *,
        configuration: Configuration,
        analysis_directory: Optional[AnalysisDirectory] = None,
        working_directory: Optional[str],
    ) -> None:
        super(Coverage, self).__init__(
            command_arguments, original_directory, configuration, analysis_directory
        )
        self._working_directory: Optional[str] = working_directory

    def _run(self) -> None:
        paths = self._find_paths()
        modules = {}
        for path in _parse_paths(paths):
            module = parse_path_to_module(path)
            relative_path = os.path.relpath(path, self._working_directory)
            if module is not None:
                modules[relative_path] = module
        coverage = _collect_coverage(modules)
        print(json.dumps(list(map(dataclasses.asdict, coverage))))

    def _find_paths(self) -> List[Path]:
        return _find_paths(self._configuration.local_root, set())
