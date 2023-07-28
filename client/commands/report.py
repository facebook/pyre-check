# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module provides the entrypoint for `pyre report`, a command to
collect data about code and how well Pyre can understand its types.

"""
from __future__ import annotations

import dataclasses
import json
import logging
import multiprocessing
from pathlib import Path
from typing import List, Optional, Sequence

import libcst

from .. import (
    coverage_data,
    dataclasses_json_extensions as json_mixins,
    frontend_configuration,
    log,
)
from . import commands

LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class ModulePath:
    project_root: Path
    relative_to_root: Path

    def absolute_path(self) -> Path:
        return self.project_root / self.relative_to_root

    @staticmethod
    def create(
        project_root: Path,
        absolute_path: Path,
    ) -> Optional[ModulePath]:
        """
        Given a project root (always absolute) and a raw module path path which
        may be either absolute or relative (to the current directory), represent
        the module path as the project root plus a relative offset.

        If the path is not under `project_root`, return `None`.
        """
        try:
            return ModulePath(
                project_root=project_root,
                relative_to_root=absolute_path.relative_to(project_root),
            )
        except ValueError:
            None


def get_module_paths(
    configuration: frontend_configuration.Base,
    paths: Optional[List[Path]],
) -> List[ModulePath]:
    project_root = configuration.get_local_root() or configuration.get_global_root()
    if paths is None:
        paths = [
            configuration.get_local_root() or configuration.get_global_root(),
        ]
    maybe_module_paths = [
        ModulePath.create(
            project_root=project_root,
            # (the / operator ignores the left hand side if the right side is already absolute)
            absolute_path=Path.cwd() / path,
        )
        for path in coverage_data.find_module_paths(
            paths=paths,
            excludes=configuration.get_excludes(),
        )
    ]
    return [
        module_path for module_path in maybe_module_paths if module_path is not None
    ]


@dataclasses.dataclass(frozen=True)
class ModuleData(json_mixins.SnakeCaseAndExcludeJsonMixin):
    path: str
    mode: coverage_data.ModuleModeInfo
    suppressions: Sequence[coverage_data.TypeErrorSuppression]
    functions: Sequence[coverage_data.FunctionAnnotationInfo]

    @staticmethod
    def collect(
        module: libcst.MetadataWrapper,
        path: ModulePath,
        strict_by_default: bool,
        ignored: bool,
    ) -> ModuleData:
        mode = coverage_data.collect_mode(module, strict_by_default, ignored)
        suppressions = coverage_data.collect_suppressions(module)
        functions = coverage_data.collect_functions(module)
        return ModuleData(
            mode=mode,
            suppressions=suppressions,
            functions=functions,
            # `path` is relative here so that data isn't tied to one machine.
            path=str(path.relative_to_root),
        )

    @dataclasses.dataclass(frozen=True)
    class CollectFromPathArgs:
        """
        Multiprocessing requires mapping a function over a list of single
        arguments, so we have to make a struct in order to parallelize
        collect_from_path.
        """

        path: ModulePath
        strict_by_default: bool
        ignored: bool

    @staticmethod
    def collect_from_path(
        args: ModuleData.CollectFromPathArgs,
    ) -> Optional[ModuleData]:
        module = coverage_data.module_from_path(args.path.absolute_path())
        if module is None:
            return None
        else:
            return ModuleData.collect(
                module,
                path=args.path,
                strict_by_default=args.strict_by_default,
                ignored=args.ignored,
            )

    @staticmethod
    def collect_from_paths(
        module_paths: Sequence[ModulePath],
        strict_by_default: bool,
        number_of_workers: int,
        ignored_modules: Sequence[ModulePath],
    ) -> List[ModuleData]:
        tasks = []
        for path in module_paths:
            ignored = path in ignored_modules
            tasks.append(
                ModuleData.CollectFromPathArgs(
                    path=path, strict_by_default=strict_by_default, ignored=ignored
                )
            )
        with multiprocessing.Pool(number_of_workers) as pool:
            return [
                module_data
                for module_data in pool.imap_unordered(
                    ModuleData.collect_from_path, tasks
                )
                if module_data is not None
            ]


def print_data_as_json(data: Sequence[ModuleData]) -> None:
    raw_data = [module_data.to_dict() for module_data in data]
    json.dump(raw_data, log.stdout)


def run(
    configuration: frontend_configuration.Base,
    paths: Optional[List[Path]],
) -> int:
    module_paths = get_module_paths(
        configuration=configuration,
        paths=paths,
    )
    ignored_paths = [Path(path) for path in configuration.get_ignore_all_errors()]
    data = ModuleData.collect_from_paths(
        module_paths,
        strict_by_default=configuration.is_strict(),
        number_of_workers=configuration.get_number_of_workers(),
        ignored_modules=get_module_paths(configuration, ignored_paths),
    )

    print_data_as_json(data)
    return commands.ExitCode.SUCCESS
