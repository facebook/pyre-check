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


def get_module_paths(
    configuration: frontend_configuration.Base, paths: Optional[List[Path]]
) -> List[Path]:
    return list(
        coverage_data.find_module_paths(
            coverage_data.get_paths_to_collect(
                paths,
                root=(
                    configuration.get_local_root() or configuration.get_global_root()
                ),
            ),
            excludes=configuration.get_excludes(),
        )
    )


@dataclasses.dataclass(frozen=True)
class ModuleData(json_mixins.SnakeCaseAndExcludeJsonMixin):
    path: str
    mode: coverage_data.ModuleModeInfo
    suppressions: Sequence[coverage_data.TypeErrorSuppression]
    functions: Sequence[coverage_data.FunctionAnnotationInfo]

    @staticmethod
    def collect(
        module: libcst.MetadataWrapper,
        path: Path,
        strict_by_default: bool,
    ) -> ModuleData:
        mode = coverage_data.collect_mode(module, strict_by_default)
        suppressions = coverage_data.collect_suppressions(module)
        functions = coverage_data.collect_functions(module)
        return ModuleData(
            mode=mode,
            suppressions=suppressions,
            functions=functions,
            # `path` is relative here so that data isn't tied to one machine.
            path=str(path.relative_to(Path.cwd())),
        )

    @dataclasses.dataclass(frozen=True)
    class CollectFromPathArgs:
        """
        Multiprocessing requires mapping a function over a list of single
        arguments, so we have to make a struct in order to parallelize
        collect_from_path.
        """

        path: Path
        strict_by_default: bool

    @staticmethod
    def collect_from_path(
        args: ModuleData.CollectFromPathArgs,
    ) -> Optional[ModuleData]:
        module = coverage_data.module_from_path(args.path)
        if module is None:
            return None
        else:
            return ModuleData.collect(
                module,
                args.path,
                args.strict_by_default,
            )

    @staticmethod
    def collect_from_paths(
        module_paths: Sequence[Path],
        strict_by_default: bool,
        number_of_workers: int,
    ) -> List[ModuleData]:
        tasks = [
            ModuleData.CollectFromPathArgs(
                path=path, strict_by_default=strict_by_default
            )
            for path in module_paths
        ]
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
    data = ModuleData.collect_from_paths(
        module_paths,
        strict_by_default=configuration.is_strict(),
        number_of_workers=configuration.get_number_of_workers(),
    )
    print_data_as_json(data)
    return commands.ExitCode.SUCCESS
