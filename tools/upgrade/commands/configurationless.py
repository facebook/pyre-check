# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Collection, List, Optional

from .. import filesystem
from ..configuration import Configuration
from ..repository import Repository
from .command import Command

LOG: logging.Logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class ConfigurationlessOptions:
    global_configuration: Configuration
    local_configuration: Configuration
    default_project_mode: filesystem.LocalMode
    default_global_mode: filesystem.LocalMode

    @property
    def ignore_all_errors_prefixes(self) -> Collection[Path]:
        return (
            self.global_configuration.get_ignore_path_prefixes()
            | self.local_configuration.get_ignore_path_prefixes()
        )

    @property
    def exclude_patterns(self) -> Collection[re.Pattern[str]]:
        return (
            self.global_configuration.get_exclude_as_patterns()
            | self.local_configuration.get_exclude_as_patterns()
        )


class Configurationless(Command):
    def __init__(
        self, *, repository: Repository, path: Path, includes: List[str], commit: bool
    ) -> None:
        super().__init__(repository)
        self._path: Path = path
        self._includes: List[str] = includes
        self._commit: bool = commit

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "Configurationless":
        return Configurationless(
            repository=repository,
            path=arguments.path,
            includes=arguments.include_file_suffixes,
            commit=(not arguments.no_commit),
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(Configurationless, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "path",
            help="Path to project root with local configuration.",
            type=Path,
        )
        parser.add_argument(
            "--no-commit",
            help="Do not commit changes after completing codemod.",
            action="store_true",
        )
        parser.add_argument(
            "--include-file-suffixes",
            action="extend",
            nargs="+",
            type=str,
            default=["**.py"],
            help="The suffixes to search for and include in the codemod. Default is '**.py'.",
        )

    @staticmethod
    def get_default_global_mode(
        global_configuration: Configuration,
    ) -> filesystem.LocalMode:
        global_is_strict = (
            global_configuration.strict
            if global_configuration.strict is not None
            else True  # set default configuration strictness to STRICT
        )
        return (
            filesystem.LocalMode.STRICT
            if global_is_strict
            else filesystem.LocalMode.UNSAFE
        )

    @staticmethod
    def get_default_local_mode(
        local_configuration: Configuration,
        default_global_mode: filesystem.LocalMode,
    ) -> filesystem.LocalMode:
        default_project_strictness_setting = local_configuration.strict

        if default_project_strictness_setting is None:
            return default_global_mode
        elif default_project_strictness_setting:
            return filesystem.LocalMode.STRICT
        else:
            return filesystem.LocalMode.UNSAFE

    def get_file_mode_to_apply(
        self, file: Path, options: ConfigurationlessOptions
    ) -> Optional[filesystem.LocalMode]:
        file = (self._path / file).absolute()
        if any(
            exclude_pattern.search(str(file)) is not None
            for exclude_pattern in options.exclude_patterns
        ):
            # TODO(T174803521): implement `EXCLUDE` LocalMode and return here
            return None
        elif any(
            file.is_relative_to(ignore_prefix)
            for ignore_prefix in options.ignore_all_errors_prefixes
        ):
            return filesystem.LocalMode.IGNORE
        elif options.default_project_mode == options.default_global_mode:
            return None
        else:
            return options.default_project_mode
