# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
from pathlib import Path

from ..configuration import Configuration
from ..filesystem import path_exists
from ..repository import Repository
from .command import CommandArguments, ErrorSource, ProjectErrorSuppressingCommand


LOG: logging.Logger = logging.getLogger(__name__)


class FixmeSingle(ProjectErrorSuppressingCommand):
    def __init__(
        self,
        command_arguments: CommandArguments,
        *,
        repository: Repository,
        only_fix_error_code: int,
        upgrade_version: bool,
        error_source: ErrorSource,
        path: Path,
    ) -> None:
        super().__init__(
            command_arguments,
            repository=repository,
            only_fix_error_code=only_fix_error_code,
            upgrade_version=upgrade_version,
            error_source=error_source,
        )
        self._path: Path = path

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "FixmeSingle":
        command_arguments = CommandArguments.from_arguments(arguments)
        return FixmeSingle(
            command_arguments,
            repository=repository,
            only_fix_error_code=arguments.only_fix_error_code,
            upgrade_version=arguments.upgrade_version,
            error_source=arguments.error_source,
            path=arguments.path,
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(FixmeSingle, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "path",
            help="Path to project root with local configuration",
            type=path_exists,
        )

    def run(self) -> None:
        project_configuration = Configuration.find_project_configuration()
        configuration_path = self._path / ".pyre_configuration.local"
        configuration = Configuration(configuration_path)
        self._suppress_errors_in_project(configuration, project_configuration.parent)
