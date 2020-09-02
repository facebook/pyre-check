# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging

from ..configuration import Configuration
from ..repository import Repository
from .command import ErrorSuppressingCommand  # noqa
from .command import CommandArguments, ProjectErrorSuppressingCommand


LOG: logging.Logger = logging.getLogger(__name__)


class FixmeAll(ProjectErrorSuppressingCommand):
    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "FixmeAll":
        command_arguments = CommandArguments.from_arguments(arguments)
        return FixmeAll(
            command_arguments,
            repository=repository,
            only_fix_error_code=arguments.only_fix_error_code,
            upgrade_version=arguments.upgrade_version,
            error_source=arguments.error_source,
            no_commit=arguments.no_commit,
            submit=arguments.submit,
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(FixmeAll, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)

    def run(self) -> None:
        project_configuration = Configuration.find_project_configuration()
        configurations = Configuration.gather_local_configurations()
        for configuration in configurations:
            self._suppress_errors_in_project(
                configuration, project_configuration.parent
            )
