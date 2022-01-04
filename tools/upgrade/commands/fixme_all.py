# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging

from ..configuration import Configuration
from ..repository import Repository
from .command import CommandArguments, ErrorSource, ErrorSuppressingCommand


LOG: logging.Logger = logging.getLogger(__name__)


class FixmeAll(ErrorSuppressingCommand):
    def __init__(
        self,
        command_arguments: CommandArguments,
        *,
        repository: Repository,
        upgrade_version: bool,
        error_source: ErrorSource,
    ) -> None:
        super().__init__(command_arguments, repository=repository)
        self._upgrade_version: bool = upgrade_version
        self._error_source: ErrorSource = error_source

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "FixmeAll":
        command_arguments = CommandArguments.from_arguments(arguments)
        return FixmeAll(
            command_arguments,
            repository=repository,
            upgrade_version=arguments.upgrade_version,
            error_source=arguments.error_source,
        )

    @classmethod
    # pyre-fixme[40]: Non-static method `add_arguments` cannot override a static
    #  method defined in `ErrorSuppressingCommand`.
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(FixmeAll, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "--upgrade-version",
            action="store_true",
            help="Upgrade and clean project if a version override set.",
        )
        parser.add_argument(
            "--error-source",
            choices=list(ErrorSource),
            default=ErrorSource.GENERATE,
            type=ErrorSource,
        )

    def run(self) -> None:
        project_configuration = Configuration.find_project_configuration()
        configurations = Configuration.gather_local_configurations()
        for configuration in configurations:
            self._suppress_errors(
                configuration=configuration,
                error_source=self._error_source,
                upgrade_version=self._upgrade_version,
            )
            local_root = configuration.get_directory().resolve()
            title = "{} for {}".format(
                "Update pyre version"
                if self._upgrade_version
                else "Suppress pyre errors",
                str(local_root.relative_to(project_configuration.parent.resolve())),
            )
            self._repository.commit_changes(commit=(not self._no_commit), title=title)
