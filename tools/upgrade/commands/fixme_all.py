# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
Fixes errors provided through stdin for all sub-projects using Pyre. This
differs from fixme_single in the sense that this runs over a whole repository
(or directory containing a top-level .pyre_configuration) and operates over
sub-projects with .pyre_configuration.local files.
"""


import argparse
import logging

from pyre_extensions import override

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

    @override
    def run(self) -> None:
        project_configuration = Configuration.find_project_configuration()
        configurations = Configuration.gather_local_configurations()
        for configuration in configurations:
            self._get_and_suppress_errors(
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
