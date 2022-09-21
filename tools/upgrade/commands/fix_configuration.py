# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import argparse
import logging
import subprocess
from pathlib import Path
from typing import Optional

from pyre_extensions import override

from .. import UserError
from ..configuration import Configuration
from ..filesystem import path_exists
from ..repository import Repository
from .command import CommandArguments, ErrorSuppressingCommand
from .consolidate_nested_configurations import consolidate_nested


LOG: logging.Logger = logging.getLogger(__name__)


class FixConfiguration(ErrorSuppressingCommand):
    def __init__(
        self, command_arguments: CommandArguments, *, repository: Repository, path: Path
    ) -> None:
        super().__init__(command_arguments, repository)
        self._path: Path = path
        self._configuration: Optional[Configuration] = Configuration(
            path / ".pyre_configuration.local"
        )

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "FixConfiguration":
        command_arguments = CommandArguments.from_arguments(arguments)
        return FixConfiguration(
            command_arguments, repository=repository, path=arguments.path
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(FixConfiguration, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "path",
            help="Path to project root with local configuration",
            type=path_exists,
        )

    def _remove_bad_targets(self) -> None:
        configuration = self._configuration
        if not configuration:
            return
        targets = configuration.targets
        if not targets:
            return
        buildable_targets = []
        for target in targets:
            build_command = ["buck", "query", target]
            try:
                subprocess.check_output(build_command, timeout=30)
            except subprocess.TimeoutExpired:
                buildable_targets.append(target)
            except subprocess.CalledProcessError:
                LOG.info(f"Removing bad target: {target}")
                pass
            else:
                buildable_targets.append(target)
        if len(buildable_targets) == 0 and not configuration.source_directories:
            LOG.info(f"Removing empty configuration at: {configuration.get_path()}")
            self._repository.remove_paths([configuration.get_path()])
            self._configuration = None
        else:
            configuration.targets = buildable_targets
            configuration.write()

    def _consolidate_nested(self) -> None:
        parent_local_configuration_path = Configuration.find_parent_file(
            ".pyre_configuration.local", self._path.parent
        )
        if not parent_local_configuration_path:
            return
        parent_local_configuration = Configuration(parent_local_configuration_path)
        ignored_subdirectories = parent_local_configuration.ignore_all_errors or []
        if (
            str(self._path.relative_to(parent_local_configuration_path.parent))
            in ignored_subdirectories
        ):
            return
        LOG.info(
            f"Consolidating with configuration at: {parent_local_configuration_path}"
        )
        consolidate_nested(
            self._repository,
            parent_local_configuration_path,
            [self._path / ".pyre_configuration.local"],
        )
        self._configuration = parent_local_configuration

    def _commit_changes(self) -> None:
        title = "Fix broken configuration for {}".format(str(self._path))
        self._repository.commit_changes(
            commit=(not self._no_commit),
            title=title,
            summary="Cleaning up broken pyre configurations by removing targets "
            + "that cannot build and removing nested configurations where applicable.",
            reviewers=["pyre", "sentinel"],
        )

    @override
    def run(self) -> None:
        self._remove_bad_targets()
        self._consolidate_nested()

        # Clean any revealed errors.
        configuration = self._configuration
        if configuration:
            try:
                self._get_and_suppress_errors(configuration)
            except UserError as error:
                LOG.warning(
                    f"Configuration at {configuration.get_path()} still "
                    + f"does not build:\n{str(error)}."
                )
                LOG.warning("Discarding changes.")
                self._repository.revert_all(remove_untracked=False)
                return
        self._commit_changes()
