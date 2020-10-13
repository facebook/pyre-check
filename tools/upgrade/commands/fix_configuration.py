# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
import subprocess
from pathlib import Path
from typing import Optional

from .. import UserError
from ..configuration import Configuration
from ..filesystem import path_exists
from ..repository import Repository
from .command import CommandArguments, ErrorSuppressingCommand


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
        configuration = self._configuration
        parent_local_configuration = Configuration.find_parent_file(
            ".pyre_configuration.local", self._path.parent
        )
        if not parent_local_configuration or not configuration:
            return
        LOG.info(f"Consolidating with configuration at: {parent_local_configuration}")
        parent_configuration = Configuration(parent_local_configuration)
        child_targets = configuration.targets
        if not child_targets:
            return
        parent_configuration.add_targets(child_targets)
        parent_configuration.deduplicate_targets()
        parent_configuration.write()
        self._repository.remove_paths([configuration.get_path()])
        self._configuration = parent_configuration

    def _commit_changes(self) -> None:
        title = "Fix broken configuration for {}".format(str(self._path))
        self._repository.submit_changes(
            commit=(not self._no_commit),
            submit=self._submit,
            title=title,
            summary="Cleaning up broken pyre configurations by removing targets "
            + "that cannot build and removing nested configurations where applicable.",
            reviewers=["pyre", "sentinel"],
        )

    def run(self) -> None:
        self._remove_bad_targets()
        self._consolidate_nested()

        # Clean any revealed errors.
        configuration = self._configuration
        if configuration:
            LOG.info(f"Checking for errors in: {configuration.get_path()}")
            try:
                errors = configuration.get_errors()
                self._apply_suppressions(errors)
                if self._lint:
                    if self._repository.format():
                        errors = configuration.get_errors(should_clean=False)
                        self._apply_suppressions(errors)
            except UserError as error:
                LOG.warning(
                    f"Configuration at {configuration.get_path()} still "
                    + f"does not build:\n{str(error)}."
                )
                LOG.warning("Discarding changes.")
                self._repository.revert_all(remove_untracked=False)
                return
        self._commit_changes()
