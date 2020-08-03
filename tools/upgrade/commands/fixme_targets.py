# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
from pathlib import Path
from typing import List, Optional

from typing_extensions import Final

from ..configuration import Configuration
from ..errors import errors_from_targets
from ..filesystem import Target, find_targets
from ..repository import Repository
from .command import CommandArguments, ErrorSuppressingCommand


LOG: logging.Logger = logging.getLogger(__name__)


class FixmeTargets(ErrorSuppressingCommand):
    def __init__(
        self,
        command_arguments: CommandArguments,
        *,
        repository: Repository,
        subdirectory: Optional[str],
        no_commit: bool,
        submit: bool,
    ) -> None:
        super().__init__(command_arguments, repository)
        self._subdirectory: Final[Optional[str]] = subdirectory
        self._no_commit: bool = no_commit
        self._submit: bool = submit

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "FixmeTargets":
        command_arguments = CommandArguments.from_arguments(arguments)
        return FixmeTargets(
            command_arguments,
            repository=repository,
            subdirectory=arguments.subdirectory,
            no_commit=arguments.no_commit,
            submit=arguments.submit,
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(FixmeTargets, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument("--submit", action="store_true", help=argparse.SUPPRESS)
        parser.add_argument(
            "--subdirectory", help="Only upgrade TARGETS files within this directory."
        )
        parser.add_argument(
            "--no-commit", action="store_true", help="Keep changes in working state."
        )

    def run(self) -> None:
        subdirectory = self._subdirectory
        subdirectory = Path(subdirectory) if subdirectory else None
        project_configuration = Configuration.find_project_configuration(subdirectory)
        project_directory = project_configuration.parent
        search_root = subdirectory if subdirectory else project_directory

        all_targets = find_targets(search_root)
        if not all_targets:
            return
        for path, targets in all_targets.items():
            self._run_fixme_targets_file(project_directory, path, targets)

        self._repository.submit_changes(
            commit=(not self._no_commit),
            submit=self._submit,
            title=f"Upgrade pyre version for {search_root} (TARGETS)",
        )

    def _run_fixme_targets_file(
        self, project_directory: Path, path: str, targets: List[Target]
    ) -> None:
        LOG.info("Processing %s...", path)
        target_names = [
            path.replace("/TARGETS", "") + ":" + target.name + "-pyre-typecheck"
            for target in targets
        ]
        errors = errors_from_targets(project_directory, path, target_names)
        if not errors:
            return
        LOG.info("Found %d type errors in %s.", len(errors), path)

        if not errors:
            return

        self._suppress_errors(errors)

        if not self._lint:
            return

        if self._repository.format():
            errors = errors_from_targets(project_directory, path, target_names)
            if not errors:
                LOG.info("Errors unchanged after linting.")
                return
            LOG.info("Found %d type errors after linting.", len(errors))
            self._suppress_errors(errors)
