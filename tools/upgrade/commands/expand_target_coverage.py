# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
from pathlib import Path
from typing import Optional

from typing_extensions import Final

from ..configuration import Configuration
from ..errors import Errors
from ..filesystem import LocalMode, add_local_mode, find_files
from ..repository import Repository
from .command import CommandArguments, ErrorSuppressingCommand


LOG: logging.Logger = logging.getLogger(__name__)


class ExpandTargetCoverage(ErrorSuppressingCommand):
    def __init__(
        self,
        command_arguments: CommandArguments,
        *,
        repository: Repository,
        subdirectory: Optional[str],
        fixme_threshold: bool,
        no_commit: bool,
        submit: bool,
    ) -> None:
        super().__init__(command_arguments, repository)
        self._subdirectory: Final[Optional[str]] = subdirectory
        self._fixme_threshold: bool = fixme_threshold
        self._no_commit: bool = no_commit
        self._submit: bool = submit

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "ExpandTargetCoverage":
        command_arguments = CommandArguments.from_arguments(arguments)
        return ExpandTargetCoverage(
            command_arguments,
            repository=repository,
            subdirectory=arguments.subdirectory,
            fixme_threshold=arguments.fixme_threshold,
            no_commit=arguments.no_commit,
            submit=arguments.submit,
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(ExpandTargetCoverage, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "--subdirectory", help="Only upgrade TARGETS files within this directory."
        )
        parser.add_argument(
            "--fixme-threshold",
            type=int,
            help="Ignore all errors in a file if fixme count exceeds threshold.",
        )
        parser.add_argument(
            "--no-commit", action="store_true", help="Keep changes in working state."
        )
        parser.add_argument("--submit", action="store_true", help=argparse.SUPPRESS)

    def run(self) -> None:
        subdirectory = self._subdirectory
        subdirectory = Path(subdirectory) if subdirectory else Path.cwd()

        # Do not change if configurations exist below given root
        existing_configurations = find_files(subdirectory, ".pyre_configuration.local")
        if existing_configurations and not existing_configurations == [
            str(subdirectory / ".pyre_configuration.local")
        ]:
            LOG.warning(
                "Cannot expand targets because nested configurations exist:\n%s",
                "\n".join(existing_configurations),
            )
            return

        # Expand coverage
        local_configuration = Configuration.find_local_configuration(subdirectory)
        if not local_configuration:
            LOG.warning("Could not find a local configuration to codemod.")
            return
        LOG.info("Expanding typecheck targets in `%s`", local_configuration)
        configuration = Configuration(local_configuration)
        configuration.add_targets(["//" + str(subdirectory) + "/..."])
        configuration.deduplicate_targets()
        configuration.write()

        # Suppress errors
        all_errors = configuration.get_errors()
        error_threshold = self._fixme_threshold

        for path, errors in all_errors:
            errors = list(errors)
            error_count = len(errors)
            if error_threshold and error_count > error_threshold:
                LOG.info(
                    "%d errors found in `%s`. Adding file-level ignore.",
                    error_count,
                    path,
                )
                add_local_mode(path, LocalMode.IGNORE)
            else:
                self._suppress_errors(Errors(errors))

        # Lint and re-run pyre once to resolve most formatting issues
        if self._lint:
            if self._repository.format():
                errors = configuration.get_errors(should_clean=False)
                self._suppress_errors(errors)

        self._repository.submit_changes(
            commit=(not self._no_commit),
            submit=self._submit,
            title=f"Expand target type coverage in {local_configuration}",
            summary="Expanding type coverage of targets in configuration.",
            set_dependencies=False,
        )
