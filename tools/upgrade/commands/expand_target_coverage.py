# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
from pathlib import Path
from typing import Optional

from typing_extensions import Final

from ..configuration import Configuration
from ..filesystem import LocalMode, find_files, path_exists
from ..repository import Repository
from .command import CommandArguments, ErrorSource, ErrorSuppressingCommand


LOG: logging.Logger = logging.getLogger(__name__)


class ExpandTargetCoverage(ErrorSuppressingCommand):
    def __init__(
        self,
        command_arguments: CommandArguments,
        *,
        repository: Repository,
        local_configuration: Optional[str],
        fixme_threshold: bool,
    ) -> None:
        super().__init__(command_arguments, repository)
        self._local_configuration: Final[Optional[str]] = local_configuration
        self._fixme_threshold: bool = fixme_threshold

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "ExpandTargetCoverage":
        command_arguments = CommandArguments.from_arguments(arguments)
        return ExpandTargetCoverage(
            command_arguments,
            repository=repository,
            local_configuration=arguments.local_configuration,
            fixme_threshold=arguments.fixme_threshold,
        )

    @classmethod
    # pyre-fixme[40]: Non-static method `add_arguments` cannot override a static
    #  method defined in `ErrorSuppressingCommand`.
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(ExpandTargetCoverage, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "-l",
            "--local-configuration",
            type=path_exists,
            help="Path to project root with local configuration",
        )
        parser.add_argument(
            "--fixme-threshold",
            type=int,
            help="Ignore all errors in a file if fixme count exceeds threshold.",
        )

    def run(self) -> None:
        local_root = self._local_configuration
        local_root = Path(local_root) if local_root else Path.cwd()

        # Do not change if configurations exist below given root
        existing_configurations = find_files(local_root, ".pyre_configuration.local")
        if existing_configurations and not existing_configurations == [
            str(local_root / ".pyre_configuration.local")
        ]:
            LOG.warning(
                "Cannot expand targets because nested configurations exist:\n%s",
                "\n".join(existing_configurations),
            )
            return

        # Expand coverage
        local_configuration = Configuration.find_local_configuration(local_root)
        if not local_configuration:
            LOG.warning("Could not find a local configuration to codemod.")
            return
        LOG.info("Expanding typecheck targets in `%s`", local_configuration)
        configuration = Configuration(local_configuration)
        existing_targets = configuration.targets
        glob_target = "//{}/...".format(str(local_root))
        if existing_targets == [glob_target]:
            LOG.info("Configuration is already fully expanded.")
            return
        configuration.add_targets([glob_target])
        configuration.deduplicate_targets()
        configuration.write()

        # Suppress errors
        self._get_and_suppress_errors(
            configuration,
            error_source=ErrorSource.GENERATE,
            fixme_threshold=self._fixme_threshold,
            fixme_threshold_fallback_mode=LocalMode.IGNORE,
        )

        self._repository.commit_changes(
            commit=(not self._no_commit),
            title=f"Expand target type coverage in {local_root}",
            summary="Expanding type coverage of targets in configuration.",
            set_dependencies=False,
        )
