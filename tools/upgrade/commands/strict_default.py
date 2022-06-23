# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
from pathlib import Path
from typing import Optional

from pyre_extensions import override

from ....client.find_directories import (
    CONFIGURATION_FILE,
    find_global_and_local_root,
    LOCAL_CONFIGURATION_FILE,
)
from .. import UserError
from ..configuration import Configuration
from ..filesystem import LocalMode, path_exists
from ..repository import Repository
from .command import CommandArguments, ErrorSource, ErrorSuppressingCommand

LOG: logging.Logger = logging.getLogger(__name__)


def _get_configuration_path(local_configuration: Optional[Path]) -> Optional[Path]:
    found_root = find_global_and_local_root(
        Path(".") if local_configuration is None else local_configuration
    )
    if found_root is None:
        return None
    else:
        local_root = found_root.local_root
        if local_root:
            return local_root / LOCAL_CONFIGURATION_FILE
        else:
            return found_root.global_root / CONFIGURATION_FILE


class StrictDefault(ErrorSuppressingCommand):
    def __init__(
        self,
        command_arguments: CommandArguments,
        *,
        repository: Repository,
        local_configuration: Path,
        remove_strict_headers: bool,
        fixme_threshold: int,
    ) -> None:
        super().__init__(command_arguments, repository)
        self._local_configuration: Path = local_configuration
        self._remove_strict_headers: bool = remove_strict_headers
        self._fixme_threshold: int = fixme_threshold

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "StrictDefault":
        command_arguments = CommandArguments.from_arguments(arguments)
        return StrictDefault(
            command_arguments,
            repository=repository,
            local_configuration=arguments.local_configuration,
            remove_strict_headers=arguments.remove_strict_headers,
            fixme_threshold=arguments.fixme_threshold,
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(StrictDefault, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "-l",
            "--local-configuration",
            type=path_exists,
            help="Path to project root with local configuration",
        )
        parser.add_argument(
            # TODO(T53195818): Not implemented
            "--remove-strict-headers",
            action="store_true",
            help="Delete unnecessary `# pyre-strict` headers.",
        )
        parser.add_argument(
            "--fixme-threshold",
            type=int,
            default=None,
            help="Mark file as unsafe if fixme count exceeds threshold.",
        )

    def _commit_changes(self) -> None:
        title = f"Convert {self._local_configuration} to use strict default"
        summary = (
            "Turning on strict default; files with more than "
            + f"{self._fixme_threshold} errors opted-out of strict."
        )
        self._repository.commit_changes(
            commit=(not self._no_commit),
            title=title,
            summary=summary,
            set_dependencies=False,
        )

    @override
    def run(self) -> None:
        configuration_path = _get_configuration_path(self._local_configuration)
        if configuration_path is None:
            raise UserError("Cannot find a path to configuration")
        configuration = Configuration(configuration_path)
        LOG.info("Processing %s", configuration.get_directory())
        configuration.add_strict()
        configuration.write()

        self._get_and_suppress_errors(
            configuration,
            error_source=ErrorSource.GENERATE,
            fixme_threshold=self._fixme_threshold,
            fixme_threshold_fallback_mode=LocalMode.UNSAFE,
        )
        self._commit_changes()
