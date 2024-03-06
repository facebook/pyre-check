# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
strict_default contains a library for running a codemod across a codebase that turns pyre-strict on. Optionally, we can also remove the #pyre-strict and add #pyre-unsafe headers from files where the error count is below a certain threshhold.
"""


import argparse
import logging
from pathlib import Path
from typing import List, Optional

from pyre_extensions import override

from ....client.find_directories import (
    CONFIGURATION_FILE,
    find_global_and_local_root,
    LOCAL_CONFIGURATION_FILE,
)
from .. import UserError
from ..configuration import Configuration
from ..filesystem import LocalMode, path_exists, remove_local_mode
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
        remove_unsafe_headers: bool,
        remove_strict_flag: bool,
        paths: List[str] | None,
    ) -> None:
        super().__init__(command_arguments, repository)
        self._local_configuration: Path = local_configuration
        self._remove_strict_headers: bool = remove_strict_headers
        self._fixme_threshold: int = fixme_threshold
        self._remove_unsafe_headers: bool = remove_unsafe_headers
        self._remove_strict_flag: bool = remove_strict_flag
        self._paths: List[str] | None = paths

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
            remove_unsafe_headers=arguments.remove_unsafe_headers,
            remove_strict_flag=arguments.remove_strict_flag,
            paths=arguments.paths,
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
        parser.add_argument(
            "--remove-unsafe-headers",
            action="store_true",
            help="Remove `# pyre-unsafe` headers and replace with `# pyre-fixmes` if the number of new suppressions is under the given fixme threshold",
        )
        parser.add_argument(
            "--remove-strict-flag",
            action="store_true",
            help="Remove the strict flag completely. Useful for repositories where configurations are strict by default.",
        )
        parser.add_argument(
            "--paths",
            default=None,
            nargs="+",
            help="Paths to convert to strict default. Defaults to all paths in the current directory.",
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
        if self._remove_strict_flag:
            configuration.use_strict_default()
        else:
            configuration.add_strict()
        configuration.write()

        argument_paths = self._paths
        if argument_paths is None or len(argument_paths) == 0:
            source_paths = configuration.get_source_paths()
        else:
            LOG.info("Running on specific paths: %s", ", ".join(argument_paths))
            source_paths = (Path(path) for path in argument_paths)

        modes = [
            *([LocalMode.STRICT] if self._remove_strict_headers else []),
            *([LocalMode.UNSAFE] if self._remove_unsafe_headers else []),
        ]

        if self._remove_strict_headers or self._remove_unsafe_headers:
            for path in source_paths:
                remove_local_mode(path, modes)

        self._get_and_suppress_errors(
            configuration,
            error_source=ErrorSource.GENERATE,
            fixme_threshold=self._fixme_threshold,
            fixme_threshold_fallback_mode=LocalMode.UNSAFE,
        )
        self._commit_changes()
