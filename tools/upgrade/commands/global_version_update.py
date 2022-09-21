# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import argparse
import logging
from pathlib import Path
from typing import List

from pyre_extensions import override

from ..configuration import Configuration
from ..filesystem import path_exists
from ..repository import Repository
from .command import Command, CommandArguments, ErrorSource
from .fixme import Fixme


LOG: logging.Logger = logging.getLogger(__name__)


class GlobalVersionUpdate(Command):
    def __init__(
        self,
        *,
        repository: Repository,
        error_source: str,
        hash: str,
        paths: List[Path],
        no_commit: bool,
    ) -> None:
        super().__init__(repository)
        self._error_source: str = error_source
        self._hash: str = hash
        self._paths: List[Path] = paths
        self._no_commit: bool = no_commit

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "GlobalVersionUpdate":
        return GlobalVersionUpdate(
            repository=repository,
            error_source=arguments.error_source,
            hash=arguments.hash,
            paths=arguments.paths,
            no_commit=arguments.no_commit,
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(GlobalVersionUpdate, GlobalVersionUpdate).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument("hash", help="Hash of new Pyre version")
        parser.add_argument(
            "--paths",
            nargs="*",
            help="A list of paths to local Pyre projects.",
            default=[],
            type=path_exists,
        )
        parser.add_argument(
            "--error-source",
            type=ErrorSource,
            choices=list(ErrorSource),
            default=ErrorSource.GENERATE,
        )
        parser.add_argument(
            "--no-commit", action="store_true", help="Keep changes in working state."
        )

    def _set_local_overrides(
        self, configuration_paths: List[Path], old_version: str
    ) -> None:
        for configuration_path in configuration_paths:
            if "mock_repository" in str(configuration_path):
                # Skip local configurations we have for testing.
                continue
            local_configuration = Configuration(configuration_path)
            if local_configuration.version:
                LOG.info(
                    "Skipping %s as it already has a custom version field.",
                    configuration_path,
                )
                continue
            local_configuration.set_version(old_version)
            local_configuration.write()

    def _suppress_global_errors(self, global_configuration: Configuration) -> None:
        if global_configuration.targets or global_configuration.source_directories:
            LOG.info("Suppressing errors after upgrading global version.")
            command_arguments = CommandArguments(
                comment=None,
                max_line_length=None,
                truncate=False,
                unsafe=False,
                force_format_unsuppressed=False,
                lint=True,
                no_commit=True,
                should_clean=True,
            )
            fixme_command = Fixme(
                command_arguments,
                repository=self._repository,
                error_source=self._error_source,
            )
            fixme_command.run()

        self._repository.commit_changes(
            commit=(not self._no_commit),
            title="Update pyre global configuration version",
            summary=f"Automatic upgrade to hash `{self._hash}`",
            ignore_failures=True,
        )

    @override
    def run(self) -> None:
        global_configuration = Configuration.find_project_configuration()

        # Update to new global version.
        configuration = Configuration(global_configuration)
        old_version = configuration.version
        if not old_version:
            LOG.error(
                "Global configuration at %s has no version field.", global_configuration
            )
            return
        configuration.set_version(self._hash)
        configuration.write()

        paths = self._paths
        configuration_paths = (
            [path / ".pyre_configuration.local" for path in paths]
            if paths
            else [
                configuration.get_path()
                for configuration in Configuration.gather_local_configurations()
            ]
        )

        self._set_local_overrides(configuration_paths, old_version)
        self._suppress_global_errors(configuration)
