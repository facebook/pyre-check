# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
TODO(T132414938) Add a module-level docstring
"""


import argparse
import logging
from pathlib import Path
from typing import Dict, List, Optional

from pyre_extensions import override

from typing_extensions import Final

from ..configuration import Configuration
from ..filesystem import find_files
from ..repository import Repository
from .command import CommandArguments, ErrorSuppressingCommand


LOG: logging.Logger = logging.getLogger(__name__)


def consolidate_nested(
    repository: Repository, topmost: Path, nested: List[Path]
) -> None:
    total_targets = []
    for nested_configuration in nested:
        configuration = Configuration(nested_configuration)
        targets = configuration.targets
        if targets:
            total_targets.extend(targets)
            repository.remove_paths([nested_configuration])
    configuration = Configuration(topmost)
    configuration.add_targets(total_targets)
    configuration.deduplicate_targets()
    configuration.write()


class ConsolidateNestedConfigurations(ErrorSuppressingCommand):
    def __init__(
        self,
        command_arguments: CommandArguments,
        *,
        repository: Repository,
        subdirectory: Optional[str],
    ) -> None:
        super().__init__(command_arguments, repository)
        self._subdirectory: Final[Optional[str]] = subdirectory

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "ConsolidateNestedConfigurations":
        command_arguments = CommandArguments.from_arguments(arguments)
        return ConsolidateNestedConfigurations(
            command_arguments,
            repository=repository,
            subdirectory=arguments.subdirectory,
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(ConsolidateNestedConfigurations, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument("--subdirectory")

    @staticmethod
    def gather_nested_configuration_mapping(
        configurations: List[str],
    ) -> Dict[str, List[str]]:
        nested_configurations = {}
        for configuration in configurations:
            if len(nested_configurations) == 0:
                nested_configurations[configuration] = []
                continue
            inserted = False
            for topmost_configuration in nested_configurations.keys():
                existing = topmost_configuration.replace(
                    ".pyre_configuration.local", ""
                )
                current = configuration.replace(".pyre_configuration.local", "")
                if current.startswith(existing):
                    nested_configurations[topmost_configuration].append(configuration)
                    inserted = True
                    break
                elif existing.startswith(current):
                    nested_configurations[configuration] = nested_configurations[
                        topmost_configuration
                    ] + [topmost_configuration]
                    del nested_configurations[topmost_configuration]
                    inserted = True
                    break
            if not inserted:
                nested_configurations[configuration] = []
        return nested_configurations

    @override
    def run(self) -> None:
        subdirectory = self._subdirectory
        subdirectory = Path(subdirectory) if subdirectory else Path.cwd()

        # Find configurations
        configurations = sorted(find_files(subdirectory, ".pyre_configuration.local"))
        if not configurations:
            LOG.warning(
                f"Skipping consolidation. No configurations found in {subdirectory}"
            )
            return
        if len(configurations) == 1:
            configuration = configurations[0]
            LOG.warning(
                f"Skipping consolidation. Only one configuration found: {configuration}"
            )
            return

        # Gather nesting structure of configurations
        nested_configurations = self.gather_nested_configuration_mapping(configurations)
        if all(len(nested) == 0 for nested in nested_configurations.values()):
            LOG.warning(
                "Skipping consolidation. None of the configurations found are nested."
            )
            return

        # Consolidate targets
        for topmost, nested in nested_configurations.items():
            if len(nested) == 0:
                continue
            consolidate_nested(
                self._repository,
                Path(topmost),
                [Path(configuration) for configuration in nested],
            )
            configuration = Configuration(Path(topmost))
            self._get_and_suppress_errors(configuration)

        self._repository.commit_changes(
            commit=(not self._no_commit),
            title=f"Consolidate configurations in {subdirectory}",
            summary="Consolidating nested configurations.",
            set_dependencies=False,
        )
