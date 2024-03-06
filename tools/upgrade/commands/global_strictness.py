# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
The global strictness command sets the global configuration's strictness field and adjusts the local configurations to preserve semantics.

For example, if a global configuration is strict and local configurations are non-strict, running the command with strict=False will set the global configuration to
strict=False, remove strict=False, and set any unset local configurations to strict=True. Alternatively, running with strict=True will set the global configuration to
strict, remove any configurations strict=True, and any unset configurations will be set to False.
"""
from __future__ import annotations

import argparse
import logging

from ..configuration import Configuration
from ..repository import Repository
from .command import Command

LOG: logging.Logger = logging.getLogger(__name__)


class GlobalStrictness(Command):
    def __init__(self, *, repository: Repository, strict: bool) -> None:
        self.repository = repository
        self.strict = strict

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> GlobalStrictness:
        return GlobalStrictness(repository=repository, strict=arguments.strict)

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(GlobalStrictness, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "--strict",
            type=bool,
            help="Whether to make the global configuration strict or non-strict",
        )

    def run(self) -> None:
        global_configuration = Configuration(Configuration.find_project_configuration())

        LOG.info(f"Setting global strictness to {self.strict}...")
        global_configuration.strict = self.strict
        global_configuration.write()
        configurations = Configuration.gather_local_configurations()

        LOG.info(
            f"Found {len(configurations)} configurations. Setting unset configurations to strict={not self.strict}..."
        )
        additions = 0
        removals = 0
        for configuration in configurations:
            if configuration.strict is None:
                additions += 1
                configuration.strict = not self.strict
                configuration.write()
            if configuration.strict == self.strict:
                removals += 1
                configuration.strict = None
                configuration.write()
        LOG.info(
            f"Done setting {additions} local configurations to strict={not self.strict} and removing {removals} unnecessary settings."
        )
