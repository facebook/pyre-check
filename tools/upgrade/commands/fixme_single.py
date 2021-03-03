# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
from pathlib import Path

from ..configuration import Configuration
from ..errors import Errors
from ..filesystem import LocalMode, add_local_mode, path_exists
from ..repository import Repository
from .command import CommandArguments, ErrorSource, ErrorSuppressingCommand


LOG: logging.Logger = logging.getLogger(__name__)


class FixmeSingle(ErrorSuppressingCommand):
    def __init__(
        self,
        command_arguments: CommandArguments,
        *,
        repository: Repository,
        path: Path,
        upgrade_version: bool,
        error_source: ErrorSource,
        fixme_threshold: int,
    ) -> None:
        super().__init__(command_arguments, repository=repository)
        self._path: Path = path
        self._upgrade_version: bool = upgrade_version
        self._error_source: ErrorSource = error_source
        self._fixme_threshold: int = fixme_threshold

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "FixmeSingle":
        command_arguments = CommandArguments.from_arguments(arguments)
        return FixmeSingle(
            command_arguments,
            repository=repository,
            path=arguments.path,
            upgrade_version=arguments.upgrade_version,
            error_source=arguments.error_source,
            fixme_threshold=arguments.fixme_threshold,
        )

    @classmethod
    # pyre-fixme[40]: Non-static method `add_arguments` cannot override a static
    #  method defined in `ErrorSuppressingCommand`.
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(FixmeSingle, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "path",
            help="Path to project root with local configuration",
            type=path_exists,
        )
        parser.add_argument(
            "--upgrade-version",
            action="store_true",
            help="Upgrade and clean project if a version override set.",
        )
        parser.add_argument(
            "--error-source",
            choices=list(ErrorSource),
            default=ErrorSource.GENERATE,
            type=ErrorSource,
        )
        parser.add_argument(
            "--fixme-threshold",
            type=int,
            default=0,
            help="Ignore all errors if fixme count exceeds threshold.",
        )

    def run(self) -> None:
        project_configuration = Configuration.find_project_configuration()
        configuration_path = self._path / ".pyre_configuration.local"
        configuration = Configuration(configuration_path)
        if self._fixme_threshold == 0:
            self._suppress_errors(
                configuration=configuration,
                error_source=self._error_source,
                upgrade_version=self._upgrade_version,
            )
        else:
            all_errors = configuration.get_errors()
            error_threshold = self._fixme_threshold

            for path, errors in all_errors.paths_to_errors.items():
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
                    self._apply_suppressions(Errors(errors))

        local_root = configuration.get_directory().resolve()
        title = "{} for {}".format(
            "Update pyre version" if self._upgrade_version else "Suppress pyre errors",
            str(local_root.relative_to(project_configuration.parent.resolve())),
        )
        self._repository.commit_changes(commit=(not self._no_commit), title=title)
