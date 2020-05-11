# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
import subprocess
from pathlib import Path

from ..configuration import Configuration
from ..errors import Errors
from ..filesystem import FilesystemException
from ..repository import Repository


LOG: logging.Logger = logging.getLogger(__name__)


class Command:
    def __init__(self, arguments: argparse.Namespace, repository: Repository) -> None:
        self._arguments: argparse.Namespace = arguments
        self._repository: Repository = repository

    def run(self) -> None:
        pass


class ErrorSuppressingCommand(Command):
    def __init__(self, arguments: argparse.Namespace, repository: Repository) -> None:
        super().__init__(arguments, repository)
        self._comment: str = arguments.comment
        self._max_line_length: int = arguments.max_line_length
        self._truncate: bool = arguments.truncate
        self._unsafe: bool = getattr(arguments, "unsafe", False)

    def _suppress_errors(self, errors: Errors) -> None:
        errors.suppress(
            self._comment, self._max_line_length, self._truncate, self._unsafe
        )

    def _suppress_errors_in_project(
        self, configuration: Configuration, root: Path
    ) -> None:
        LOG.info("Processing %s", configuration.get_directory())
        if not configuration.is_local:
            return
        if self._arguments.upgrade_version:
            if configuration.version:
                configuration.remove_version()
                configuration.write()
            else:
                return
        errors = (
            Errors.from_stdin(self._arguments.only_fix_error_code)
            if self._arguments.error_source == "stdin"
            and not self._arguments.upgrade_version
            else configuration.get_errors()
        )
        if len(errors) > 0:
            self._suppress_errors(errors)

            # Lint and re-run pyre once to resolve most formatting issues
            if self._arguments.lint:
                if self._repository.format():
                    errors = configuration.get_errors(should_clean=False)
                    self._suppress_errors(errors)
        try:
            project_root = root.resolve()
            local_root = configuration.get_directory().resolve()
            title = "{} for {}".format(
                "Update pyre version"
                if self._arguments.upgrade_version
                else "Suppress pyre errors",
                str(local_root.relative_to(project_root)),
            )
            self._repository.submit_changes(
                commit=(not self._arguments.no_commit),
                submit=self._arguments.submit,
                title=title,
            )
        except subprocess.CalledProcessError:
            action = "submit" if self._arguments.submit else "commit"
            raise FilesystemException(f"Error while attempting to {action} changes.")
