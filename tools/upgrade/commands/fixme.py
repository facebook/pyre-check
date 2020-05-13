# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import json
import logging
from typing import Optional

from ..configuration import Configuration
from ..errors import Errors
from ..repository import Repository
from .command import ErrorSuppressingCommand


LOG: logging.Logger = logging.getLogger(__name__)


def _errors_from_run(only_fix_error_code: Optional[int] = None) -> Errors:
    configuration_path = Configuration.find_project_configuration()
    if not configuration_path:
        LOG.warning("Could not find pyre configuration.")
        return Errors.empty()
    with open(configuration_path) as configuration_file:
        configuration = Configuration(configuration_path, json.load(configuration_file))
        return configuration.get_errors(only_fix_error_code)


class Fixme(ErrorSuppressingCommand):
    def __init__(self, arguments: argparse.Namespace, repository: Repository) -> None:
        super().__init__(arguments, repository)
        self._lint: bool = arguments.lint
        self._error_source: str = arguments.error_source
        self._only_fix_error_code: Optional[int] = arguments.only_fix_error_code

    @staticmethod
    def add_arguments(parser: argparse.ArgumentParser) -> None:
        parser.set_defaults(command=Fixme)
        parser.add_argument(
            "--error-source", choices=["stdin", "generate"], default="stdin"
        )
        parser.add_argument("--comment", help="Custom comment after fixme comments")
        parser.add_argument(
            "--unsafe",
            action="store_true",
            help="Don't check syntax when applying fixmes.",
        )
        parser.add_argument("--lint", action="store_true", help=argparse.SUPPRESS)
        parser.add_argument(
            "--force-format-unsuppressed", action="store_true", help=argparse.SUPPRESS
        )

    def run(self) -> None:
        # Suppress errors in project with no local configurations.
        if self._error_source == "generate":
            errors = _errors_from_run(self._only_fix_error_code)
            self._suppress_errors(errors)

            if self._lint:
                if self._repository.format():
                    errors = _errors_from_run(self._only_fix_error_code)
                    self._suppress_errors(errors)
        else:
            errors = Errors.from_stdin(self._only_fix_error_code)
            self._suppress_errors(errors)
