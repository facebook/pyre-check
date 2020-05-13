# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import json
import logging
from enum import Enum
from typing import Optional

from ..configuration import Configuration
from ..errors import Errors
from ..repository import Repository
from .command import ErrorSuppressingCommand


LOG: logging.Logger = logging.getLogger(__name__)


class ErrorSource(Enum):
    STDIN = "stdin"
    GENERATE = "generate"


class Fixme(ErrorSuppressingCommand):
    def __init__(self, arguments: argparse.Namespace, repository: Repository) -> None:
        super().__init__(arguments, repository)
        self._lint: bool = arguments.lint
        self._error_source: str = arguments.error_source
        self._only_fix_error_code: Optional[int] = arguments.only_fix_error_code

    @staticmethod
    def add_arguments(parser: argparse.ArgumentParser) -> None:
        ErrorSuppressingCommand.add_arguments(parser)
        parser.set_defaults(command=Fixme)
        parser.add_argument(
            "--error-source", choices=list(ErrorSource), default=ErrorSource.STDIN
        )
        parser.add_argument("--lint", action="store_true", help=argparse.SUPPRESS)

    def run(self) -> None:
        if self._error_source == ErrorSource.GENERATE:
            errors = self._generate_errors()
            self._suppress_errors(errors)

            if self._lint:
                if self._repository.format():
                    errors = self._generate_errors()
                    self._suppress_errors(errors)
        else:
            errors = Errors.from_stdin(self._only_fix_error_code)
            self._suppress_errors(errors)

    def _generate_errors(self) -> Errors:
        configuration_path = Configuration.find_project_configuration()
        with open(configuration_path) as configuration_file:
            configuration = Configuration(
                configuration_path, json.load(configuration_file)
            )
            return configuration.get_errors(self._only_fix_error_code)
