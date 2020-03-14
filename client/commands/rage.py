# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import os
import sys
from typing import IO, List, Optional

from typing_extensions import Final

from .. import get_binary_version
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from ..version import __version__
from .command import Command


class Rage(Command):
    NAME = "rage"

    def __init__(
        self,
        arguments: argparse.Namespace,
        original_directory: str,
        configuration: Optional[Configuration] = None,
        analysis_directory: Optional[AnalysisDirectory] = None,
    ) -> None:
        super(Rage, self).__init__(
            arguments, original_directory, configuration, analysis_directory
        )
        self._output_path: Final[Optional[str]] = arguments.output_path

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        rage = parser.add_parser(
            cls.NAME,
            epilog="""
            Collects troubleshooting diagnostics for Pyre, and writes this
            information to the terminal or to a file.
            """,
        )
        rage.set_defaults(command=cls)
        rage.add_argument(
            "--output-file",
            default=None,
            metavar="PATH",
            dest="output_path",
            help="The path to the output file (defaults to stdout)",
            type=os.path.abspath,
        )

    def _flags(self) -> List[str]:
        log_directory = self._log_directory
        if log_directory:
            return ["-log-directory", log_directory]
        else:
            return []

    def _run(self) -> None:
        output_path = self._output_path
        if output_path:
            with open(output_path, "w") as output_file:
                self._rage(output_file)
        else:
            self._rage(sys.stdout)

    def _rage(self, output_file: IO[str]) -> None:
        # Do not use logging. Logging goes to stderr.
        print("Client version:", __version__, file=output_file, flush=True)
        print("Binary path:", self._configuration.binary, file=output_file, flush=True)
        print(
            "Configured binary version:",
            get_binary_version(self._configuration) or "Cannot get version from binary",
            file=output_file,
            flush=True,
        )
        self._call_client(
            command=self.NAME, capture_output=False, stdout=output_file
        ).check()
