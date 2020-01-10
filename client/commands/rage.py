# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
from typing import List, Optional

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

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        rage = parser.add_parser(
            cls.NAME,
            epilog="""
            Collects troubleshooting diagnostics for Pyre, and writes this
            information to the terminal.
            """,
        )
        rage.set_defaults(command=cls)

    def _flags(self) -> List[str]:
        log_directory = self._log_directory
        if log_directory:
            return ["-log-directory", log_directory]
        else:
            return []

    def _run(self) -> None:
        # Do not use logging. Logging goes to stderr.
        print("Client version:", __version__, flush=True)
        print("Binary path:", self._configuration.binary, flush=True)
        print(
            "Configured binary version:",
            get_binary_version(self._configuration),
            flush=True,
        )
        self._call_client(command=self.NAME, capture_output=False).check()
