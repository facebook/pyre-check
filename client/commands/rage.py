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
from .servers import Servers


RAGE_DELIMITER: Final[str] = "=" * 50


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
        self._log_directory_for_binary: str = self.log_directory

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
        return ["-log-directory", self._log_directory_for_binary]

    def _run(self) -> None:
        output_path = self._output_path
        if output_path:
            with open(output_path, "w") as output_file:
                self._rage(output_file)
        else:
            self._rage(sys.stdout)

    def _call_client_for_root_project(self, output_file: IO[str]) -> None:
        self._arguments.servers_subcommand = "list"
        all_servers = Servers(
            self._arguments,
            self._original_directory,
            self._configuration,
            self._analysis_directory,
        )._all_server_details()
        if any(server.is_root() for server in all_servers):
            self._call_client(
                command=self.NAME, capture_output=False, stdout=output_file
            ).check()
        else:
            # We need to pass in an analysis directory because the default
            # analysis directory for a project root without a server is an
            # invalid link tree, which means `_call_client` will fail.
            self._analysis_directory = AnalysisDirectory(".")
            print(
                f"No server running for {self._original_directory}."
                " Printing rage for all running servers.",
                file=output_file,
                flush=True,
            )
            print(f"\n{RAGE_DELIMITER}\n", file=output_file, flush=True)
            for server in all_servers:
                self._log_directory_for_binary = str(
                    self._dot_pyre_directory / server.local_root
                )
                self._call_client(
                    command=self.NAME, capture_output=False, stdout=output_file
                ).check()
                print(f"\n{RAGE_DELIMITER}\n", file=output_file, flush=True)

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
        if self.local_configuration is not None:
            self._call_client(
                command=self.NAME, capture_output=False, stdout=output_file
            ).check()
        else:
            self._call_client_for_root_project(output_file)
