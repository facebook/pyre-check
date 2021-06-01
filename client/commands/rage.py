# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import sys
from typing import IO, List, Optional

from typing_extensions import Final

from .. import command_arguments, recently_used_configurations
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration, SimpleSearchPathElement
from ..version import __version__
from .command import Command, State
from .servers import Servers


RAGE_DELIMITER: Final[str] = "=" * 50


class Rage(Command):
    NAME = "rage"

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        *,
        original_directory: str,
        configuration: Configuration,
        analysis_directory: Optional[AnalysisDirectory] = None,
        output_path: Optional[str],
    ) -> None:
        super(Rage, self).__init__(
            command_arguments, original_directory, configuration, analysis_directory
        )
        self._log_directory_for_binary: str = self._configuration.log_directory
        self._output_path: Final[Optional[str]] = output_path

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
        servers = Servers(
            self._command_arguments,
            self._original_directory,
            configuration=self._configuration,
            analysis_directory=self._analysis_directory,
            subcommand="list",
        )
        recent_local_roots = recently_used_configurations.Cache(
            self._configuration.dot_pyre_directory
        ).get_all_items()
        if servers.is_root_server_running() or recent_local_roots == []:
            self._call_client(
                command=self.NAME, capture_output=False, stdout=output_file
            ).check()
        else:
            # We need to pass in an analysis directory because the default
            # analysis directory for a project root without a server is an
            # invalid link tree, which means `_call_client` will fail.
            self._analysis_directory = AnalysisDirectory(SimpleSearchPathElement("."))
            print(
                f"No server running for {self._original_directory}."
                + " Printing rage for all recently-used servers.",
                file=output_file,
                flush=True,
            )
            print(f"\n{RAGE_DELIMITER}\n", file=output_file, flush=True)
            for local_root in recent_local_roots:
                print(
                    f"\nRage for configuration at `{local_root}`:\n",
                    file=output_file,
                    flush=True,
                )
                self._log_directory_for_binary = str(
                    self._configuration.dot_pyre_directory / local_root
                )
                self._call_rage(output_file)
                print(f"\n{RAGE_DELIMITER}\n", file=output_file, flush=True)

    def _rage(self, output_file: IO[str]) -> None:
        # Do not use logging. Logging goes to stderr.
        print("Client version:", __version__, file=output_file, flush=True)
        print(
            "Binary path:",
            self._configuration.get_binary_respecting_override(),
            file=output_file,
            flush=True,
        )
        print(
            "Configured binary version:",
            self._configuration.get_binary_version()
            or "Cannot get version from binary",
            file=output_file,
            flush=True,
        )
        if self._configuration.local_root is not None:
            self._call_rage(output_file)
        else:
            self._call_client_for_root_project(output_file)

    def _call_rage(
        self,
        output_file: IO[str],
    ) -> None:
        if self._state() != State.RUNNING:
            print(
                f"{RAGE_DELIMITER}\nNo server running!\n{RAGE_DELIMITER}\n",
                file=output_file,
                flush=True,
            )
        self._call_client(
            command=self.NAME,
            capture_output=False,
            stdout=output_file,
            check_analysis_directory=False,
        ).check()
