# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
import shutil
import signal
import subprocess
import tempfile
from itertools import chain
from pathlib import Path
from typing import Optional

import psutil

from .. import (
    command_arguments,
    configuration_monitor,
    recently_used_configurations,
    watchman,
)
from ..analysis_directory import BUCK_BUILDER_CACHE_PREFIX, AnalysisDirectory
from ..configuration import Configuration, SimpleSearchPathElement
from ..find_directories import BINARY_NAME, CLIENT_NAME
from ..project_files_monitor import ProjectFilesMonitor
from .command import Command
from .rage import Rage


LOG: logging.Logger = logging.getLogger(__name__)


PYRE_FIRE = """
                                                    ',
                                                   ,c:
                                ',                ;cc:
                              ,:l;             ';:lll:
                            'cllc'            'clllll:
                           ;loooc'           'cllllllc'
                          ;looool,           :llllllll,       :,
                         'looooool'         ;lollllllo:      ;ll;
                         ;ooooooool,       'loooooloool;    ,clll:
                         cdoddoooooo;      ;oooooooooool:  ;loooll:
                         cddddddddodo;     cooooooooooooolloooooool;
                         ;ddddddddddo;    'loooooooooooooooooooooooc'
                          cdddddddddc     'ldddddooooooooooooooooooo,
                           ,coodolc;       cddddddddddoooooooooooooo;
                               '           ,oddddddddddddddddodooooo;
                          ,::::::::::::,    :ddddddddddddddddddddddl'
                          'lddddddddddxd:    :ddddddddddddddddddddd:
                            ;odddddddddddl,   ;oxdddddddddddddddddl'
                             'ldddddddddddo:   ,:ldxddxddddddddddl'
                               :ddddddddddddl'    cdxxxxxxxdddddl'
                                ,ldddddddddddo;    ,oxxxxxxxxxdc
                                  :ddddddddddddc;'  'cdxxxxxxo;
                                   ,ldddddddddddxo;   ;dxxxo:
                                     cdddddddddddddc   'lo:
                                      ;oddddddddddddo,
                                       'cddddddddddddd:
                                        ;odddddddddddddl,
                                       :ddddddddddddddddd:
                                     ,ldddddddddddddddddddl,
                                    :odddddddddddddddddddddo:
                                  'ldddddddddddddddddddddddddl'
                                 ;odddddddddddl, ,ldddddddddddo;
                               'cdddddddddddd:     :ddddddddddddc'
                              ;odddddddddddo,       ,odddddddddddo;
                             cddddddddddddc           cddddddddddddc
                           ;oxddxddxddddo;             ;odxxxxddddxxo,
                           ;:::::::::::;'               ';:::::::::::;
"""


class Kill(Command):
    NAME = "kill"

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        *,
        original_directory: str,
        configuration: Configuration,
        analysis_directory: Optional[AnalysisDirectory] = None,
        with_fire: bool,
    ) -> None:
        super(Kill, self).__init__(
            command_arguments, original_directory, configuration, analysis_directory
        )
        self._with_fire = with_fire

    def generate_analysis_directory(self) -> AnalysisDirectory:
        return AnalysisDirectory(SimpleSearchPathElement("."))

    @staticmethod
    def _delete_linked_path(link_path: Path) -> None:
        try:
            actual_path = os.readlink(link_path)
            os.remove(actual_path)
        except OSError:
            pass
        try:
            os.unlink(link_path)
        except OSError:
            pass

    def _delete_caches(self) -> None:
        # If a resource cache exists, delete it to remove corrupted artifacts.
        try:
            shutil.rmtree(
                str(self._configuration.dot_pyre_directory / "resource_cache")
            )
        except OSError:
            pass

        # If a buck builder cache exists, also remove it.
        scratch_path = None
        try:
            scratch_path = (
                subprocess.check_output(
                    f"mkscratch path --subdir pyre {self._configuration.project_root}".split()  # noqa
                )
                .decode()
                .strip()
            )
        except Exception as exception:
            LOG.debug("Could not find scratch path because of exception: %s", exception)
        if scratch_path is not None:
            for buck_builder_cache_directory in Path(scratch_path).glob(
                f"{BUCK_BUILDER_CACHE_PREFIX}*"
            ):
                try:
                    LOG.debug(
                        "Deleting buck builder cache at %s",
                        buck_builder_cache_directory,
                    )
                    shutil.rmtree(buck_builder_cache_directory)
                except OSError as exception:
                    LOG.debug(
                        "Failed to delete buck builder cache due to exception: %s.",
                        exception,
                    )
        recently_used_configurations.Cache(
            self._configuration.dot_pyre_directory
        ).delete()

    def _kill_processes_by_name(self, name: str) -> None:
        for process in psutil.process_iter(attrs=["name"]):
            if process.info["name"] != name:
                continue
            # Do not kill the `pyre kill` command itself.
            pid_to_kill = process.pid
            if pid_to_kill == os.getpgid(os.getpid()):
                continue
            try:
                LOG.info(f"Killing process {name} with pid {pid_to_kill}.")
                os.kill(pid_to_kill, signal.SIGKILL)
            except (ProcessLookupError, PermissionError) as exception:
                LOG.debug(
                    f"Failed to kill process {name} "
                    + f"with pid {pid_to_kill} due to exception {exception}"
                )

    def _kill_client_processes(self) -> None:
        self._kill_processes_by_name(CLIENT_NAME)
        watchman.stop_subscriptions(
            ProjectFilesMonitor.base_path(self._configuration), ProjectFilesMonitor.NAME
        )
        watchman.stop_subscriptions(
            configuration_monitor.ConfigurationMonitor.base_path(self._configuration),
            configuration_monitor.ConfigurationMonitor.NAME,
        )

        try:
            isolation_prefix = (
                self._configuration.get_isolation_prefix_respecting_override()
            )
            command = ["buck", "kill"] + (
                ["--isolation_prefix", isolation_prefix] if isolation_prefix else []
            )
            subprocess.run(command)
            LOG.debug(f"Ran {command}")
        except Exception as exception:
            LOG.debug(f"Could not run `{command}`: {exception}")

    def _kill_binary_processes(self) -> None:
        # Kills all processes that have the same binary as the one specified
        # in the configuration.
        LOG.warning("Force-killing all running pyre servers.")
        LOG.warning(
            "Use `pyre servers stop` if you want to gracefully stop"
            + " all running servers."
        )
        binary_name = _get_process_name("PYRE_BINARY", BINARY_NAME)
        self._kill_processes_by_name(binary_name)

    def _delete_server_files(self) -> None:
        dot_pyre_directory = self._configuration.dot_pyre_directory
        LOG.info("Deleting server files under %s", dot_pyre_directory)
        socket_paths = dot_pyre_directory.glob("**/server.sock")
        json_server_paths = dot_pyre_directory.glob("**/json_server.sock")
        pid_paths = dot_pyre_directory.glob("**/server.pid")
        for path in chain(socket_paths, json_server_paths, pid_paths):
            self._delete_linked_path(path)

    def _rage(self) -> None:
        with tempfile.NamedTemporaryFile(
            prefix="pyre-rage-", suffix=".log", delete=False
        ) as output:
            LOG.warning("Saving the output of `pyre rage` into `%s`", output.name)
            Rage(
                self._command_arguments,
                original_directory=self._original_directory,
                configuration=self._configuration,
                analysis_directory=self._analysis_directory,
                output_path=output.name,
            ).run()

    def _run(self) -> None:
        explicit_local = self._configuration.local_root
        if explicit_local:
            LOG.warning(
                "Pyre kill will terminate all running servers. "
                + f"Specifying local path `{explicit_local}` is unnecessary."
            )
        self._rage()
        self._kill_binary_processes()
        self._delete_server_files()
        self._delete_caches()
        self._kill_client_processes()
        if self._with_fire:
            LOG.warning(
                "Note that `--with-fire` adds emphasis to `pyre kill` "
                + "but does not affect its behavior."
                + f"\n{PYRE_FIRE}"
            )


def _get_process_name(environment_variable_name: str, default: str) -> str:
    overridden = os.getenv(environment_variable_name)
    if overridden is not None:
        return os.path.basename(overridden)
    else:
        return default
