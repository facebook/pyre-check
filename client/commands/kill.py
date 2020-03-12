# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
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

from .. import BINARY_NAME, CLIENT_NAME, LOG_DIRECTORY, configuration_monitor
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from ..project_files_monitor import ProjectFilesMonitor
from ..watchman_subscriber import WatchmanSubscriber
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
        arguments: argparse.Namespace,
        original_directory: str,
        configuration: Optional[Configuration] = None,
        analysis_directory: Optional[AnalysisDirectory] = None,
    ) -> None:
        super(Kill, self).__init__(
            arguments, original_directory, configuration, analysis_directory
        )
        self._with_fire: bool = arguments.with_fire

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        kill = parser.add_parser(cls.NAME)
        kill.set_defaults(command=cls)
        kill.add_argument(
            "--with-fire", action="store_true", help="A no-op flag that adds emphasis."
        )

    def generate_analysis_directory(self) -> AnalysisDirectory:
        return AnalysisDirectory(".")

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
        root_log_directory = Path(self._current_directory, LOG_DIRECTORY)
        # If a resource cache exists, delete it to remove corrupted artifacts.
        try:
            shutil.rmtree(str(root_log_directory / "resource_cache"))
        except OSError:
            pass

        # If a buck builder cache exists, also remove it.
        scratch_path = None
        try:
            scratch_path = (
                subprocess.check_output(
                    f"mkscratch path --subdir pyre {self._current_directory}".split()
                )
                .decode()
                .strip()
            )
        except Exception as exception:
            LOG.debug("Could not find scratch path because of exception: %s", exception)
        if scratch_path is not None:
            buck_builder_cache_directory = str(
                Path(scratch_path, ".buck_builder_cache")
            )
            try:
                LOG.debug(
                    "Deleting buck builder cache at %s", buck_builder_cache_directory
                )
                shutil.rmtree(buck_builder_cache_directory)
            except OSError as exception:
                LOG.debug(
                    "Failed to delete buck builder cache due to exception: %s.",
                    exception,
                )

    def _kill_processes_by_name(self, name: str) -> None:
        for process in psutil.process_iter(attrs=["name"]):
            if process.info["name"] != name:
                continue
            # Do not kill the `pyre kill` command itself.
            pid_to_kill = process.pid
            if pid_to_kill == os.getpgid(os.getpid()):
                continue
            try:
                LOG.info(
                    "Killing process {} with pid {}.".format(
                        process.info["name"], pid_to_kill
                    )
                )
                os.kill(pid_to_kill, signal.SIGKILL)
            except (ProcessLookupError, PermissionError) as exception:
                LOG.debug(
                    "Failed to kill process {} with pid {} due to exception {}".format(
                        process.info["name"], pid_to_kill, exception
                    )
                )

    def _kill_client_processes(self) -> None:
        self._kill_processes_by_name(CLIENT_NAME)
        WatchmanSubscriber.stop_subscriber(
            ProjectFilesMonitor.base_path(self._configuration), ProjectFilesMonitor.NAME
        )
        WatchmanSubscriber.stop_subscriber(
            configuration_monitor.ConfigurationMonitor.base_path(self._configuration),
            configuration_monitor.ConfigurationMonitor.NAME,
        )

    def _kill_binary_processes(self) -> None:
        # Kills all processes that have the same binary as the one specified
        # in the configuration.
        LOG.warning("Force-killing all running pyre servers.")
        LOG.warning(
            "Use `pyre servers stop` if you want to gracefully stop"
            " all running servers."
        )
        binary_name = _get_process_name("PYRE_BINARY", BINARY_NAME)
        self._kill_processes_by_name(binary_name)

    def _delete_server_files(self) -> None:
        root_log_directory = Path(self._current_directory, LOG_DIRECTORY)
        LOG.info("Deleting server files under %s", root_log_directory)
        socket_paths = root_log_directory.glob("**/server.sock")
        json_server_paths = root_log_directory.glob("**/json_server.sock")
        pid_paths = root_log_directory.glob("**/server.pid")
        for path in chain(socket_paths, json_server_paths, pid_paths):
            self._delete_linked_path(path)

    def _rage(self) -> None:
        with tempfile.NamedTemporaryFile(
            prefix="pyre-rage-", suffix=".log", delete=False
        ) as output:
            LOG.warning("Saving the output of `pyre rage` into `%s`", output.name)
            arguments = self._arguments
            arguments.output_path = output.name
            Rage(
                arguments,
                self._original_directory,
                self._configuration,
                self._analysis_directory,
            ).run()

    def _run(self) -> None:
        explicit_local = self._arguments.local_configuration
        if explicit_local:
            LOG.warning(
                "Pyre kill will terminate all running servers. "
                "Specifying local path `{}` is unnecessary.".format(explicit_local)
            )
        self._rage()
        self._kill_binary_processes()
        self._delete_server_files()
        self._delete_caches()
        self._kill_client_processes()
        if self._arguments.with_fire:
            LOG.warning(
                "Note that `--with-fire` adds emphasis to `pyre kill` but does not affect its behavior."
                f"\n{PYRE_FIRE}"
            )


def _get_process_name(environment_variable_name: str, default: str) -> str:
    overridden = os.getenv(environment_variable_name)
    if overridden is not None:
        return os.path.basename(overridden)
    else:
        return default
