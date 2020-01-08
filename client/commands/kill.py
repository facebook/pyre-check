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


LOG: logging.Logger = logging.getLogger(__name__)


PYRE_FIRE = """
                              .
                             ,,,
                            .,,,,
                           ,,,,,,,,
                          ,,,*/*,,,,.
                         ,,,*///*,,,,,
                       ,,,,*/////*,,,,,
                      ,,,,////////*,,,,,
                     ,,,*/////(/////*,,, ,
                    ,,,**/////(((//*/*,,.,.
                   ,,,*/////((((((////,,,,*
                ,,.,,,/////((((((((//***/,,,
               .,,,,/*////((((((((((/////*,,.
               ,,,,*//////((((((((((/////*,,,
               ,,,,*/////((((((((((((////*,,,,,
               ,,,,,//////((((((((((/////,,,,,*
               .,,,,,*/////(((((((/////*,,,,,*.
            ##, *,,,,,,*//////////////,,,,,,**
           .,*###(/**,,,*,,,,,,,,,,,,,,,*,**/
/(##################(****///***,**,*****//*(#############//
///#####%%%%%%%%#############((####(########%%##########*//
///#%%#,     /**/((#########%#######. ############%%%%##/*/
             //*/*//#####%%..#((#(###   *######////*   ////
             ,/**///#%%#.   ,////////*    .%##/*/*//,
               .///*        .///*////*       #/*////,
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
        try:
            shutil.rmtree("/tmp/pyre/buck_builder_cache")
        except OSError:
            pass

    def _kill_client_processes(self) -> None:
        for process in psutil.process_iter(attrs=["name"]):
            if process.info["name"] != CLIENT_NAME:
                continue
            # We need to be careful about how we kill the client here, as otherwise we
            # might cause a race where we attempt to kill the `pyre kill` command.
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
            except ProcessLookupError:
                continue
        WatchmanSubscriber.stop_subscriber(
            ProjectFilesMonitor.base_path(self._configuration), ProjectFilesMonitor.NAME
        )
        WatchmanSubscriber.stop_subscriber(
            configuration_monitor.ConfigurationMonitor.base_path(self._configuration),
            configuration_monitor.ConfigurationMonitor.NAME,
        )

    @staticmethod
    def _kill_binary_processes() -> None:
        # Kills all processes that have the same binary as the one specified
        # in the configuration.
        binary_name = _get_process_name("PYRE_BINARY", BINARY_NAME)
        subprocess.run(["pkill", binary_name])

    def _delete_server_files(self) -> None:
        root_log_directory = Path(self._current_directory, LOG_DIRECTORY)
        LOG.info("Deleting server files under %s", root_log_directory)
        socket_paths = root_log_directory.glob("**/server.sock")
        json_server_paths = root_log_directory.glob("**/json_server.sock")
        pid_paths = root_log_directory.glob("**/server.pid")
        for path in chain(socket_paths, json_server_paths, pid_paths):
            self._delete_linked_path(path)

    def _run(self) -> None:
        explicit_local = self._arguments.local_configuration
        if explicit_local:
            LOG.warning(
                "Pyre kill will terminate all running servers. "
                "Specifying local path `{}` is unnecessary.".format(explicit_local)
            )
        self._kill_binary_processes()
        self._delete_server_files()
        self._delete_caches()
        self._kill_client_processes()
        if self._arguments.with_fire is True:
            LOG.warning(
                "All --with-fire functionality has now been included in `pyre kill`.\n   "
                "The flag is now a no-op, but here is a pyre for your troubles."
            )
            LOG.info(PYRE_FIRE)


def _get_process_name(environment_variable_name: str, default: str) -> str:
    overridden = os.getenv(environment_variable_name)
    if overridden is not None:
        return os.path.basename(overridden)
    else:
        return default
