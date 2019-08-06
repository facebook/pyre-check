# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
# pyre-strict

import argparse
import logging
import os
import shutil
import signal
import subprocess
from typing import Iterable  # noqa

from .. import BINARY_NAME, CLIENT_NAME
from ..configuration import Configuration
from ..filesystem import AnalysisDirectory
from .command import Command


LOG = logging.getLogger(__name__)  # type: logging.Logger


class Kill(Command):
    NAME = "kill"

    def __init__(
        self,
        arguments: argparse.Namespace,
        configuration: Configuration,
        analysis_directory: AnalysisDirectory,
    ) -> None:
        super(Kill, self).__init__(arguments, configuration, analysis_directory)

    def _run(self) -> None:
        # Kills all processes that have the same binary as the one specified
        # in the configuration.
        binary_name = _get_process_name("PYRE_BINARY", BINARY_NAME)
        subprocess.run(["pkill", binary_name])
        server_root = os.path.join(
            os.path.realpath(self._analysis_directory.get_root()), ".pyre", "server"
        )

        def _delete_linked_path(link_path: str) -> None:
            try:
                actual_path = os.readlink(link_path)
                os.remove(actual_path)
            except OSError:
                pass
            try:
                os.unlink(link_path)
            except OSError:
                pass

        _delete_linked_path(os.path.join(server_root, "server.sock"))
        _delete_linked_path(os.path.join(server_root, "json_server.sock"))

        if self._arguments.with_fire is True:
            # If a resource cache exists, delete it to remove corrupted artifacts.
            try:
                shutil.rmtree(os.path.join(os.getcwd(), ".pyre/resource_cache"))
            except OSError:
                pass

        # We need to be careful about how we kill the client here, as otherwise we might
        # cause a race where we attempt to kill the `pyre kill` command.
        process_ids = (
            subprocess.run(
                ["pgrep", _get_process_name("PYRE_CLIENT", CLIENT_NAME)],
                stdout=subprocess.PIPE,
            )
            .stdout.decode("utf-8")
            .split()
        )
        for process_id in process_ids:
            id = int(process_id)
            if id == os.getpid():
                continue
            os.kill(os.getpgid(id), signal.SIGKILL)


def _get_process_name(environment_variable_name: str, default: str) -> str:
    overridden = os.getenv(environment_variable_name)
    if overridden is not None:
        return os.path.basename(overridden)
    else:
        return default
