# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
import subprocess

from .. import BINARY_NAME
from .command import Command


LOG = logging.getLogger(__name__)


class Kill(Command):
    NAME = "kill"

    def __init__(self, arguments, configuration, analysis_directory) -> None:
        super(Kill, self).__init__(arguments, configuration, analysis_directory)

    def _run(self) -> None:
        # Kills all processes that have the same binary as the one specified
        # in the configuration.
        subprocess.run(["pkill", "{}".format(BINARY_NAME)])
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
