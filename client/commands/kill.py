# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
import signal
import subprocess
import traceback

from .. import BINARY_NAME, SUCCESS
from ..filesystem import remove_if_exists
from .command import Command, State


LOG = logging.getLogger(__name__)


class Kill(Command):
    NAME = "kill"

    def __init__(self, arguments, configuration, analysis_directory) -> None:
        super(Kill, self).__init__(arguments, configuration, analysis_directory)

    def _run(self) -> int:
        self._kill_all_processes()
        return SUCCESS

    def _kill_all_processes(self) -> None:
        """
            Kills all processes that have the same binary as the one specified
            in the # configuration.
        """
        subprocess.run(["pkill", "-f", "{}".format(BINARY_NAME)])
