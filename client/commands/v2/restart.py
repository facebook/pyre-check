# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
from pathlib import Path

from ... import command_arguments, commands, configuration as configuration_module
from . import incremental, server_connection, start, stop


LOG: logging.Logger = logging.getLogger(__name__)


def run(
    configuration: configuration_module.Configuration,
    incremental_arguments: command_arguments.IncrementalArguments,
) -> commands.ExitCode:
    try:
        stop.run_stop(configuration)
        incremental.run_incremental(configuration, incremental_arguments)
        return commands.ExitCode.SUCCESS
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during pyre restart: {error}"
        ) from error
