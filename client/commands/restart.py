# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging

from .. import command_arguments, configuration as configuration_module
from . import commands, incremental, stop, remote_logging


LOG: logging.Logger = logging.getLogger(__name__)


@remote_logging.log_usage(command_name="restart")
def run(
    configuration: configuration_module.Configuration,
    incremental_arguments: command_arguments.IncrementalArguments,
) -> commands.ExitCode:
    try:
        stop.run_stop(configuration)
        return incremental.run_incremental(
            configuration, incremental_arguments
        ).exit_code
    except Exception as error:
        raise commands.ClientException(
            f"Exception occurred during pyre restart: {error}"
        ) from error
