# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging

from ... import commands, command_arguments, configuration as configuration_module
from . import remote_logging


LOG: logging.Logger = logging.getLogger(__name__)


def run_statistics(
    configuration: configuration_module.Configuration,
    statistics_arguments: command_arguments.StatisticsArguments,
) -> commands.ExitCode:
    LOG.warning("Coming soon...")
    return commands.ExitCode.SUCCESS


@remote_logging.log_usage(command_name="statistics")
def run(
    configuration: configuration_module.Configuration,
    statistics_arguments: command_arguments.StatisticsArguments,
) -> commands.ExitCode:
    try:
        return run_statistics(configuration, statistics_arguments)
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during statistics collection: {error}"
        ) from error
