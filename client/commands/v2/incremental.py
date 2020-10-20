# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging

from ... import command_arguments, commands, configuration as configuration_module


LOG: logging.Logger = logging.getLogger(__name__)


def run(
    configuration: configuration_module.Configuration,
    incremental_arguments: command_arguments.IncrementalArguments,
) -> commands.ExitCode:
    try:
        LOG.warning("Not implemented yet")

        return commands.ExitCode.SUCCESS
    except Exception as error:
        LOG.error(f"Exception occured during incremental query: {error}")
        return commands.ExitCode.FAILURE
