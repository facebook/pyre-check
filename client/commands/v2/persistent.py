# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging

from ... import (
    command_arguments,
    commands,
    configuration as configuration_module,
)

LOG: logging.Logger = logging.getLogger(__name__)


def run(
    configuration: configuration_module.Configuration,
    start_arguments: command_arguments.StartArguments,
) -> commands.ExitCode:
    LOG.warning("The best is yet to come...")
    return commands.ExitCode.SUCCESS
