# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module provides the logic for the `pyre restart` command, which
is a single command that effectively runs `pyre stop` followed by
`pyre incremental`.
"""


import logging

from .. import command_arguments, configuration as configuration_module, identifiers
from . import commands, frontend_configuration, incremental, stop


LOG: logging.Logger = logging.getLogger(__name__)


def run(
    configuration: configuration_module.Configuration,
    incremental_arguments: command_arguments.IncrementalArguments,
) -> commands.ExitCode:
    restart_configuration = frontend_configuration.OpenSource(configuration)
    stop.run_stop(restart_configuration, flavor=identifiers.PyreFlavor.CLASSIC)
    return incremental.run_incremental(
        restart_configuration, incremental_arguments
    ).exit_code
