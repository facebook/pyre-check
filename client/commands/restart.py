# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import logging

from .. import command_arguments, configuration as configuration_module
from . import commands, frontend_configuration, incremental, stop


LOG: logging.Logger = logging.getLogger(__name__)


def run(
    configuration: configuration_module.Configuration,
    incremental_arguments: command_arguments.IncrementalArguments,
) -> commands.ExitCode:
    restart_configuration = frontend_configuration.OpenSource(configuration)
    stop.run_stop(restart_configuration)
    return incremental.run_incremental(
        restart_configuration, incremental_arguments
    ).exit_code
