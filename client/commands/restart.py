# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module provides the logic for the `pyre restart` command, which
is a single command that effectively runs `pyre stop` followed by
`pyre incremental`.
"""


import logging

from .. import command_arguments, frontend_configuration, identifiers
from . import commands, incremental, stop


LOG: logging.Logger = logging.getLogger(__name__)


def run(
    configuration: frontend_configuration.Base,
    incremental_arguments: command_arguments.IncrementalArguments,
) -> commands.ExitCode:
    stop.run(configuration, flavor=identifiers.PyreFlavor.CLASSIC)
    return incremental.run_incremental(configuration, incremental_arguments).exit_code
