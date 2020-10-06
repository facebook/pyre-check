# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging

from ... import commands, configuration


LOG: logging.Logger = logging.getLogger(__name__)


def run(
    configuration: configuration.Configuration,
    terminal: bool,
    store_type_check_resolution: bool,
    no_watchman: bool,
) -> commands.ExitCode:
    LOG.warning("Not implemented yet")
    return commands.ExitCode.SUCCESS
