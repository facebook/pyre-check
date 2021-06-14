# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging

from ... import commands, configuration as configuration_module
from . import remote_logging


LOG: logging.Logger = logging.getLogger(__name__)


def run_profile(
    configuration: configuration_module.Configuration, output: commands.ProfileOutput
) -> commands.ExitCode:
    LOG.info("Coming soon...")
    return commands.ExitCode.SUCCESS


@remote_logging.log_usage(command_name="profile")
def run(
    configuration: configuration_module.Configuration, output: commands.ProfileOutput
) -> commands.ExitCode:
    try:
        return run_profile(configuration, output)
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during profile: {error}"
        ) from error
