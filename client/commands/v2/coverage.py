# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging

from ... import commands, configuration as configuration_module
from . import remote_logging

LOG: logging.Logger = logging.getLogger(__name__)


def run_coverage(
    configuration: configuration_module.Configuration, working_directory: str
) -> commands.ExitCode:
    LOG.warning("Coming soon...")
    return commands.ExitCode.SUCCESS


@remote_logging.log_usage(command_name="coverage")
def run(
    configuration: configuration_module.Configuration, working_directory: str
) -> commands.ExitCode:
    try:
        return run_coverage(configuration, working_directory)
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during pyre coverage: {error}"
        ) from error
