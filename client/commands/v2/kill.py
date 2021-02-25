# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging

from ... import commands, configuration as configuration_module
from . import remote_logging

LOG: logging.Logger = logging.getLogger(__name__)


@remote_logging.log_usage(command_name="kill")
def run(
    configuration: configuration_module.Configuration, with_fire: bool
) -> commands.ExitCode:
    try:
        LOG.warning("Not implemented yet")
        return commands.ExitCode.SUCCESS
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during `pyre kill`: {error}"
        ) from error
