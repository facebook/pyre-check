# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
from pathlib import Path

from ... import commands, configuration as configuration_module
from . import server_connection


LOG: logging.Logger = logging.getLogger(__name__)


def run(
    configuration: configuration_module.Configuration, query: str
) -> commands.ExitCode:
    socket_path = server_connection.get_default_socket_path(
        log_directory=Path(configuration.log_directory)
    )
    try:
        LOG.warning("Not implemented yet!")
        return commands.ExitCode.SUCCESS
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during pyre query: {error}"
        ) from error
