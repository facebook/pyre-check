# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
from pathlib import Path

from ... import commands, configuration as configuration_module
from . import remote_logging, statistics

LOG: logging.Logger = logging.getLogger(__name__)


def find_root(
    configuration: configuration_module.Configuration, working_directory: Path
) -> Path:
    local_root = configuration.local_root
    if local_root is not None:
        return Path(local_root)

    return working_directory


def run_coverage(
    configuration: configuration_module.Configuration, working_directory: str
) -> commands.ExitCode:
    sources = statistics.find_paths_to_parse(
        [find_root(configuration, Path(working_directory))]
    )
    LOG.warning(f"SOURCES = {list(sources)}")
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
