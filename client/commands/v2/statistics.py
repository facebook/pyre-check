# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
from pathlib import Path
from typing import Iterable

from ... import commands, command_arguments, configuration as configuration_module
from . import remote_logging


LOG: logging.Logger = logging.getLogger(__name__)


def find_roots(
    configuration: configuration_module.Configuration,
    statistics_arguments: command_arguments.StatisticsArguments,
) -> Iterable[Path]:
    filter_paths = statistics_arguments.filter_paths
    if len(filter_paths) > 0:

        def to_absolute_path(given: str) -> Path:
            path = Path(given)
            return path if path.is_absolute() else Path.cwd() / path

        return {to_absolute_path(path) for path in filter_paths}

    local_root = configuration.local_root
    if local_root is not None:
        return [Path(local_root)]

    return [Path.cwd()]


def run_statistics(
    configuration: configuration_module.Configuration,
    statistics_arguments: command_arguments.StatisticsArguments,
) -> commands.ExitCode:
    LOG.warning("Coming soon...")
    return commands.ExitCode.SUCCESS


@remote_logging.log_usage(command_name="statistics")
def run(
    configuration: configuration_module.Configuration,
    statistics_arguments: command_arguments.StatisticsArguments,
) -> commands.ExitCode:
    try:
        return run_statistics(configuration, statistics_arguments)
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during statistics collection: {error}"
        ) from error
