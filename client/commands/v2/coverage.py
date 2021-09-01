# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import json
import logging
import os
from pathlib import Path
from typing import Optional, Iterable, List

from ... import (
    commands,
    configuration as configuration_module,
    coverage_collector as collector,
    log,
)
from . import remote_logging, statistics

LOG: logging.Logger = logging.getLogger(__name__)


def find_root(
    configuration: configuration_module.Configuration, working_directory: Path
) -> Path:
    local_root = configuration.local_root
    if local_root is not None:
        return Path(local_root)

    return working_directory


def collect_coverage_for_path(
    path: Path, working_directory: str
) -> Optional[collector.FileCoverage]:
    module = statistics.parse_path_to_module(path)
    relative_path = os.path.relpath(str(path), working_directory)
    return (
        collector.collect_coverage_for_module(relative_path, module)
        if module is not None
        else None
    )


def collect_coverage_for_paths(
    paths: Iterable[Path], working_directory: str
) -> List[collector.FileCoverage]:
    result: List[collector.FileCoverage] = []
    for path in paths:
        coverage = collect_coverage_for_path(path, working_directory)
        if coverage is not None:
            result.append(coverage)
    return result


def run_coverage(
    configuration: configuration_module.Configuration, working_directory: str
) -> commands.ExitCode:
    data = collect_coverage_for_paths(
        statistics.find_paths_to_parse(
            [find_root(configuration, Path(working_directory))]
        ),
        working_directory,
    )
    log.stdout.write(json.dumps([dataclasses.asdict(entry) for entry in data]))
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
