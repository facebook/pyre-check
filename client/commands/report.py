# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module provides the entrypoint for `pyre report`, a command to
collect data about code and how well Pyre can understand its types.

"""
import logging
from pathlib import Path
from typing import List, Optional

from .. import (
    configuration as configuration_module,
    coverage_data,
    frontend_configuration,
)
from . import commands

LOG: logging.Logger = logging.getLogger(__name__)


def get_module_paths(
    configuration: frontend_configuration.Base, paths: Optional[List[Path]]
) -> List[Path]:
    return list(
        coverage_data.find_module_paths(
            coverage_data.get_paths_to_collect(
                paths,
                root=(
                    configuration.get_local_root() or configuration.get_global_root()
                ),
            ),
            excludes=configuration.get_excludes(),
        )
    )


def run(
    raw_configuration: configuration_module.Configuration,
    paths: Optional[List[Path]],
) -> int:
    configuration = frontend_configuration.OpenSource(raw_configuration)
    module_paths = get_module_paths(
        configuration=configuration,
        paths=paths,
    )
    LOG.warning("`pyre report` is not yet fully implemented.")
    LOG.info(f"Module paths: {list(module_paths)}")
    return commands.ExitCode.SUCCESS
