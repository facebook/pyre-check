# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import os
import shutil
import subprocess
from logging import Logger
from pathlib import Path
from typing import Optional

from .find_directories import (
    CONFIGURATION_FILE,
    LOCAL_CONFIGURATION_FILE,
    find_global_root,
)


TEST_DIRECTORY = "__TEST_DIRECTORY"
LOG_DIRECTORY: str = ".pyre"

LOG: Logger = logging.getLogger(__name__)


def find_log_directory(
    project_root: str, local_configuration: Optional[str], dot_pyre_directory: str
) -> str:
    """Pyre outputs all logs to a .pyre directory that lives in the project root."""
    log_directory = dot_pyre_directory
    if local_configuration:
        # `log_directory` will never escape `.pyre/` because in `switch_root` we have
        # guaranteed that configurations are never deeper than local configurations
        relative = os.path.relpath(local_configuration, project_root)
        log_directory = os.path.join(log_directory, relative)
    return log_directory


def get_dot_pyre_directory(root_directory: str) -> str:
    dot_pyre_directory = os.path.join(root_directory, LOG_DIRECTORY)
    try:
        scratch_path = (
            subprocess.check_output(["mkscratch", "path", root_directory])
            .decode()
            .strip()
        )
    except Exception as exception:
        LOG.debug("Could not find scratch path because of exception: %s", exception)
    else:
        # Test if we have permission to create a nested directory.
        test_outer_directory = os.path.join(scratch_path, TEST_DIRECTORY)
        test_scratch_directory = os.path.join(test_outer_directory, TEST_DIRECTORY)
        try:
            os.makedirs(test_scratch_directory, exist_ok=True)
        except PermissionError:
            LOG.debug(
                "Did not have permission to write to the dot pyre directory in scratch."
                " Falling back to a dot pyre directory in the project root."
            )
        except Exception as exception:
            LOG.debug(
                f"Could not test if the dot pyre directory is writable:\n{exception}\n"
                "Falling back to a dot pyre directory in the project root."
            )
        else:
            dot_pyre_directory = os.path.join(scratch_path, LOG_DIRECTORY)
        finally:
            try:
                shutil.rmtree(test_outer_directory)
            except Exception:
                pass
    return dot_pyre_directory


def log_directory(
    project_root: str,
    local_root: Optional[str] = None,
    subdirectory: Optional[str] = None,
) -> Path:
    dot_pyre = get_dot_pyre_directory(root_directory=project_root)
    if local_root is None:
        return Path(dot_pyre)
    project_root_path = find_global_root(Path(project_root)) or Path(project_root)
    log_directory = Path(
        find_log_directory(
            dot_pyre_directory=dot_pyre,
            project_root=str(project_root_path),
            local_configuration=local_root,
        )
    )
    if subdirectory is None:
        return log_directory
    return log_directory / subdirectory


def get_configuration_value(directory: str, key: str) -> str:
    local_configuration = Path(directory) / LOCAL_CONFIGURATION_FILE
    with open(local_configuration) as file:
        local_configuration = json.load(file)

    if local_configuration.get(key):
        return local_configuration.get(key)
    else:
        project_root = find_global_root(Path(directory)) or Path(directory)
        project_configuration = project_root / CONFIGURATION_FILE
        with open(project_configuration) as file:
            project_configuration = json.load(file)
        return project_configuration.get(key)
