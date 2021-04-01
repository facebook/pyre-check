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
                + " Falling back to a dot pyre directory in the project root."
            )
        except Exception as exception:
            LOG.debug(
                f"Could not test if the dot pyre directory is writable:\n{exception}\n"
                + "Falling back to a dot pyre directory in the project root."
            )
        else:
            dot_pyre_directory = os.path.join(scratch_path, LOG_DIRECTORY)
        finally:
            try:
                shutil.rmtree(test_outer_directory)
            except Exception:
                pass
    return dot_pyre_directory
