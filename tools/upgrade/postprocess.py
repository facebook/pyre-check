# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import logging
import subprocess
from logging import Logger


LOG: Logger = logging.getLogger(__name__)


def get_lint_status() -> int:
    lint_status = subprocess.run(
        [
            "arc",
            "lint",
            "--never-apply-patches",
            "--enforce-lint-clean",
            "--output",
            "none",
        ]
    )
    return lint_status.returncode


def apply_lint() -> None:
    LOG.info("Lint was dirty after file modifications. Cleaning lint and re-checking.")
    subprocess.run(["arc", "lint", "--apply-patches", "--output", "none"])
