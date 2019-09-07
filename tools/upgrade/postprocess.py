# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import logging
import subprocess


LOG = logging.getLogger(__name__)


def get_lint_status() -> int:
    lint_status = subprocess.call(
        [
            "arc",
            "lint",
            "--never-apply-patches",
            "--enforce-lint-clean",
            "--output",
            "none",
        ]
    )
    return lint_status


def apply_lint() -> None:
    LOG.info("Lint was dirty after file modifications. Cleaning lint and re-checking.")
    subprocess.call(["arc", "lint", "--apply-patches", "--output", "none"])
