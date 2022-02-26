#! /usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import logging
import subprocess
import sys

LOG = logging.getLogger(__name__)


def _changed_paths() -> list[str]:
    is_public = (
        subprocess.check_output(["git", "branch", "--remote", "--contains", "HEAD"])
        .decode()
        .strip()
        == ""
    )
    paths = (
        subprocess.check_output(
            ["git", "diff", "--name-only", "--staged", "HEAD^" if is_public else "HEAD"]
        )
        .decode()
        .split()
    )
    if paths == [""]:
        return []
    return paths


def _lint_python(all_paths: list[str]) -> None:
    paths = [path for path in all_paths if path.endswith(".py")]
    LOG.info("Formatting...")
    subprocess.check_call(["black", *paths])

    LOG.info("Linting...")
    output = subprocess.check_output(["flake8", "--no-py2", *paths]).decode()
    if output != "":
        print(output.rstrip())
        sys.exit(1)
    else:
        LOG.info("Done")


if __name__ == "__main__":
    logging.basicConfig(
        format="%(asctime)s [%(levelname)s] %(message)s", level=logging.DEBUG
    )

    paths = _changed_paths()
    LOG.info(f"Changed paths: {', '.join(paths)}")

    _lint_python(paths)
