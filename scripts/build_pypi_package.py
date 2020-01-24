#!/usr/bin/env python3
#
# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import platform
import re
from pathlib import Path


def is_linux() -> bool:
    return platform.system() == "Linux"


def valid_typeshed(typeshed_path: str) -> str:
    path = Path(typeshed_path).absolute() / "stdlib"
    if path.is_dir():
        return typeshed_path
    raise ValueError(
        "The provided typeshed directory is not in the expected format: \
        It does not contain a 'stdlib' directory."
    )


def valid_version(version: str) -> str:
    pattern = re.compile(r"^[0-9]+\.[0-9]+\.[0-9]+$")
    if pattern.match(version):
        return version
    raise ValueError("Invalid version format.")


def main() -> None:
    parser = argparse.ArgumentParser(description="Build a PyPi Package.")
    parser.add_argument("--typeshed-path", type=valid_typeshed, required=True)
    parser.add_argument("--version", type=valid_version, required=True)

    _ = parser.parse_args()


if __name__ == "__main__":
    main()
