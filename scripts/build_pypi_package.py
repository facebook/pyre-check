#!/usr/bin/env python3
#
# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import platform
import re
import tempfile
from pathlib import Path


MODULE_NAME = "pyre_check"


PYRE_CHECK_DIRECTORY: Path = Path(__file__).parent.parent
STUBS_DIRECTORY: Path = PYRE_CHECK_DIRECTORY / "stubs"
CLIENT_DIRECTORY: Path = PYRE_CHECK_DIRECTORY / "client"
UPGRADE_DIRECTORY: Path = PYRE_CHECK_DIRECTORY / "tools/upgrade"


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


# sapp directory is either beside or inside pyre-check directory
def sapp_directory() -> Path:
    path = PYRE_CHECK_DIRECTORY / "tools/sapp"
    if not path.is_dir():
        path = PYRE_CHECK_DIRECTORY.parent / "sapp"
    return path


def mkdir_and_init(module_path: Path) -> None:
    module_path.mkdir()
    (module_path / "__init__.py").touch()


def add_init_files(build_root: str) -> None:
    # setup.py sdist will refuse to work for directories without a `__init__.py`.
    build_path = Path(build_root)
    module_path = build_path / MODULE_NAME
    mkdir_and_init(module_path)
    mkdir_and_init(module_path / "tools")
    mkdir_and_init(module_path / "tools/upgrade")
    (module_path / "client").mkdir()


def main() -> None:
    parser = argparse.ArgumentParser(description="Build a PyPi Package.")
    parser.add_argument("--typeshed-path", type=valid_typeshed, required=True)
    parser.add_argument("--version", type=valid_version, required=True)

    _ = parser.parse_args()
    with tempfile.TemporaryDirectory() as build_root:
        add_init_files(build_root)


if __name__ == "__main__":
    main()
