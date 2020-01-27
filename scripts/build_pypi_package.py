#!/usr/bin/env python3
#
# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import platform
import re
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import List


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


def rsync_files(
    filters: List[str], source_directory: Path, target_directory: Path
) -> None:
    command = ["rsync", "-avm"]
    command.extend(["--filter=" + filter_string for filter_string in filters])
    command.append(str(source_directory))
    command.append(str(target_directory))
    subprocess.call(command)


def sync_python_files(build_root: str) -> None:
    target_root = Path(build_root, MODULE_NAME)
    filters = ["- tests/", "+ */", "-! *.py"]
    rsync_files(filters, CLIENT_DIRECTORY, target_root)
    rsync_files(filters, UPGRADE_DIRECTORY, target_root / "tools")


def sync_pysa_stubs(build_root: str) -> None:
    build_path = Path(build_root)
    filters = ["+ */", "-! *.pysa"]
    rsync_files(filters, STUBS_DIRECTORY / "taint", build_path)
    rsync_files(filters, STUBS_DIRECTORY / "third_party_taint", build_path)
    shutil.copy(
        STUBS_DIRECTORY / "taint/taint.config", build_path / "taint/taint.config"
    )


def sync_sapp_files(build_root: str) -> None:
    target_root = Path(build_root, MODULE_NAME)
    filters = ["- tests/", "+ */", "+ *.py", "+ *requirements.json", "- *"]
    rsync_files(filters, sapp_directory(), target_root / "tools")


def patch_version(version: str, build_root: str) -> None:
    file_contents = "__version__ = {}".format(version)
    (Path(build_root) / MODULE_NAME / "client/version.py").write_text(file_contents)


def binary_exists() -> bool:
    return (PYRE_CHECK_DIRECTORY / "_build/default/main.exe").is_file()


def sync_binary(build_root: str) -> None:
    build_path = Path(build_root)
    (build_path / "bin").mkdir()
    shutil.copy(
        PYRE_CHECK_DIRECTORY / "_build/default/main.exe", build_path / "bin/pyre.bin"
    )


def sync_documentation_files(build_root: str) -> None:
    shutil.copy(PYRE_CHECK_DIRECTORY / "README.md", build_root)
    shutil.copy(PYRE_CHECK_DIRECTORY / "LICENSE", build_root)


def main() -> None:
    parser = argparse.ArgumentParser(description="Build a PyPi Package.")
    parser.add_argument("--typeshed-path", type=valid_typeshed, required=True)
    parser.add_argument("--version", type=valid_version, required=True)

    arguments = parser.parse_args()

    if not binary_exists():
        raise ValueError(
            "The binary file does not exist. \
            Have you run 'make' in the toplevel directory?"
        )

    with tempfile.TemporaryDirectory() as build_root:
        add_init_files(build_root)
        patch_version(arguments.version, build_root)

        sync_python_files(build_root)
        sync_pysa_stubs(build_root)
        sync_sapp_files(build_root)
        sync_binary(build_root)
        sync_documentation_files(build_root)


if __name__ == "__main__":
    main()
