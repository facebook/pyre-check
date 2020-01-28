#!/usr/bin/env python3
#
# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import json
import platform
import re
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import List, Tuple


MODULE_NAME = "pyre_check"
RUNTIME_DEPENDENCIES = ["typeshed", "pywatchman", "psutil", "libcst", "pyre_extensions"]

SCRIPTS_DIRECTORY: Path = Path(__file__).parent
PYRE_CHECK_DIRECTORY: Path = SCRIPTS_DIRECTORY.parent
STUBS_DIRECTORY: Path = PYRE_CHECK_DIRECTORY / "stubs"
CLIENT_DIRECTORY: Path = PYRE_CHECK_DIRECTORY / "client"
UPGRADE_DIRECTORY: Path = PYRE_CHECK_DIRECTORY / "tools/upgrade"


def distribution_platform() -> str:
    if platform.system() == "Linux":
        return "-manylinux1_x86_64"
    return "-macosx_10_11_x86_64"


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


def add_init_files(build_root: Path) -> None:
    # setup.py sdist will refuse to work for directories without a `__init__.py`.
    module_path = build_root / MODULE_NAME
    mkdir_and_init(module_path)
    mkdir_and_init(module_path / "tools")
    mkdir_and_init(module_path / "tools/upgrade")
    (module_path / "client").mkdir()


def rsync_files(
    filters: List[str],
    source_directory: Path,
    target_directory: Path,
    arguments: List[str],
) -> None:
    command = ["rsync"]
    command.extend(arguments)
    command.extend(["--filter=" + filter_string for filter_string in filters])
    command.append(str(source_directory))
    command.append(str(target_directory))
    subprocess.run(command)


def sync_python_files(build_root: Path) -> None:
    target_root = build_root / MODULE_NAME
    filters = ["- tests/", "+ */", "-! *.py"]
    rsync_files(filters, CLIENT_DIRECTORY, target_root, ["-avm"])
    rsync_files(filters, UPGRADE_DIRECTORY, target_root / "tools", ["-avm"])


def sync_pysa_stubs(build_root: Path) -> None:
    filters = ["+ */", "-! *.pysa"]
    rsync_files(filters, STUBS_DIRECTORY / "taint", build_root, ["-avm"])
    rsync_files(filters, STUBS_DIRECTORY / "third_party_taint", build_root, ["-avm"])
    shutil.copy(
        STUBS_DIRECTORY / "taint/taint.config", build_root / "taint/taint.config"
    )


def sync_sapp_files(build_root: Path) -> None:
    filters = ["- tests/", "+ */", "+ *.py", "+ *requirements.json", "- *"]
    rsync_files(filters, sapp_directory(), build_root / MODULE_NAME / "tools", ["-avm"])


def sync_stubs(build_root: Path) -> None:
    rsync_files(
        [],
        STUBS_DIRECTORY,
        build_root,
        [
            "--recursive",
            "--copy-links",
            "--prune-empty-dirs",
            "--verbose",
            "--include='django/***'",
            "--include='lxml/***'",
            "--exclude='*'",
        ],
    )


def patch_version(version: str, build_root: Path) -> None:
    file_contents = "__version__ = {}".format(version)
    (build_root / MODULE_NAME / "client/version.py").write_text(file_contents)


def binary_exists() -> bool:
    return (PYRE_CHECK_DIRECTORY / "_build/default/main.exe").is_file()


def sync_binary(build_root: Path) -> None:
    (build_root / "bin").mkdir()
    shutil.copy(
        PYRE_CHECK_DIRECTORY / "_build/default/main.exe", build_root / "bin/pyre.bin"
    )


def sync_documentation_files(build_root: Path) -> None:
    shutil.copy(PYRE_CHECK_DIRECTORY / "README.md", build_root)
    shutil.copy(PYRE_CHECK_DIRECTORY / "LICENSE", build_root)


def generate_setup_py(version: str) -> str:
    path = PYRE_CHECK_DIRECTORY / "scripts/setup_template.py"
    setup_template = path.read_text()
    sapp_dependencies = json.loads((sapp_directory() / "requirements.json").read_text())
    return setup_template.format(
        PACKAGE_NAME="pyre-check",
        PACKAGE_VERSION=version,
        MODULE_NAME=MODULE_NAME,
        RUNTIME_DEPENDENCIES=RUNTIME_DEPENDENCIES,
        SAPP_DEPENDENCIES=sapp_dependencies,
    )


def create_setup_cfg(build_root: Path) -> None:
    setup_cfg = build_root / "setup.cfg"
    setup_cfg.touch()
    setup_cfg.write_text("[metadata]\nlicense_file = LICENSE")


def create_setup_py(version: str, build_root: Path) -> None:
    setup_contents = generate_setup_py(version)
    (build_root / "setup.py").write_text(setup_contents)


def build_distribution(build_root: Path) -> None:
    subprocess.run(["python3", "setup.py", "sdist"], cwd=build_root)


def build_wheel(build_root: Path) -> None:
    subprocess.run(["python3", "setup.py", "bdist_wheel"], cwd=build_root)


def create_dist_directory() -> None:
    (SCRIPTS_DIRECTORY / "dist").mkdir(exist_ok=True)


def rename_and_move_artifacts(build_root: Path) -> Tuple[Path, Path]:
    dist_directory = build_root / "dist"
    wheel = list(dist_directory.glob("**/*.whl"))
    source_distribution = list(dist_directory.glob("**/*.tar.gz"))
    # make sure the appropriate numbers of files are present in the dist folder
    if not len(wheel) == 1 and not len(source_distribution) == 1:
        raise ValueError("Unexpected files found in {}/dist.".format(build_root))
    source_distribution, wheel = source_distribution[0], wheel[0]
    destination_path = SCRIPTS_DIRECTORY / "dist"
    source_distribution_name = source_distribution.name
    source_distribution_destination = destination_path / (
        source_distribution_name.split(".tar.gz")[0]
        + distribution_platform()
        + ".tar.gz"
    )
    wheel_name = wheel.name
    wheel_destination = destination_path / wheel_name.replace(
        "-none-any", distribution_platform()
    )
    shutil.move(wheel, wheel_destination)
    shutil.move(source_distribution, source_distribution_destination)
    return wheel_destination, source_distribution_destination


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
        build_path = Path(build_root)
        add_init_files(build_path)
        patch_version(arguments.version, build_path)
        create_setup_py(arguments.version, build_path)

        sync_python_files(build_path)
        sync_pysa_stubs(build_path)
        sync_stubs(build_path)
        sync_sapp_files(build_path)
        sync_binary(build_path)
        sync_documentation_files(build_path)
        sync_stubs(build_path)

        build_distribution(build_path)
        create_dist_directory()
        create_setup_cfg(build_path)
        subprocess.run(["twine", "check", build_path / "dist"])
        build_wheel(build_path)

        wheel_destination, distribution_destination = rename_and_move_artifacts(
            build_path
        )
        print("\nAll done.")
        print("\n Build artifact available at:\n {}\n".format(wheel_destination))
        print(
            "\n Source distribution available at:\n {}\n".format(
                distribution_destination
            )
        )


if __name__ == "__main__":
    main()
