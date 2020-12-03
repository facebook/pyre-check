# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import os
import platform
import re
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import List, Tuple

# just validate that it's available, but we don't invoke it directly
import wheel as _wheel  # noqa
from twine.commands.check import check as twine_check

from .setup import run as run_setup


MODULE_NAME = "pyre_check"
RUNTIME_DEPENDENCIES = [
    "click",
    "dataclasses",
    "dataclasses-json",
    "pywatchman",
    "psutil",
    "libcst>=0.3.6",
    "pyre_extensions",
]


def distribution_platform() -> str:
    if platform.system() == "Linux":
        return "-manylinux1_x86_64"
    return "-macosx_10_11_x86_64"


def validate_typeshed(typeshed_path: Path) -> None:
    path = typeshed_path.absolute() / "stdlib"
    if not path.is_dir():
        raise ValueError(
            "The provided typeshed directory is not in the expected format: \
            It does not contain a 'stdlib' directory."
        )


def validate_version(version: str) -> None:
    pattern = re.compile(r"^[0-9]+\.[0-9]+\.[0-9]+$")
    if not pattern.match(version):
        raise ValueError("Invalid version format.")


# sapp directory is either beside or inside pyre-check directory
def sapp_directory(pyre_directory: Path) -> Path:
    path = pyre_directory / "tools/sapp"
    if not path.is_dir():
        path = pyre_directory.parent / "sapp"
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
    mkdir_and_init(module_path / "tools/upgrade/commands")
    mkdir_and_init(module_path / "client")


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


def sync_python_files(pyre_directory: Path, build_root: Path) -> None:
    target_root = build_root / MODULE_NAME
    filters = ["- tests/", "+ */", "-! *.py"]
    rsync_files(filters, pyre_directory / "client", target_root, ["-avm"])
    rsync_files(
        filters, pyre_directory / "tools" / "upgrade", target_root / "tools", ["-avm"]
    )


def sync_pysa_stubs(pyre_directory: Path, build_root: Path) -> None:
    filters = ["+ */"]
    rsync_files(filters, pyre_directory / "stubs" / "taint", build_root, ["-avm"])
    rsync_files(
        filters, pyre_directory / "stubs" / "third_party_taint", build_root, ["-avm"]
    )


def sync_sapp_files(pyre_directory: Path, build_root: Path) -> None:
    filters = ["- tests/", "+ */", "+ *.py", "+ *requirements.json", "- *"]
    rsync_files(
        filters,
        sapp_directory(pyre_directory),
        build_root / MODULE_NAME / "tools",
        ["-avm"],
    )


def sync_stubs(pyre_directory: Path, build_root: Path) -> None:
    rsync_files(
        [],
        pyre_directory / "stubs",
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


def sync_typeshed(build_root: Path, typeshed_path: Path) -> None:
    typeshed_target = build_root / "typeshed"
    rsync_files(
        ["+ */", "-! *.pyi"], typeshed_path / "stdlib", typeshed_target, ["-avm"]
    )
    rsync_files(
        ["+ */", "-! *.pyi"], typeshed_path / "third_party", typeshed_target, ["-avm"]
    )
    rsync_files(
        [],
        typeshed_target,
        build_root,
        [
            "--recursive",
            "--copy-links",
            "--prune-empty-dirs",
            "--verbose",
            "--chmod='+w'",
            "--include='stdlib/***'",
            "--include='third_party/***'",
            "--exclude='*'",
        ],
    )


def patch_version(version: str, build_root: Path) -> None:
    file_contents = "__version__ = {}".format(version)
    (build_root / MODULE_NAME / "client/version.py").write_text(file_contents)


def binary_exists(pyre_directory: Path) -> bool:
    return (pyre_directory / "source" / "_build/default/main.exe").is_file()


def sync_binary(pyre_directory: Path, build_root: Path) -> None:
    (build_root / "bin").mkdir()
    shutil.copy(
        pyre_directory / "source" / "_build/default/main.exe",
        build_root / "bin/pyre.bin",
    )


def sync_documentation_files(pyre_directory: Path, build_root: Path) -> None:
    shutil.copy(pyre_directory / "README.md", build_root)
    shutil.copy(pyre_directory / "LICENSE", build_root)


def generate_setup_py(pyre_directory: Path, version: str) -> str:
    path = pyre_directory / "scripts/pypi/setup.py"
    setup_template = path.read_text()
    sapp_dependencies = (
        sapp_directory(pyre_directory) / "requirements.json"
    ).read_text()
    runtime_dependencies = json.dumps(RUNTIME_DEPENDENCIES)
    return setup_template.format(
        PACKAGE_NAME="pyre-check",
        PACKAGE_VERSION=version,
        MODULE_NAME=MODULE_NAME,
        RUNTIME_DEPENDENCIES=runtime_dependencies,
        SAPP_DEPENDENCIES=sapp_dependencies,
    )


def create_setup_cfg(build_root: Path) -> None:
    setup_cfg = build_root / "setup.cfg"
    setup_cfg.touch()
    setup_cfg.write_text("[metadata]\nlicense_file = LICENSE")


def create_setup_py(pyre_directory: Path, version: str, build_root: Path) -> None:
    setup_contents = generate_setup_py(pyre_directory, version)
    (build_root / "setup.py").write_text(setup_contents)


def run_setup_command(
    pyre_directory: Path, build_root: Path, version: str, command: str
) -> None:
    with open(pyre_directory / "README.md") as f:
        long_description = f.read()
    old_dir = os.getcwd()
    os.chdir(build_root)
    run_setup(
        package_name="pyre-check",
        package_version=version,
        module_name=MODULE_NAME,
        runtime_dependencies=RUNTIME_DEPENDENCIES,
        sapp_dependencies=json.loads(
            (sapp_directory(pyre_directory) / "requirements.json").read_text()
        ),
        long_description=long_description,
        script_name="setup.py",
        script_args=[command],
    )
    os.chdir(old_dir)


def build_distribution(pyre_directory: Path, build_root: Path, version: str) -> None:
    run_setup_command(pyre_directory, build_root, version, "sdist")


def build_wheel(pyre_directory: Path, build_root: Path, version: str) -> None:
    run_setup_command(pyre_directory, build_root, version, "bdist_wheel")


def create_dist_directory(pyre_directory: Path) -> None:
    (pyre_directory / "scripts" / "dist").mkdir(exist_ok=True)


def rename_and_move_artifacts(
    pyre_directory: Path, build_root: Path
) -> Tuple[Path, Path]:
    dist_directory = build_root / "dist"
    wheel = list(dist_directory.glob("**/*.whl"))
    source_distribution = list(dist_directory.glob("**/*.tar.gz"))
    # make sure the appropriate numbers of files are present in the dist folder
    if not len(wheel) == 1 and not len(source_distribution) == 1:
        raise ValueError("Unexpected files found in {}/dist.".format(build_root))
    source_distribution, wheel = source_distribution[0], wheel[0]
    destination_path = pyre_directory / "scripts" / "dist"
    source_distribution_name = source_distribution.name
    source_distribution_destination = destination_path / (
        source_distribution_name.split(".tar.gz")[0]
        + distribution_platform()
        + ".tar.gz"
    )
    wheel_name = wheel.name
    wheel_destination = destination_path / wheel_name.replace(
        "-any", distribution_platform()
    )
    shutil.move(wheel, wheel_destination)
    shutil.move(source_distribution, source_distribution_destination)
    return wheel_destination, source_distribution_destination


def run(pyre_directory: Path, typeshed_path: Path, version: str) -> None:
    validate_typeshed(typeshed_path)
    validate_version(version)
    if not binary_exists(pyre_directory):
        raise ValueError(
            "The binary file does not exist. \
            Have you run 'make' in the toplevel directory?"
        )

    with tempfile.TemporaryDirectory() as build_root:
        build_path = Path(build_root)
        add_init_files(build_path)
        patch_version(version, build_path)
        create_setup_py(pyre_directory, version, build_path)

        sync_python_files(pyre_directory, build_path)
        sync_pysa_stubs(pyre_directory, build_path)
        sync_stubs(pyre_directory, build_path)
        sync_sapp_files(pyre_directory, build_path)
        sync_typeshed(build_path, typeshed_path)
        sync_binary(pyre_directory, build_path)
        sync_documentation_files(pyre_directory, build_path)
        sync_stubs(pyre_directory, build_path)

        build_distribution(pyre_directory, build_path, version)
        create_dist_directory(pyre_directory)
        create_setup_cfg(build_path)
        twine_check([path.as_posix() for path in (build_path / "dist").iterdir()])
        build_wheel(pyre_directory, build_path, version)

        wheel_destination, distribution_destination = rename_and_move_artifacts(
            pyre_directory, build_path
        )
        print("\nAll done.")
        print("\n Build artifact available at:\n {}\n".format(wheel_destination))
        print(
            "\n Source distribution available at:\n {}\n".format(
                distribution_destination
            )
        )
