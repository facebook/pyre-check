# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
TODO(T132414938) Add a module-level docstring
"""

import dataclasses
import json
import logging
import os
import platform
import re
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import List, Optional, Sequence, Tuple

# just validate that it's available, but we don't invoke it directly
import wheel as _wheel  # noqa
from twine.commands.check import check as twine_check

from .setup import run as run_setup

MODULE_NAME = "pyre_check"
EXPECTED_LD_PATH = "/lib64/ld-linux-x86-64.so.2"

LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class BuildArtifacts:
    wheel_path: Path
    source_distribution_path: Path


def get_source_distribution_and_wheel(artifact_directory: Path) -> BuildArtifacts:
    wheel = list(artifact_directory.glob("**/*.whl"))
    source_distribution = list(artifact_directory.glob("**/*.tar.gz"))
    # make sure the appropriate numbers of files are present in the dist folder
    if not len(wheel) == 1 and not len(source_distribution) == 1:
        raise ValueError(f"Unexpected files found in {artifact_directory}.")
    return BuildArtifacts(
        source_distribution_path=source_distribution[0], wheel_path=wheel[0]
    )


def _distribution_platform() -> str:
    system = platform.system()
    if system == "Linux":
        # Currently we only ever build on Intel Linux machines.
        return "-manylinux1_x86_64"
    elif system == "Darwin":
        if "arm" in platform.processor():
            # This means we are on Apple Silicon machines.
            # The minimum possible arm64 Mac version for pip is 11.0.
            return "-macosx_11_0_arm64"
        return "-macosx_10_11_x86_64"
    else:
        raise RuntimeError(f"Building on platform `{system}` is not supported.")


def _validate_typeshed(typeshed_path: Path) -> None:
    path = typeshed_path.absolute() / "stdlib"
    if not path.is_dir():
        raise ValueError(
            "The provided typeshed directory is not in the expected format: \
            It does not contain a 'stdlib' directory."
        )


def _validate_version(version: str) -> None:
    pattern = re.compile(r"^[0-9]+\.[0-9]+\.[0-9]+$")
    if not pattern.match(version):
        raise ValueError("Invalid version format.")


def _mkdir_and_init(module_path: Path, version: Optional[str] = None) -> None:
    module_path.mkdir()
    init_path = module_path / "__init__.py"
    if version is None:
        init_path.touch()
    else:
        init_path.write_text(
            f"""\
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

__version__ = "{version}"
"""
        )


def _add_init_files(build_root: Path, version: str) -> None:
    # setup.py sdist will refuse to work for directories without a `__init__.py`.
    module_path = build_root / MODULE_NAME
    _mkdir_and_init(module_path, version)
    _mkdir_and_init(module_path / "tools")
    _mkdir_and_init(module_path / "tools/upgrade")
    _mkdir_and_init(module_path / "tools/upgrade/commands")
    _mkdir_and_init(module_path / "client")


def _rsync_files(
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


def _sync_python_files(pyre_directory: Path, build_root: Path) -> None:
    target_root = build_root / MODULE_NAME
    filters = ["- tests/", "+ */", "-! *.py"]
    _rsync_files(filters, pyre_directory / "client", target_root, ["-avm"])
    _rsync_files(
        filters, pyre_directory / "tools" / "upgrade", target_root / "tools", ["-avm"]
    )


def _sync_pysa_stubs(pyre_directory: Path, build_root: Path) -> None:
    filters = ["+ */"]
    _rsync_files(filters, pyre_directory / "stubs" / "taint", build_root, ["-avm"])
    _rsync_files(
        filters, pyre_directory / "stubs" / "third_party_taint", build_root, ["-avm"]
    )


def _sync_stubs(pyre_directory: Path, build_root: Path) -> None:
    _rsync_files(
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


def _sync_sapp_filters(pyre_directory: Path, build_root: Path) -> None:
    _rsync_files(
        [],
        pyre_directory / "tools" / "sapp" / "pysa_filters",
        build_root,
        [
            "--recursive",
            "--prune-empty-dirs",
            "--verbose",
        ],
    )


def _sync_typeshed(build_root: Path, typeshed_path: Path) -> None:
    typeshed_target = build_root / "typeshed"
    _rsync_files(
        ["+ */", "-! *.pyi"], typeshed_path / "stdlib", typeshed_target, ["-avm"]
    )
    _rsync_files(
        ["+ */", "-! *.pyi"], typeshed_path / "stubs", typeshed_target, ["-avm"]
    )
    _rsync_files(
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
            "--exclude='*'",
        ],
    )


def _patch_version(version: str, build_root: Path) -> None:
    file_contents = f'__version__ = "{version}"'
    (build_root / MODULE_NAME / "client/version.py").write_text(file_contents)


def _ensure_usable_binary_exists(pyre_directory: Path) -> None:
    binary_path = pyre_directory / "source" / "_build/default/main.exe"
    if not binary_path.is_file():
        raise ValueError(
            "The binary file does not exist. \
            Have you run 'make' in the toplevel directory?"
        )
    result = subprocess.run(
        ["file", str(binary_path)],
        stdout=subprocess.PIPE,
        encoding="utf-8",
    )
    if "dynamically linked" in result.stdout and EXPECTED_LD_PATH not in result.stdout:
        raise ValueError(
            "The built executable appears to include an unreleasable ld path. "
            f"The output of running `file` on it was {result.stdout}"
        )


def _sync_binary(pyre_directory: Path, build_root: Path) -> None:
    (build_root / "bin").mkdir()
    shutil.copy(
        pyre_directory / "source" / "_build/default/main.exe",
        build_root / "bin/pyre.bin",
    )


def _strip_binary(build_root: Path) -> None:
    binary_path = build_root / "bin/pyre.bin"
    result = subprocess.run(["strip", str(binary_path)])
    if result.returncode != 0:
        LOG.warning("Unable to strip debugging info from binary.")


def _sync_documentation_files(pyre_directory: Path, build_root: Path) -> None:
    shutil.copy(pyre_directory / "README.md", build_root)
    shutil.copy(pyre_directory / "LICENSE", build_root)


def _create_setup_configuration(build_root: Path) -> None:
    setup_cfg = build_root / "setup.cfg"
    setup_cfg.touch()
    setup_cfg.write_text("[metadata]\nlicense_file = LICENSE")


def _create_setup_py(
    pyre_directory: Path,
    version: str,
    build_root: Path,
    dependencies: Sequence[str],
    nightly: bool,
) -> None:
    path = pyre_directory / "scripts/pypi/setup.py"
    setup_template = path.read_text()
    setup_contents = setup_template.format(
        PACKAGE_NAME="pyre-check-nightly" if nightly else "pyre-check",
        PACKAGE_VERSION=version,
        MODULE_NAME=MODULE_NAME,
        RUNTIME_DEPENDENCIES=json.dumps(dependencies),
    )

    (build_root / "setup.py").write_text(setup_contents)


def _run_setup_command(
    pyre_directory: Path,
    build_root: Path,
    dependencies: Sequence[str],
    version: str,
    command: str,
    nightly: bool,
) -> None:
    with open(pyre_directory / "README.md") as f:
        long_description = f.read()
    old_dir = os.getcwd()
    os.chdir(build_root)
    run_setup(
        package_name="pyre-check-nightly" if nightly else "pyre-check",
        package_version=version,
        module_name=MODULE_NAME,
        runtime_dependencies=dependencies,
        long_description=long_description,
        script_name="setup.py",
        script_args=[command],
    )
    os.chdir(old_dir)


def _rename_and_move_artifacts(
    build_root: Path, output_directory: Path
) -> Tuple[Path, Path]:
    dist_directory = build_root / "dist"
    build_artifacts = get_source_distribution_and_wheel(dist_directory)
    source_distribution = build_artifacts.source_distribution_path
    wheel = build_artifacts.wheel_path

    source_distribution_destination = output_directory / (
        source_distribution.name.split(".tar.gz")[0]
        + _distribution_platform()
        + ".tar.gz"
    )
    wheel_destination = output_directory / wheel.name.replace(
        "-any", _distribution_platform()
    )

    LOG.info(f"Moving wheel from {str(wheel)} to {str(wheel_destination)}")
    wheel.replace(wheel_destination)
    LOG.info(
        f"Moving source distribution from {str(source_distribution)} to {str(source_distribution_destination)}"
    )
    source_distribution.replace(source_distribution_destination)

    return source_distribution_destination, wheel_destination


def build_pypi_package(
    pyre_directory: Path,
    typeshed_path: Path,
    version: str,
    nightly: bool,
    output_directory: Optional[Path] = None,
) -> None:
    if output_directory is None:
        output_directory = pyre_directory / "scripts" / "dist"
    LOG.info(f"Output directory is {str(output_directory)}")

    _validate_typeshed(typeshed_path)
    _validate_version(version)
    _ensure_usable_binary_exists(pyre_directory)

    dependencies = [
        line.strip()
        for line in (pyre_directory / "requirements.txt").read_text().split("\n")
        if len(line) > 0
    ]

    with tempfile.TemporaryDirectory(prefix="pyre_package_build_") as build_path_str:
        build_path = Path(build_path_str)
        _add_init_files(build_path, version)
        _create_setup_py(pyre_directory, version, build_path, dependencies, nightly)

        _sync_python_files(pyre_directory, build_path)
        _sync_pysa_stubs(pyre_directory, build_path)
        _sync_stubs(pyre_directory, build_path)
        _sync_typeshed(build_path, typeshed_path)
        _sync_sapp_filters(pyre_directory, build_path)
        _sync_binary(pyre_directory, build_path)
        _strip_binary(build_path)
        _sync_documentation_files(pyre_directory, build_path)

        _patch_version(version, build_path)

        _run_setup_command(
            pyre_directory,
            build_path,
            dependencies,
            version,
            "sdist",
            nightly,
        )
        _create_setup_configuration(build_path)
        twine_check([path.as_posix() for path in (build_path / "dist").iterdir()])

        _run_setup_command(
            pyre_directory,
            build_path,
            dependencies,
            version,
            "bdist_wheel",
            nightly,
        )
        source_distribution_destination, wheel_destination = _rename_and_move_artifacts(
            build_path, output_directory
        )
        LOG.info("All done.")
        LOG.info(
            "\n Build artifact available at:\n {}\n".format(
                str(source_distribution_destination)
            )
        )
        LOG.info(
            "\n Source distribution available at:\n {}\n".format(str(wheel_destination))
        )
