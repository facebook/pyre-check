#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This script provides a the logic used to bootstrap a local opam
switch for building Pyre by collecting all dependencies, as well
as how to configure opam and then invoke dune for various flavors
of builds.
"""

import argparse
import logging
import os
import subprocess
import sys
import textwrap
from enum import Enum
from pathlib import Path
from subprocess import CalledProcessError
from typing import Dict, List, Mapping, Optional, Tuple

LOG: logging.Logger = logging.getLogger(__name__)


COMPILER_VERSION = "4.14.0"
DEPENDENCIES = [
    "base64.3.5.1",
    "cmdliner.1.1.1",
    "core.v0.16.2",
    "re2.v0.16.0",
    "dune.3.7.1",
    "yojson.2.0.2",
    "jsonm.1.0.2",
    "ppx_deriving_yojson.3.7.0",
    "ppx_yojson_conv.v0.16.0",
    "ounit2.2.2.7",
    "menhir.20220210",
    "lwt.5.6.1",
    "lwt_ppx.2.1.0",
    "ounit2-lwt.2.2.7",
    "pyre-ast.0.1.11",
    "mtime.1.4.0",
]


class OldOpam(Exception):
    pass


class OpamVersionParseError(Exception):
    pass


class BuildType(Enum):
    EXTERNAL = "external"
    FACEBOOK = "facebook"


def _custom_linker_option(pyre_directory: Path, build_type: BuildType) -> str:
    # HACK: This is a temporary workaround for inconsistent OS installations
    # in FB-internal CI. Can be removed once all fleets are upgraded.
    if build_type == BuildType.FACEBOOK and sys.platform == "linux":
        return (
            (pyre_directory / "facebook" / "scripts" / "custom_linker_options.txt")
            .read_text()
            .rstrip()
        )
    else:
        return ""


def detect_opam_version() -> Tuple[int, ...]:
    LOG.info(["opam", "--version"])
    version = subprocess.check_output(
        ["opam", "--version"], universal_newlines=True
    ).strip()

    try:
        version_semver = version.split("~")[0]
        version = tuple(map(int, version_semver.split(".")))
    except ValueError as error:
        message = f"Failed to parse output of `opam --version`: `{version}`"
        raise OpamVersionParseError(message) from error

    LOG.info(f"Found opam version {'.'.join(map(str, version))}")

    if version[0] != 2:
        LOG.error(
            "Pyre only supports opam 2.0.0 and above, please update your "
            + "opam version."
        )
        raise OldOpam

    return version


def _run_command(
    command: List[str],
    current_working_directory: Optional[Path] = None,
    add_environment_variables: Optional[Mapping[str, str]] = None,
) -> str:
    if add_environment_variables:
        environment_variables = {
            **os.environ,
            **add_environment_variables,
        }
    else:
        environment_variables = None
    LOG.info(command)
    try:
        output = subprocess.check_output(
            command,
            universal_newlines=True,
            cwd=current_working_directory,
            env=environment_variables,
        )
    except CalledProcessError as called_process_error:
        LOG.info(
            f"Command: {command} returned non zero exit code.\n"
            f"stdout: {called_process_error.stdout}\n"
            f"stderr: {called_process_error.stderr}"
        )
        raise called_process_error

    if output.endswith("\n"):
        return output[:-1]
    else:
        return output


def _switch_name() -> str:
    return f"pyre-{COMPILER_VERSION}"


def _compiler_specification() -> str:
    """
    Command-line argument to set the compiler version in `opam switch create ...`

    The format for how to specify this changed in 4.12.0, see
    https://discuss.ocaml.org/t/experimental-new-layout-for-the-ocaml-variants-packages-in-opam-repository/6779
    """
    return ",".join(
        [
            f"--packages=ocaml-variants.{COMPILER_VERSION}+options",
            "ocaml-option-flambda",
        ]
    )


def _opam_command(opam_version: Tuple[int, ...]) -> List[str]:
    command = ["opam"]

    # We need to explicitly set the opam cli version we are using,
    # otherwise it automatically uses `2.0` which means we can't use
    # some options from 2.1 such as `--assume-depexts`.
    if opam_version >= (2, 1):
        command.append("--cli=2.1")
    if opam_version >= (2, 2):
        command.append("--cli=2.2")
    return command


def produce_root_dune_file(pyre_directory: Path, build_type: BuildType) -> None:
    # lint-ignore: NoUnsafeFilesystemRule
    with open(pyre_directory / "source" / "dune.in") as dune_in:
        # lint-ignore: NoUnsafeFilesystemRule
        with open(pyre_directory / "source" / "dune", "w") as dune:
            dune_data = dune_in.read()
            dune.write(
                "; WARNING: This file is generated from dune.in when invoking 'make'.\n"
            )
            dune.write("; Please edit the original file.\n\n")
            dune.write(
                dune_data.replace("%VERSION%", build_type.value).replace(
                    "%CUSTOM_LINKER_OPTION%",
                    _custom_linker_option(pyre_directory, build_type),
                )
            )


def produce_taint_test_dune_file(pyre_directory: Path) -> None:
    directory = pyre_directory / "source/interprocedural_analyses/taint/test"
    # lint-ignore: NoUnsafeFilesystemRule
    with open(directory / "dune.in") as dune_in:
        # lint-ignore: NoUnsafeFilesystemRule
        with open(directory / "dune", "w") as dune:
            dune.write(
                "; WARNING: This file is generated from dune.in when invoking 'make'.\n"
            )
            dune.write("; Please edit the original file.\n\n")
            dune.write(dune_in.read())
            for test_file in (directory / "integration").glob("*.py"):
                dune.write("\n")
                dune.write("(rule\n")
                dune.write(" (alias runtest)\n")

                dependencies = [
                    f"integration/{test_file.name}",
                    f"integration/{test_file.name}.models",
                    f"integration/{test_file.name}.cg",
                    f"integration/{test_file.name}.hofcg",
                    f"integration/{test_file.name}.config",
                    f"integration/{test_file.name}.overrides",
                    f"integration/{test_file.name}.pysa",
                ]
                dependencies = [
                    path for path in dependencies if (directory / path).exists()
                ]
                dune.write(
                    f" (deps\n{textwrap.indent('\n'.join(dependencies), prefix='  ')})\n"
                )

                action = "(run ./integrationTest.exe)"
                action = f"(setenv\n PYSA_INTEGRATION_TEST\n {test_file.name}\n{textwrap.indent(action, prefix=' ')})"
                if test_file.name != "sanitize_tito_shaping.py":
                    # Enable invariant checking, except for the test above.
                    action = f"(setenv\n PYSA_CHECK_INVARIANTS\n 1\n{textwrap.indent(action, prefix=' ')})"
                dune.write(f" (action\n{textwrap.indent(action, prefix='  ')}))\n")


def _get_opam_environment_variables(
    opam_root: Path, opam_version: Tuple[int, ...]
) -> Dict[str, str]:
    LOG.info("Activating opam")
    opam_env_result = _run_command(
        _opam_command(opam_version)
        + [
            "env",
            "--yes",
            "--switch",
            _switch_name(),
            "--root",
            opam_root.as_posix(),
            "--set-root",
            "--set-switch",
            "--shell=bash",
        ]
    )
    opam_environment_variables: Dict[str, str] = {}
    # `opam env` produces lines of two forms:
    # - comments like ": this comment, starts with a colon;"
    # - lines defining and exporting env vars like "ENV_VAR=value; export ENV_VAR;"
    for line in opam_env_result.split("\n"):
        if not line.startswith(":"):
            environment_variable, quoted_value = line.split(";")[0].split("=")
            value = quoted_value[1:-1]
            LOG.info(f'{environment_variable}="{value}"')  # noqa: B907
            opam_environment_variables[environment_variable] = value
    return opam_environment_variables


def opam_update(
    opam_root: Path,
    opam_version: Tuple[int, ...],
    add_environment_variables: Optional[Mapping[str, str]] = None,
) -> None:
    _run_command(
        _opam_command(opam_version)
        + [
            "update",
            "--root",
            opam_root.as_posix(),
        ],
        add_environment_variables=add_environment_variables,
    )


def _initialize_opam_if_needed(
    opam_root: Path,
    opam_version: Tuple[int, ...],
    add_environment_variables: Optional[Mapping[str, str]] = None,
) -> None:
    # `opam init` is a noop if opam is already initialized, so it's safe to run
    # this unconditionally.
    _run_command(
        _opam_command(opam_version)
        + [
            "init",
            "--bare",
            "--yes",
            "--disable-sandboxing",
            "--root",
            opam_root.as_posix(),
            "default",
            "https://opam.ocaml.org",
        ],
        add_environment_variables=add_environment_variables,
    )


def _select_or_create_switch(
    opam_root: Path,
    opam_version: Tuple[int, ...],
    add_environment_variables: Optional[Mapping[str, str]] = None,
) -> None:
    switch_name = _switch_name()
    switch_root = opam_root / switch_name
    if switch_root.is_dir():
        _run_command(
            _opam_command(opam_version)
            + [
                "switch",
                "set",
                _switch_name(),
                "--root",
                opam_root.as_posix(),
            ]
        )
    else:
        _run_command(
            _opam_command(opam_version)
            + [
                "switch",
                "create",
                _switch_name(),
                _compiler_specification(),
                "--yes",
                "--root",
                opam_root.as_posix(),
            ],
            add_environment_variables=add_environment_variables,
        )


def _install_dependencies(
    opam_root: Path,
    opam_version: Tuple[int, ...],
    add_environment_variables: Optional[Mapping[str, str]] = None,
    rust_path: Optional[Path] = None,
) -> Mapping[str, str]:
    environment_variables = {
        **({} if add_environment_variables is None else add_environment_variables),
        **_get_opam_environment_variables(opam_root, opam_version),
    }
    if rust_path is not None:
        environment_variables["PATH"] = (
            str(rust_path) + ":" + environment_variables["PATH"]
        )

    opam_install_command = _opam_command(opam_version) + ["install", "--yes"]

    if sys.platform == "linux":
        # osx fails on sandcastle with exit status 2 (illegal argument) with this.
        # unable to repro locally on osx.
        opam_install_command.append("--assume-depexts")

    opam_install_command += DEPENDENCIES

    _run_command(opam_install_command, add_environment_variables=environment_variables)
    return environment_variables


def initialize_opam_switch(
    opam_root: Path,
    opam_version: Tuple[int, ...],
    release: bool = False,
    add_environment_variables: Optional[Mapping[str, str]] = None,
    rust_path: Optional[Path] = None,
) -> None:
    _initialize_opam_if_needed(opam_root, opam_version, add_environment_variables)
    opam_update(opam_root, opam_version, add_environment_variables)
    _select_or_create_switch(opam_root, opam_version, add_environment_variables)
    _install_dependencies(opam_root, opam_version, add_environment_variables, rust_path)


def full_setup(
    opam_root: Path,
    opam_version: Tuple[int, ...],
    pyre_directory: Path,
    *,
    release: bool = False,
    run_tests: bool = False,
    run_clean: bool = False,
    build_type: BuildType,
    add_environment_variables: Optional[Mapping[str, str]] = None,
    rust_path: Optional[Path] = None,
) -> None:
    opam_environment_variables: Mapping[str, str] = _install_dependencies(
        opam_root,
        opam_version,
        add_environment_variables=add_environment_variables,
        rust_path=rust_path,
    )

    def run_in_opam_environment(command: List[str]) -> None:
        _run_command(
            command,
            current_working_directory=pyre_directory / "source",
            add_environment_variables=opam_environment_variables,
        )

    produce_root_dune_file(pyre_directory, build_type)
    produce_taint_test_dune_file(pyre_directory)
    if run_clean:
        # Note: we do not run `make clean` because we want the result of the
        # explicit `produce_root_dune_file` to remain.
        # Dune 3.7 runs into `rmdir` failure when cleaning the `_build` directory
        # for some reason. Manually clean the dir to work around the issue.
        run_in_opam_environment(["rm", "-rf", "_build"])
    if release:
        LOG.info("Running a release build. This may take a while.")
        run_in_opam_environment(["make", "release"])
        if run_tests:
            run_in_opam_environment(["make", "release_test"])
    else:
        run_in_opam_environment(["make", "dev"])
        if run_tests:
            run_in_opam_environment(["make", "test"])


def _make_opam_root(local: bool) -> Path:
    home = Path.home()
    home_opam = home / ".opam"
    if local and not home_opam.is_dir():
        local_opam = home / "local" / "opam"
        local_opam.parent.mkdir(parents=True, exist_ok=True)
        local_opam.symlink_to(home_opam, target_is_directory=True)
    return home_opam


def _infer_build_type_from_filesystem(pyre_directory: Path) -> BuildType:
    if (pyre_directory / "facebook").is_dir():
        return BuildType.FACEBOOK
    else:
        return BuildType.EXTERNAL


def setup(
    add_environment_variables: Optional[Mapping[str, str]] = None,
) -> None:
    # lint-ignore: NoCustomLogRule
    logging.basicConfig(
        level=logging.INFO, format="[%(asctime)s] [%(levelname)s] %(message)s"
    )

    parser = argparse.ArgumentParser(description="Set up Pyre.")

    parser.add_argument("--pyre-directory", type=Path)

    parser.add_argument("--local", action="store_true")
    parser.add_argument("--configure", action="store_true")
    parser.add_argument("--release", action="store_true")
    parser.add_argument("--build-type", type=BuildType)
    parser.add_argument("--no-tests", action="store_true")
    parser.add_argument("--rust-path", type=Path)

    parsed = parser.parse_args()

    pyre_directory = parsed.pyre_directory
    if not pyre_directory:
        pyre_directory = Path(__file__).parent.parent.absolute()

    opam_root = _make_opam_root(parsed.local)
    build_type = parsed.build_type or _infer_build_type_from_filesystem(pyre_directory)
    opam_version = detect_opam_version()
    release = parsed.release

    if parsed.configure:
        produce_root_dune_file(pyre_directory, build_type)
        produce_taint_test_dune_file(pyre_directory)
    else:
        initialize_opam_switch(
            opam_root,
            opam_version,
            release,
            add_environment_variables,
            parsed.rust_path,
        )
        full_setup(
            opam_root,
            opam_version,
            pyre_directory,
            release=release,
            run_tests=not parsed.no_tests,
            build_type=build_type,
            add_environment_variables=add_environment_variables,
            rust_path=parsed.rust_path,
        )


if __name__ == "__main__":
    setup(add_environment_variables=None)
