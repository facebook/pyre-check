#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This script provides a the logic used to bootstrap a local opam
switch for building Pyre by collecting all dependencies, as well
as how to configure opam and then invoke dune for various flavors
of builds.
"""


import argparse
import logging
import os
import shutil
import subprocess
import sys
from enum import Enum
from pathlib import Path
from subprocess import CalledProcessError
from tempfile import mkdtemp
from typing import Dict, List, Mapping, NamedTuple, Optional, Type


LOG: logging.Logger = logging.getLogger(__name__)


COMPILER_VERSION = "4.14.0"
DEPENDENCIES = [
    "base64.3.5.1",
    "core.v0.15.1",
    "core_unix.v0.15.2",
    "re2.v0.15.0",
    "dune.3.7.1",
    "yojson.2.0.2",
    "ppx_deriving_yojson.3.7.0",
    "ppx_yojson_conv.v0.15.1",
    "ounit2.2.2.7",
    "menhir.20220210",
    "lwt.5.6.1",
    "lwt_ppx.2.1.0",
    "ounit2-lwt.2.2.7",
    "pyre-ast.0.1.8",
    "mtime.1.4.0",
    "errpy.0.0.7",
]


class OCamlbuildAlreadyInstalled(Exception):
    pass


class OldOpam(Exception):
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


class Setup(NamedTuple):
    opam_root: Path

    release: bool = False

    def switch_name(self) -> str:
        return f"{COMPILER_VERSION}+flambda" if self.release else COMPILER_VERSION

    def compiler_specification(self) -> str:
        """
        Command-line argument to set the compiler version in `opam switch create ...`

        The format for how to specify this changed in 4.12.0, see
        https://discuss.ocaml.org/t/experimental-new-layout-for-the-ocaml-variants-packages-in-opam-repository/6779
        """
        if not self.release:
            return COMPILER_VERSION
        else:
            return ",".join(
                [
                    f"--packages=ocaml-variants.{COMPILER_VERSION}+options",
                    "ocaml-options-only-flambda",
                ]
            )

    @property
    def environment_variables(self) -> Mapping[str, str]:
        return os.environ

    def produce_dune_file(
        self, pyre_directory: Path, build_type: Optional[BuildType] = None
    ) -> None:
        if not build_type:
            if (pyre_directory / "facebook").is_dir():
                build_type = BuildType.FACEBOOK
            else:
                build_type = BuildType.EXTERNAL
        with open(pyre_directory / "source" / "dune.in") as dune_in:
            with open(pyre_directory / "source" / "dune", "w") as dune:
                dune_data = dune_in.read()
                dune.write(
                    dune_data.replace("%VERSION%", build_type.value).replace(
                        "%CUSTOM_LINKER_OPTION%",
                        _custom_linker_option(pyre_directory, build_type),
                    )
                )

    def check_if_preinstalled(self) -> None:
        if self.environment_variables.get(
            "CHECK_IF_PREINSTALLED"
        ) != "false" and shutil.which("ocamlc"):
            ocamlc_location = self.run(["ocamlc", "-where"])
            test_ocamlbuild_location = Path(ocamlc_location) / "ocamlbuild"
            if test_ocamlbuild_location.is_dir():
                LOG.error(
                    "OCamlbuild will refuse to install since it is already "
                    + f"present at {test_ocamlbuild_location}."
                )
                LOG.error("If you want to bypass this safety check, run:")
                LOG.error("CHECK_IF_PREINSTALLED=false ./scripts/setup.sh")
                raise OCamlbuildAlreadyInstalled

    def already_initialized(self) -> bool:
        return Path(self.opam_root.as_posix()).is_dir()

    def validate_opam_version(self) -> None:
        version = self.run(["opam", "--version"])
        if version[:1] != "2":
            LOG.error(
                "Pyre only supports opam 2.0.0 and above, please update your "
                + "opam version."
            )
            raise OldOpam

    def opam_environment_variables(self) -> Dict[str, str]:
        LOG.info("Activating opam")
        opam_env_result = self.run(
            [
                "opam",
                "env",
                "--yes",
                "--switch",
                self.switch_name(),
                "--root",
                self.opam_root.as_posix(),
                "--set-root",
                "--set-switch",
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
                LOG.info(f'{environment_variable}="{value}"')
                opam_environment_variables[environment_variable] = value
        return opam_environment_variables

    def initialize_opam_switch(self) -> Mapping[str, str]:
        self.check_if_preinstalled()

        self.validate_opam_version()
        self.run(
            [
                "opam",
                "init",
                "--bare",
                "--yes",
                "--disable-sandboxing",
                "--root",
                self.opam_root.as_posix(),
                "default",
                "https://opam.ocaml.org",
            ]
        )
        self.run(["opam", "update", "--root", self.opam_root.as_posix()])
        self.run(
            [
                "opam",
                "switch",
                "create",
                self.switch_name(),
                self.compiler_specification(),
                "--yes",
                "--root",
                self.opam_root.as_posix(),
            ]
        )
        opam_environment_variables = self.opam_environment_variables()

        opam_install_command = ["opam", "install", "--yes"]

        if sys.platform == "linux":
            # setting `--assume-depexts` means that opam will not require a "system"
            # installed version of Rust (e.g. via `dnf`` or `yum`) but will instead
            # accept a version referenced on the system `$PATH`
            opam_install_command.append("--assume-depexts")

        self.run(
            opam_install_command + DEPENDENCIES,
            add_environment_variables=opam_environment_variables,
        )

        return opam_environment_variables

    def set_opam_switch_and_install_dependencies(self, rust_path: Optional[Path]) -> Mapping[str, str]:
        self.run(
            [
                "opam",
                "switch",
                "set",
                self.switch_name(),
                "--root",
                self.opam_root.as_posix(),
            ]
        )

        environment_variables = self.opam_environment_variables()
        if rust_path is not None:
            environment_variables["PATH"] = str(rust_path) + ":" + environment_variables["PATH"]

        opam_install_command = ["opam", "install", "--yes"]

        if sys.platform == "linux":
            # osx fails on sandcastle with exit status 2 (illegal argument) with this.
            # unable to repro locally on osx.
            opam_install_command.append("--assume-depexts")

        opam_install_command += DEPENDENCIES

        self.run(
            opam_install_command,
            add_environment_variables=environment_variables
        )
        return environment_variables

    def full_setup(
        self,
        pyre_directory: Path,
        *,
        run_tests: bool = False,
        run_clean: bool = False,
        build_type_override: Optional[BuildType] = None,
        rust_path: Optional[Path] = None
    ) -> None:
        opam_environment_variables: Mapping[
            str, str
        ] = self.set_opam_switch_and_install_dependencies(rust_path=rust_path)

        def run_in_opam_environment(command: List[str]) -> None:
            self.run(
                command,
                current_working_directory=pyre_directory / "source",
                add_environment_variables=opam_environment_variables,
            )

        self.produce_dune_file(pyre_directory, build_type_override)
        if run_clean:
            # Note: we do not run `make clean` because we want the result of the
            # explicit `produce_dune_file` to remain.
            # Dune 3.7 runs into `rmdir` failure when cleaning the `_build` directory
            # for some reason. Manually clean the dir to work around the issue.
            run_in_opam_environment(["rm", "-rf", "_build"])
        if self.release:
            LOG.info("Running a release build. This may take a while.")
            run_in_opam_environment(["make", "release"])
            if run_tests:
                run_in_opam_environment(["make", "release_test"])
        else:
            run_in_opam_environment(["make", "dev"])
            if run_tests:
                run_in_opam_environment(["make", "test"])

    def run(
        self,
        command: List[str],
        current_working_directory: Optional[Path] = None,
        add_environment_variables: Optional[Mapping[str, str]] = None,
    ) -> str:
        if add_environment_variables:
            environment_variables = {
                **self.environment_variables,
                **add_environment_variables,
            }
        else:
            environment_variables = self.environment_variables
        LOG.info(command)
        try:
            output = subprocess.check_output(
                command,
                universal_newlines=True,
                cwd=current_working_directory,
                env=environment_variables,
            )
        except CalledProcessError as called_process_error:
            LOG.info(f'Command: {command} returned non zero exit code.\n\
            stdout: {called_process_error.stdout}\n\
            stderr: {called_process_error.stderr}')
            raise called_process_error

        if output.endswith("\n"):
            return output[:-1]
        else:
            return output


def _make_opam_root(local: bool, temporary_root: bool, default: Optional[Path]) -> Path:
    home = Path.home()
    home_opam = home / ".opam"
    if local:
        if not home_opam.is_dir():
            local_opam = home / "local" / "opam"
            local_opam.mkdir(parents=True)
            local_opam.symlink_to(home_opam, target_is_directory=True)
        return home_opam
    if temporary_root:
        return Path(mkdtemp())
    return default or home_opam


def setup(runner_type: Type[Setup]) -> None:
    logging.basicConfig(
        level=logging.INFO, format="[%(asctime)s] [%(levelname)s] %(message)s"
    )

    parser = argparse.ArgumentParser(description="Set up Pyre.")

    parser.add_argument("--pyre-directory", type=Path)

    parser.add_argument("--local", action="store_true")
    parser.add_argument("--temporary_root", action="store_true")
    parser.add_argument("--opam-root", type=Path)
    parser.add_argument("--configure", action="store_true")
    parser.add_argument("--environment-only", action="store_true")
    parser.add_argument("--release", action="store_true")
    parser.add_argument("--build-type", type=BuildType)
    parser.add_argument("--no-tests", action="store_true")
    parser.add_argument("--rust-path", type=Path)

    parsed = parser.parse_args()

    pyre_directory = parsed.pyre_directory
    if not pyre_directory:
        pyre_directory = Path(__file__).parent.parent.absolute()

    opam_root = _make_opam_root(parsed.local, parsed.temporary_root, parsed.opam_root)

    runner = runner_type(opam_root=opam_root, release=parsed.release)
    if parsed.configure:
        runner.produce_dune_file(pyre_directory, parsed.build_type)
    elif parsed.environment_only:
        runner.produce_dune_file(pyre_directory, parsed.build_type)
        runner.initialize_opam_switch()
        LOG.info("Environment built successfully, stopping here as requested.")
    else:
        if not runner.already_initialized():
            runner.initialize_opam_switch()
        runner.full_setup(
            pyre_directory,
            run_tests=not parsed.no_tests,
            build_type_override=parsed.build_type,
            rust_path=parsed.rust_path
        )


if __name__ == "__main__":
    setup(Setup)
