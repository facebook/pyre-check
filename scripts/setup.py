#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import argparse
import logging
import multiprocessing
import os
import shutil
import subprocess
import sys
from enum import Enum
from pathlib import Path
from tempfile import mkdtemp
from typing import Dict, List, Mapping, NamedTuple, Optional, Type


LOG: logging.Logger = logging.getLogger(__name__)


COMPILER_VERSION = "4.10.2"
DEVELOPMENT_COMPILER: str = COMPILER_VERSION
RELEASE_COMPILER = f"{COMPILER_VERSION}+flambda"
DEPENDENCIES = [
    "base64.3.5.0",
    "core.v0.14.1",
    "re2.v0.14.0",
    "dune.2.9.1",
    "yojson.1.7.0",
    "ppx_deriving_yojson.3.6.1",
    "ounit.2.2.4",
    "menhir.20211230",
    "lwt.5.5.0",
    "ounit2-lwt.2.2.4",
    "pyre-ast.0.1.8",
    "mtime.1.3.0",
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

    development: bool = False
    release: bool = False

    @property
    def compiler_override(self) -> Optional[str]:
        if self.development:
            return DEVELOPMENT_COMPILER
        if self.release:
            return RELEASE_COMPILER
        return None

    @property
    def compiler(self) -> str:
        return self.compiler_override or DEVELOPMENT_COMPILER

    @property
    def make_arguments(self) -> str:
        if self.release:
            return "release"
        else:
            return "dev"

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
                self.compiler,
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
                "--root",
                self.opam_root.as_posix(),
                "default",
                "https://opam.ocaml.org",
            ]
        )
        self.run(
            [
                "opam",
                "switch",
                "create",
                self.compiler,
                "--packages=ocaml-option-flambda",
                "--yes",
                "--root",
                self.opam_root.as_posix(),
            ]
        )
        opam_environment_variables = self.opam_environment_variables()

        # This is required to work around a bug in pre-OCaml 4.12 that prevents
        # `re2` from installing correctly.
        # See https://github.com/janestreet/re2/issues/31
        ocamlc_location = self.run(
            ["ocamlc", "-where"], add_environment_variables=opam_environment_variables
        )
        self.run(["rm", "-f", f"{ocamlc_location}/version"])

        self.run(
            ["opam", "install", "--yes"] + DEPENDENCIES,
            add_environment_variables=opam_environment_variables,
        )

        return opam_environment_variables

    def set_opam_switch_and_install_dependencies(self) -> Mapping[str, str]:
        self.run(
            [
                "opam",
                "switch",
                "set",
                self.compiler,
                "--root",
                self.opam_root.as_posix(),
            ]
        )

        opam_environment_variables = self.opam_environment_variables()
        self.run(
            ["opam", "install", "--yes"] + DEPENDENCIES,
            add_environment_variables=opam_environment_variables,
        )
        return opam_environment_variables

    def full_setup(
        self,
        pyre_directory: Path,
        *,
        run_tests: bool = False,
        run_clean: bool = False,
        build_type_override: Optional[BuildType] = None,
    ) -> None:
        self.produce_dune_file(pyre_directory, build_type_override)

        opam_environment_variables = self.set_opam_switch_and_install_dependencies()

        if run_clean:
            self.run(
                ["dune", "clean"],
                pyre_directory / "source",
                add_environment_variables=opam_environment_variables,
            )

        jobs = str(multiprocessing.cpu_count())

        if run_tests:
            self.run(
                ["make", "--jobs", jobs, "test", "--directory", "source"],
                pyre_directory,
                add_environment_variables=opam_environment_variables,
            )

        self.run(
            ["make", self.make_arguments, "--jobs", jobs, "--directory", "source"],
            pyre_directory,
            add_environment_variables=opam_environment_variables,
        )

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
        output = subprocess.check_output(
            command,
            universal_newlines=True,
            cwd=current_working_directory,
            env=environment_variables,
        )
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
    parser.add_argument("--development", action="store_true")
    parser.add_argument("--release", action="store_true")
    parser.add_argument("--build-type", type=BuildType)
    parser.add_argument("--no-tests", action="store_true")

    parsed = parser.parse_args()

    pyre_directory = parsed.pyre_directory
    if not pyre_directory:
        pyre_directory = Path(__file__).parent.parent.absolute()

    opam_root = _make_opam_root(parsed.local, parsed.temporary_root, parsed.opam_root)

    runner = runner_type(
        opam_root=opam_root, development=parsed.development, release=parsed.release
    )
    if parsed.configure:
        runner.produce_dune_file(pyre_directory, parsed.build_type)
        compiler_override = runner.compiler_override
        if compiler_override:
            runner.run(
                [
                    "opam",
                    "switch",
                    "set",
                    compiler_override,
                    "--root",
                    runner.opam_root.as_posix(),
                ]
            )
    elif parsed.environment_only:
        runner.produce_dune_file(pyre_directory, parsed.build_type)
        runner.initialize_opam_switch()
        LOG.info("Environment built successfully, stopping here as requested.")
    else:
        runner.full_setup(
            pyre_directory,
            run_tests=not parsed.no_tests,
            build_type_override=parsed.build_type,
        )


if __name__ == "__main__":
    setup(Setup)
