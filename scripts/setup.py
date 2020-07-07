#!/usr/bin/env python3

# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import argparse
import logging
import multiprocessing
import os
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from tempfile import mkdtemp
from typing import Dict, List, Mapping, Optional, Type


COMPILER_VERSION = "4.08.0"
DEVELOPMENT_COMPILER: str = COMPILER_VERSION
RELEASE_COMPILER = f"{COMPILER_VERSION}+flambda"
DEPENDENCIES = [
    "base64.3.4.0",
    "conf-sqlite3",
    "core.v0.14.0",
    "dune.2.6.0",
    "yojson.1.7.0",
    "ppx_deriving_yojson.3.5.2",
    "ounit.2.2.2",
    "sedlex.2.1",
    "menhir.20200612",
    "lwt.5.3.0",
]


class OCamlbuildAlreadyInstalled(Exception):
    pass


class OldOpam(Exception):
    pass


@dataclass(frozen=True)
class Runner:
    logger: logging.Logger
    pyre_directory: Path

    local: bool = False
    temporary_root: bool = False
    opam_root: Optional[Path] = None
    opam_repository_override: Optional[str] = None
    configure_only: bool = False
    environment_only: bool = False
    development: bool = False
    release: bool = False
    build_type: Optional[str] = None

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
    def build_type_override(self) -> Optional[str]:
        if self.build_type == "facebook":
            return "facebook"
        if self.build_type == "external":
            return "external"
        return None

    @property
    def make_arguments(self) -> str:
        if self.release:
            return "release"
        else:
            return "dev"

    @property
    def opam_repository(self) -> str:
        return self.opam_repository_override or "https://opam.ocaml.org"

    @property
    def environment_variables(self) -> Mapping[str, str]:
        return os.environ

    def make_opam_root(self) -> Path:
        home = Path.home()
        home_opam = home / ".opam"
        if self.local:
            if not home_opam.is_dir():
                local_opam = home / "local" / "opam"
                local_opam.mkdir(parents=True)
                local_opam.symlink_to(home_opam, target_is_directory=True)
            return home_opam
        if self.temporary_root:
            return Path(mkdtemp())
        return self.opam_root or home_opam

    def produce_dune_file(self) -> None:
        build_type = self.build_type_override
        if not build_type:
            if (self.pyre_directory / "facebook").is_dir:
                build_type = "facebook"
            else:
                build_type = "external"
        with open(self.pyre_directory / "dune.in") as dune_in:
            with open(self.pyre_directory / "dune", "w") as dune:
                dune_data = dune_in.read()
                dune.write(dune_data.replace("%VERSION%", build_type))

    def check_if_preinstalled(self) -> None:
        if self.environment_variables.get(
            "CHECK_IF_PREINSTALLED"
        ) != "false" and shutil.which("ocamlc"):
            ocamlc_location = self.run(["ocamlc", "-where"])
            test_ocamlbuild_location = Path(ocamlc_location) / "ocamlbuild"
            if test_ocamlbuild_location.is_dir():
                self.logger.error(
                    "OCamlbuild will refuse to install since it is already "
                    + f"present at {test_ocamlbuild_location}."
                )
                self.logger.error("If you want to bypass this safety check, run:")
                self.logger.error("CHECK_IF_PREINSTALLED=false ./scripts/setup.sh")
                raise OCamlbuildAlreadyInstalled

    def extract_opam_repository(self) -> str:
        if self.opam_repository.endswith("tar.gz"):
            temporary_repository = mkdtemp()
            self.run(
                [
                    "tar",
                    "xf",
                    self.opam_repository,
                    "-C",
                    temporary_repository,
                    "--strip-components=1",
                ]
            )
            return temporary_repository
        else:
            return self.opam_repository

    def validate_opam_version(self) -> None:
        version = self.run(["opam", "--version"])
        if version[:1] != "2":
            self.logger.error(
                "Pyre only supports opam 2.0.0 and above, please update your "
                + "opam version."
            )
            raise OldOpam

    def opam_environment_variables(self, opam_root: str) -> Dict[str, str]:
        self.logger.info("Activating opam")
        opam_env_result = self.run(
            [
                "opam",
                "env",
                "--yes",
                "--switch",
                self.compiler,
                "--root",
                opam_root,
                "--set-root",
                "--set-switch",
            ]
        )
        opam_environment_variables: Dict[str, str] = {}
        for line in opam_env_result.split("\n"):
            environment_variable, quoted_value = line.split(";")[0].split("=")
            value = quoted_value[1:-1]
            self.logger.info(f'{environment_variable}="{value}"')
            opam_environment_variables[environment_variable] = value
        return opam_environment_variables

    def __call__(self) -> None:
        opam_root = self.make_opam_root().as_posix()
        self.produce_dune_file()

        if self.configure_only:
            compiler_override = self.compiler_override
            if compiler_override:
                self.run(
                    ["opam", "switch", "set", compiler_override, "--root", opam_root]
                )
            return

        self.check_if_preinstalled()

        opam_repository = self.extract_opam_repository()
        self.validate_opam_version()
        self.run(
            [
                "opam",
                "init",
                "--yes",
                "--reinit",
                "--disable-sandboxing",
                "--compiler",
                self.compiler,
                "--root",
                opam_root,
                "default",
                opam_repository,
            ]
        )
        opam_environment_variables = self.opam_environment_variables(opam_root)

        self.run(
            ["opam", "install", "--yes"] + DEPENDENCIES,
            add_environment_variables=opam_environment_variables,
        )

        if self.environment_only:
            self.logger.info(
                "Environment built successfully, stopping here as requested."
            )
            return

        jobs = str(multiprocessing.cpu_count())
        self.run(
            ["make", self.make_arguments, "--jobs", jobs],
            self.pyre_directory,
            add_environment_variables=opam_environment_variables,
        )
        self.run(
            ["make", "--jobs", jobs, "test"],
            self.pyre_directory,
            add_environment_variables=opam_environment_variables,
        )

    def run(
        self,
        command: List[str],
        current_working_directory: Optional[Path] = None,
        add_environment_variables: Optional[Dict[str, str]] = None,
    ) -> str:
        if add_environment_variables:
            environment_variables = {
                **self.environment_variables,
                **add_environment_variables,
            }
        else:
            environment_variables = self.environment_variables
        self.logger.info(command)
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


def main(runner_type: Type[Runner]) -> None:
    logging.basicConfig(
        level=logging.INFO, format="[%(asctime)s] [%(levelname)s] %(message)s"
    )

    parser = argparse.ArgumentParser(description="Set up Pyre.")

    parser.add_argument("--pyre-directory", type=Path)

    parser.add_argument("--local", action="store_true")
    parser.add_argument("--temporary_root", action="store_true")
    parser.add_argument("--opam-root", type=Path)
    parser.add_argument("--repository", type=str)
    parser.add_argument("--configure", action="store_true")
    parser.add_argument("--environment-only", action="store_true")
    parser.add_argument("--development", action="store_true")
    parser.add_argument("--release", action="store_true")
    parser.add_argument("--build-type", type=str)

    parsed = parser.parse_args()

    pyre_directory = parsed.pyre_directory
    if not pyre_directory:
        pyre_directory = Path(__file__).parent.parent.absolute()

    runner_type(
        logger=logging.getLogger(__name__),
        pyre_directory=pyre_directory,
        local=parsed.local,
        temporary_root=parsed.temporary_root,
        opam_root=parsed.opam_root,
        opam_repository_override=parsed.repository,
        configure_only=parsed.configure,
        environment_only=parsed.environment_only,
        development=parsed.development,
        release=parsed.release,
        build_type=parsed.build_type,
    )()


if __name__ == "__main__":
    main(Runner)
