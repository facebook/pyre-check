# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import json
import logging
import subprocess
from logging import Logger
from pathlib import Path
from typing import Any, Dict, Generator, List, Optional, Sequence

from . import UserError
from .errors import Errors
from .filesystem import get_filesystem


LOG: Logger = logging.getLogger(__name__)


class Configuration:
    def __init__(
        self, path: Path, json_contents: Optional[Dict[str, Any]] = None
    ) -> None:
        if json_contents is None:
            with open(path, "r") as configuration_file:
                json_contents = json.load(configuration_file)
        self._path: Path = path
        if path.name == ".pyre_configuration.local":
            self.is_local: bool = True
        else:
            self.is_local: bool = False
        self.root: str = str(path.parent)
        self.original_contents: Dict[str, Any] = json_contents

        # Configuration fields
        self.strict: Optional[bool] = json_contents.get("strict")
        self.targets: Optional[List[str]] = json_contents.get("targets")
        self.source_directories: Optional[List[str]] = json_contents.get(
            "source_directories"
        )
        self.version: Optional[str] = json_contents.get("version")
        self.pysa_version: Optional[str] = json_contents.get("pysa_version")
        self.use_buck_builder: Optional[bool] = json_contents.get("use_buck_builder")
        self.use_buck_source_database: Optional[bool] = json_contents.get(
            "use_buck_source_database"
        )
        self.ignore_all_errors: Optional[List[str]] = json_contents.get(
            "ignore_all_errors"
        )
        self.use_buck2: Optional[bool] = json_contents.get("use_buck2")

    def get_contents(self) -> Dict[str, Any]:
        """Assumption: The field names in this class match the key names in
        the configuration."""
        contents: Dict[str, Any] = self.original_contents

        def update_contents(key: str) -> None:
            attribute = getattr(self, key)
            if attribute is not None:
                contents[key] = attribute
            elif key in contents:
                del contents[key]

        update_contents("targets")
        update_contents("source_directories")
        update_contents("version")
        update_contents("pysa_version")
        update_contents("strict")
        update_contents("use_buck_builder")
        update_contents("use_buck_source_database")
        update_contents("use_buck2")
        return contents

    @staticmethod
    def find_parent_file(
        filename: str, directory: Optional[Path] = None
    ) -> Optional[Path]:
        directory = directory or Path.cwd()
        root = directory.root
        while directory != root:
            configuration_path = directory / filename
            if configuration_path.is_file():
                return configuration_path
            parent = directory.parent
            if directory == parent:
                return None
            directory = parent
        return None

    @staticmethod
    def find_project_configuration(directory: Optional[Path] = None) -> Path:
        path = Configuration.find_parent_file(".pyre_configuration", directory)
        if path is None:
            raise UserError("No root with a `.pyre_configuration` found.")
        return path

    @staticmethod
    def find_local_configuration(directory: Optional[Path] = None) -> Optional[Path]:
        return Configuration.find_parent_file(".pyre_configuration.local", directory)

    @staticmethod
    def gather_local_configuration_paths(directory: str) -> Sequence[Path]:
        return [
            Path(path)
            for path in get_filesystem().list(
                directory, patterns=[r"**\.pyre_configuration.local"]
            )
        ]

    @staticmethod
    def gather_local_configurations() -> List["Configuration"]:
        LOG.info("Finding configurations...")
        configuration_paths = Configuration.gather_local_configuration_paths(".")
        if not configuration_paths:
            LOG.info("No projects with local configurations found.")
            return []
        configurations = []
        for configuration_path in configuration_paths:
            with open(configuration_path) as configuration_file:
                try:
                    configuration = Configuration(
                        configuration_path, json.load(configuration_file)
                    )
                    configurations.append(configuration)
                except json.decoder.JSONDecodeError:
                    LOG.error(
                        "Configuration at `%s` is invalid, skipping.",
                        configuration_path,
                    )
        LOG.info(
            "Found %d local configuration%s.",
            len(configurations),
            "s" if len(configurations) != 1 else "",
        )
        return configurations

    def get_path(self) -> Path:
        return self._path

    def get_source_paths(self) -> Generator[Path, None, None]:
        # This is an approximation
        return Path(self.root).glob("**/*.py")

    def get_directory(self) -> Path:
        return self._path.parent

    def write(self) -> None:
        with open(self._path, "w") as configuration_file:
            json.dump(self.get_contents(), configuration_file, sort_keys=True, indent=2)
            configuration_file.write("\n")

    def remove_version(self) -> None:
        if not self.version:
            LOG.info("Version not found in configuration.")
            return
        self.version = None

    def set_version(self, version: str) -> None:
        self.version = version

    def set_pysa_version(self, pysa_version: str) -> None:
        self.pysa_version = pysa_version

    def enable_source_database_buck_builder(self) -> None:
        self.use_buck_builder = True
        self.use_buck_source_database = True

    def set_use_buck1_if_possible(self) -> None:
        if self.use_buck2 is None:
            self.use_buck2 = False

    def add_strict(self) -> None:
        if self.strict:
            LOG.info("Configuration is already strict.")
            return
        self.strict = True

    # Pyre is now strict by default, so in most cases we want to
    # remove the strict flag from the config file.
    def use_strict_default(self) -> None:
        self.strict = None

    def add_targets(self, targets: List[str]) -> None:
        existing_targets = self.targets
        if existing_targets:
            existing_targets.extend(targets)
        else:
            self.targets = targets

    def deduplicate_targets(self) -> None:
        targets = self.targets
        if not targets:
            return
        glob_targets = [target for target in targets if target.endswith("/...")]
        non_glob_targets = [target for target in targets if not target.endswith("/...")]
        all_targets = sorted(set(glob_targets)) + sorted(set(non_glob_targets))
        deduplicated_targets = []
        expanded_targets = set()
        for target in all_targets:
            if target.endswith("/...") or target.endswith(":"):
                try:
                    expanded = (
                        subprocess.check_output(["buck2", "query", target])
                        .decode()
                        .strip()
                        .split("\n")
                    )
                    if not all(target in expanded_targets for target in expanded):
                        expanded_targets.update(expanded)
                        deduplicated_targets.append(target)
                except subprocess.CalledProcessError as error:
                    LOG.warning("Failed to query target: %s\n%s", target, str(error))
                    deduplicated_targets.append(target)
            elif target not in expanded_targets:
                expanded_targets.add(target)
                deduplicated_targets.append(target)
        deduplicated_targets.sort(key=lambda target: targets.index(target))
        self.targets = deduplicated_targets

    def run_pyre(
        self,
        arguments: List[str],
        description: str,
        should_clean: bool,
        command_input: Optional[str],
        stderr_flag: "subprocess._FILE" = subprocess.PIPE,
    ) -> Optional["subprocess.CompletedProcess[str]"]:
        if should_clean:
            try:
                # If building targets, run clean or space may run out on device!
                LOG.info("Running `buck clean`...")
                subprocess.call(["buck", "clean"], timeout=200)
            except subprocess.TimeoutExpired:
                LOG.warning("Buck timed out. Try running `buck kill` before retrying.")
                return None
            except subprocess.CalledProcessError as error:
                LOG.warning("Error calling `buck clean`: %s", str(error))
                return None
        try:
            LOG.info("%s", description)
            return subprocess.run(
                ["pyre", *arguments],
                stdout=subprocess.PIPE,
                stderr=stderr_flag,
                text=True,
                input=command_input,
            )
        except subprocess.CalledProcessError as error:
            LOG.warning("Error calling pyre: %s", str(error))
            return None

    def get_errors(
        self,
        only_fix_error_code: Optional[int] = None,
        should_clean: bool = True,
        command_input: Optional[str] = None,
        strict: bool = False,
    ) -> Errors:
        local_root_arguments = (
            ["--local-configuration", self.root] if self.is_local else []
        )
        strict_arguments = ["--strict"] if strict else []
        arguments = [*strict_arguments, *local_root_arguments, "--output=json", "check"]
        pyre_output = self.run_pyre(
            arguments=arguments,
            description=f"Checking `{self.root}`...",
            should_clean=self.targets is not None and should_clean,
            command_input=command_input,
        )
        if not pyre_output:
            return Errors.empty()

        stdout = pyre_output.stdout
        if stdout is None:
            return Errors.empty()

        stdout = stdout.strip()

        try:
            errors = Errors.from_json(stdout, only_fix_error_code)
        except UserError as error:
            LOG.info("Error when parsing Pyre error output.")
            LOG.info(f"Pyre stdout: {stdout}\nPyre stderr: {pyre_output.stderr}")
            raise error

        LOG.info("Found %d error%s.", len(errors), "s" if len(errors) != 1 else "")
        return errors
