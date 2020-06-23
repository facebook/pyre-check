# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import subprocess
from logging import Logger
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence

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
        self.strict: bool = bool(json_contents.get("strict"))
        self.targets: Optional[List[str]] = json_contents.get("targets")
        self.source_directories: Optional[List[str]] = json_contents.get(
            "source_directories"
        )
        self.version: Optional[str] = json_contents.get("version")
        self.differential: bool = json_contents.get("differential", False)

    def get_contents(self) -> Dict[str, Any]:
        contents: Dict[str, Any] = self.original_contents

        def update_contents(key: str) -> None:
            attribute = getattr(self, key)
            if attribute:
                contents[key] = attribute
            elif key in contents:
                del contents[key]

        update_contents("targets")
        update_contents("source_directories")
        update_contents("version")
        update_contents("strict")
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

    def add_strict(self) -> None:
        if self.strict:
            LOG.info("Configuration is already strict.")
            return
        self.strict = True

    def add_targets(self, targets: List[str]) -> None:
        existing_targets = self.targets
        if existing_targets:
            existing_targets.extend(targets)
        else:
            self.targets = targets

    def deduplicate_targets(self) -> None:
        all_targets = self.targets
        if all_targets:
            # pyre-fixme[6]: Expected `Iterable[Variable[_LT (bound to
            #  _SupportsLessThan)]]` for 1st param but got `Set[str]`.
            all_targets = sorted(set(all_targets))
            deduplicated_targets = []
            expanded_targets = set()
            for target in all_targets:
                if target.endswith("/...") or target.endswith(":"):
                    try:
                        expanded = (
                            subprocess.check_output(["buck", "query", target])
                            .decode()
                            .strip()
                            .split("\n")
                        )
                        if not all(target in expanded_targets for target in expanded):
                            expanded_targets.update(expanded)
                            deduplicated_targets.append(target)
                    except subprocess.CalledProcessError as error:
                        LOG.warning(
                            "Failed to query target: %s\n%s", target, str(error)
                        )
                        deduplicated_targets.append(target)
                elif target not in expanded_targets:
                    expanded_targets.add(target)
                    deduplicated_targets.append(target)
            self.targets = deduplicated_targets

    def get_errors(
        self, only_fix_error_code: Optional[int] = None, should_clean: bool = True
    ) -> Errors:
        if self.targets and should_clean:
            try:
                # If building targets, run clean or space may run out on device!
                LOG.info("Running `buck clean`...")
                subprocess.call(["buck", "clean"], timeout=200)
            except subprocess.TimeoutExpired:
                LOG.warning("Buck timed out. Try running `buck kill` before retrying.")
                return Errors.empty()
            except subprocess.CalledProcessError as error:
                LOG.warning("Error calling `buck clean`: %s", str(error))
                return Errors.empty()
        try:
            LOG.info("Checking `%s`...", self.root)
            if self.is_local:
                process = subprocess.run(
                    ["pyre", "-l", self.root, "--output=json", "check"],
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                )
            else:
                process = subprocess.run(
                    ["pyre", "--output=json", "check"],
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                )
            json_string = process.stdout.decode().strip()
            errors = Errors.from_json(json_string, only_fix_error_code)
            LOG.info("Found %d error%s.", len(errors), "s" if len(errors) != 1 else "")
            return errors
        except subprocess.CalledProcessError as error:
            LOG.warning("Error calling pyre: %s", str(error))
            return Errors.empty()
