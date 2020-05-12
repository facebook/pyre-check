# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import json
import logging
import subprocess
from pathlib import Path
from typing import List

from ..configuration import Configuration
from ..errors import Errors
from ..filesystem import (
    LocalMode,
    add_local_mode,
    find_files,
    find_targets,
    get_filesystem,
    path_exists,
    remove_non_pyre_ignores,
)
from ..repository import Repository
from .command import ErrorSuppressingCommand


LOG: logging.Logger = logging.getLogger(__name__)


class TargetsToConfiguration(ErrorSuppressingCommand):
    def __init__(self, arguments: argparse.Namespace, repository: Repository) -> None:
        super().__init__(arguments, repository)
        self._subdirectory: str = arguments.subdirectory
        self._glob: int = arguments.glob
        self._lint: bool = arguments.lint
        self._fixme_threshold: int = arguments.fixme_threshold
        self._no_commit: bool = arguments.no_commit
        self._submit: bool = arguments.submit

    def remove_target_typing_fields(self, files: List[str]) -> None:
        LOG.info("Removing typing options from %s targets files", len(files))
        typing_options_regex = [
            r"typing \?=.*",
            r"check_types \?=.*",
            r"check_types_options \?=.*",
            r"typing_options \?=.*",
        ]
        remove_typing_fields_command = [
            "sed",
            "-i",
            "/" + r"\|".join(typing_options_regex) + "/d",
        ] + files
        subprocess.run(remove_typing_fields_command)

    def convert_directory(self, directory: Path) -> None:
        all_targets = find_targets(directory)
        if not all_targets:
            LOG.warning("No configuration created because no targets found.")
            return
        targets_files = [
            str(directory / path)
            for path in get_filesystem().list(str(directory), patterns=[r"**/TARGETS"])
        ]
        if self._glob:
            new_targets = ["//" + str(directory) + "/..."]
        else:
            new_targets = []
            for path, target_names in all_targets.items():
                new_targets += ["//" + path + ":" + name for name in target_names]

        configuration_path = directory / ".pyre_configuration.local"
        if path_exists(str(configuration_path)):
            LOG.warning(
                "Pyre project already exists at %s.\n\
                Amending targets to existing configuration.",
                configuration_path,
            )
            with open(configuration_path) as configuration_file:
                configuration = Configuration(
                    configuration_path, json.load(configuration_file)
                )
                configuration.add_targets(new_targets)
                configuration.deduplicate_targets()
                configuration.write()
        else:
            LOG.info("Creating local configuration at %s.", configuration_path)
            configuration_contents = {"targets": new_targets, "strict": True}
            # Heuristic: if all targets with type checked targets are setting
            # a target to be strictly checked, let's turn on default strict.
            for targets_file in targets_files:
                regex_patterns = [
                    r"check_types_options \?=.*strict.*",
                    r"typing_options \?=.*strict.*",
                ]
                result = subprocess.run(
                    ["grep", "-x", r"\|".join(regex_patterns), targets_file]
                )
                if result.returncode != 0:
                    configuration_contents["strict"] = False
                    break
            configuration = Configuration(configuration_path, configuration_contents)
            configuration.write()

            # Add newly created configuration files to version control
            self._repository.add_paths([configuration_path])

        # Remove all type-related target settings
        self.remove_target_typing_fields(targets_files)
        remove_non_pyre_ignores(directory)

        all_errors = configuration.get_errors()
        error_threshold = self._fixme_threshold
        glob_threshold = self._glob

        for path, errors in all_errors:
            errors = list(errors)
            error_count = len(errors)
            if glob_threshold and error_count > glob_threshold:
                # Fall back to non-glob codemod.
                LOG.info(
                    "Exceeding error threshold of %d; falling back to listing "
                    "individual targets.",
                    glob_threshold,
                )
                self._repository.revert_all(remove_untracked=True)
                self._glob = None
                return self.run()
            if error_threshold and error_count > error_threshold:
                LOG.info(
                    "%d errors found in `%s`. Adding file-level ignore.",
                    error_count,
                    path,
                )
                add_local_mode(path, LocalMode.IGNORE)
            else:
                self._suppress_errors(Errors(errors))

        # Lint and re-run pyre once to resolve most formatting issues
        if self._lint:
            if self._repository.format():
                errors = configuration.get_errors(should_clean=False)
                self._suppress_errors(errors)

    def run(self) -> None:
        # TODO(T62926437): Basic integration testing.
        subdirectory = self._subdirectory
        subdirectory = Path(subdirectory) if subdirectory else Path.cwd()
        LOG.info(
            "Converting typecheck targets to pyre configurations in `%s`", subdirectory
        )

        configurations = find_files(subdirectory, ".pyre_configuration.local")
        configuration_directories = sorted(
            Path(configuration.replace("/.pyre_configuration.local", ""))
            for configuration in configurations
        )
        if len(configuration_directories) == 0:
            configuration_directories = [subdirectory]
        converted = []
        for directory in configuration_directories:
            if all(
                str(directory).startswith(str(converted_directory)) is False
                for converted_directory in converted
            ):
                self.convert_directory(directory)
                converted.append(directory)

        summary = self._repository.MIGRATION_SUMMARY
        glob = self._glob
        if glob:
            summary += (
                f"\n\nConfiguration target automatically expanded to include "
                f"all subtargets, expanding type coverage while introducing "
                f"no more than {glob} fixmes per file."
            )
        title = f"Convert type check targets in {subdirectory} to use configuration"
        self._repository.submit_changes(
            commit=(not self._no_commit),
            submit=self._submit,
            title=title,
            summary=summary,
            set_dependencies=False,
        )
