# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
import subprocess
from pathlib import Path
from typing import List, Optional

import libcst
from typing_extensions import Final

from ..configuration import Configuration
from ..errors import Errors, PartialErrorSuppression
from ..filesystem import (
    LocalMode,
    add_local_mode,
    find_directories,
    find_files,
    find_targets,
    get_filesystem,
    remove_non_pyre_ignores,
)
from ..repository import Repository
from .command import CommandArguments, ErrorSuppressingCommand
from .strict_default import StrictDefault


LOG: logging.Logger = logging.getLogger(__name__)


class TargetPyreRemover(libcst.CSTTransformer):
    def leave_Call(
        self, original_node: libcst.Call, updated_node: libcst.Call
    ) -> libcst.Call:
        check_types = False
        uses_pyre = True
        updated_fields = []
        for field in original_node.args:
            name = field.keyword
            value = field.value
            if not name:
                continue
            name = name.value
            if name == "check_types":
                if isinstance(value, libcst.Name):
                    check_types = check_types or value.value.lower() == "true"
            elif name == "check_types_options":
                if isinstance(value, libcst.SimpleString):
                    uses_pyre = uses_pyre and "mypy" not in value.value.lower()
            elif name not in ["typing", "typing_options"]:
                updated_fields.append(field)
        if check_types and uses_pyre:
            return updated_node.with_changes(args=updated_fields)
        return updated_node


class TargetsToConfiguration(ErrorSuppressingCommand):
    def __init__(
        self,
        command_arguments: CommandArguments,
        *,
        repository: Repository,
        subdirectory: Optional[str],
        glob: int,
        fixme_threshold: int,
        no_commit: bool,
        submit: bool,
        pyre_only: bool,
        strict: bool,
    ) -> None:
        super().__init__(command_arguments, repository)
        self._subdirectory: Final[Optional[str]] = subdirectory
        self._glob: int = glob
        self._fixme_threshold: int = fixme_threshold
        self._no_commit: bool = no_commit
        self._submit: bool = submit
        self._pyre_only: bool = pyre_only
        self._strict: bool = strict

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "TargetsToConfiguration":
        command_arguments = CommandArguments.from_arguments(arguments)
        return TargetsToConfiguration(
            command_arguments,
            repository=repository,
            subdirectory=arguments.subdirectory,
            glob=arguments.glob,
            fixme_threshold=arguments.fixme_threshold,
            no_commit=arguments.no_commit,
            submit=arguments.submit,
            pyre_only=arguments.pyre_only,
            strict=arguments.strict,
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(TargetsToConfiguration, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "--subdirectory", help="Only upgrade TARGETS files within this directory."
        )
        parser.add_argument(
            "--glob",
            type=int,
            help="Use a toplevel glob target instead of listing individual targets. \
            Fall back to individual targets if errors per file ever hits given \
            threshold.",
        )
        parser.add_argument(
            "--fixme-threshold",
            type=int,
            help="Ignore all errors in a file if fixme count exceeds threshold.",
        )
        parser.add_argument(
            "--strict",
            action="store_true",
            help="Turn on default strict mode if any targets were strict.",
        )
        parser.add_argument(
            "--pyre-only",
            action="store_true",
            help="Only convert pyre targets to configuration.",
        )
        parser.add_argument(
            "--no-commit", action="store_true", help="Keep changes in working state."
        )
        parser.add_argument("--submit", action="store_true", help=argparse.SUPPRESS)

    def remove_target_typing_fields(self, files: List[Path]) -> None:
        LOG.info("Removing typing options from %s targets files", len(files))
        if self._pyre_only and not self._glob:
            for path in files:
                targets_file = Path(path)
                source = targets_file.read_text()
                output = libcst.parse_module(source).visit(TargetPyreRemover()).code
                targets_file.write_text(output)
        else:
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
            ] + [str(file) for file in files]
            subprocess.run(remove_typing_fields_command)

    def convert_directory(self, directory: Path) -> None:
        all_targets = find_targets(directory, pyre_only=self._pyre_only)
        if not all_targets:
            LOG.warning("No configuration created because no targets found.")
            return
        if self._glob:
            new_targets = ["//" + str(directory) + "/..."]
            targets_files = [
                directory / path
                for path in get_filesystem().list(
                    str(directory), patterns=[r"**/TARGETS"]
                )
            ]
        else:
            new_targets = []
            targets_files = []
            for path, targets in all_targets.items():
                targets_files.append(Path(path))
                new_targets += [
                    "//" + path.replace("/TARGETS", "") + ":" + target.name
                    for target in targets
                ]

        apply_strict = self._strict and any(
            target.strict
            for target in [
                target for target_list in all_targets.values() for target in target_list
            ]
        )
        configuration_path = directory / ".pyre_configuration.local"
        if configuration_path.exists():
            LOG.warning(
                "Pyre project already exists at %s.\n\
                Amending targets to existing configuration.",
                configuration_path,
            )
            configuration = Configuration(configuration_path)
            configuration.add_targets(new_targets)
            configuration.deduplicate_targets()
            configuration.write()
        else:
            LOG.info("Creating local configuration at %s.", configuration_path)
            configuration_contents = {"targets": new_targets}
            configuration = Configuration(configuration_path, configuration_contents)
            configuration.write()

            # Add newly created configuration files to version control
            self._repository.add_paths([configuration_path])

        # Remove all type-related target settings
        self.remove_target_typing_fields(targets_files)
        if not self._pyre_only:
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
                try:
                    self._suppress_errors(Errors(errors))
                except PartialErrorSuppression:
                    LOG.warning(f"Could not suppress all errors in {path}")
                    LOG.info("Run with --unsafe to force suppression anyway.")
                    self._repository.revert_all(remove_untracked=True)

        if apply_strict:
            LOG.info(
                "Some targets were running strict type checking. "
                "Adding strict setting to configuration."
            )
            strict_codemod = StrictDefault(
                self._command_arguments,
                repository=self._repository,
                local_configuration=directory,
                remove_strict_headers=True,
                fixme_threshold=0,
            )
            strict_codemod.run()

        # Lint and re-run pyre once to resolve most formatting issues
        if self._lint:
            if self._repository.format():
                errors = configuration.get_errors(should_clean=False)
                try:
                    self._suppress_errors(errors)
                except PartialErrorSuppression:
                    LOG.warning(f"Could not suppress all errors in {path}")
                    LOG.info("Run with --unsafe to force suppression anyway.")
                    self._repository.revert_all(remove_untracked=True)

    def run(self) -> None:
        # TODO(T62926437): Basic integration testing.
        subdirectory = self._subdirectory
        subdirectory = Path(subdirectory) if subdirectory else Path.cwd()
        LOG.info(
            "Converting typecheck targets to pyre configurations in `%s`", subdirectory
        )
        configuration_directories = self._gather_directories(subdirectory)
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

    def _gather_directories(self, subdirectory: Path) -> List[Path]:
        configurations = find_files(subdirectory, ".pyre_configuration.local")
        configuration_directories = [
            configuration.replace("/.pyre_configuration.local", "")
            for configuration in configurations
        ]
        sorted_directories = sorted(
            (directory.split("/") for directory in configuration_directories),
            key=lambda directory: (len(directory), directory),
        )
        if len(configuration_directories) == 0:
            configuration_directories = [str(subdirectory)]
        else:
            # Fill in missing coverage
            missing_directories = []
            current_depth = len(str(subdirectory).split("/"))
            for directory in sorted_directories:
                if len(directory) <= current_depth:
                    continue
                all_subdirectories = find_directories(
                    Path("/".join(directory[0:current_depth]))
                )
                for subdirectory in all_subdirectories:
                    if all(
                        not configuration_directory.startswith(str(subdirectory))
                        for configuration_directory in configuration_directories
                    ):
                        missing_directories.append(subdirectory)
                current_depth += 1
            configuration_directories.extend(missing_directories)
        return [Path(directory) for directory in configuration_directories]
