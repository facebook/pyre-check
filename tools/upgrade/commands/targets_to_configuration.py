# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
TODO(T132414938) Add a module-level docstring
"""


import argparse
import logging
import subprocess
from pathlib import Path
from typing import Dict, List, Optional

import libcst
from pyre_extensions import override
from typing_extensions import Final

from ..configuration import Configuration
from ..errors import Errors
from ..filesystem import (
    add_local_mode,
    find_directories,
    find_files,
    find_targets,
    get_filesystem,
    LocalMode,
    remove_non_pyre_ignores,
    Target,
)
from ..repository import Repository
from .command import CommandArguments, ErrorSuppressingCommand
from .strict_default import StrictDefault

LOG: logging.Logger = logging.getLogger(__name__)


class TargetPyreRemover(libcst.CSTTransformer):
    @override
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
        pyre_only: bool,
        strict: bool,
        only_clean_targets: bool,
    ) -> None:
        super().__init__(command_arguments, repository)
        self._subdirectory: Final[Optional[str]] = subdirectory
        self._glob_threshold: Optional[int] = glob
        self._fixme_threshold: int = fixme_threshold
        self._pyre_only: bool = pyre_only
        self._strict: bool = strict
        self._only_clean_targets: bool = only_clean_targets

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
            pyre_only=arguments.pyre_only,
            strict=arguments.strict,
            only_clean_targets=arguments.only_clean_targets,
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
            "--only-clean-targets",
            action="store_true",
            help="Only perform target cleanup without affecting pyre configurations.",
        )

    def remove_target_typing_fields(self, files: List[Path]) -> None:
        LOG.info("Removing typing options from %s targets files", len(files))
        if self._pyre_only and not self._glob_threshold:
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
                r"type_checker \?=.*",
            ]
            remove_typing_fields_command = [
                "sed",
                "-i",
                "/" + r"\|".join(typing_options_regex) + "/d",
            ] + [str(file) for file in files]
            subprocess.run(remove_typing_fields_command)

    def find_or_create_configuration(
        self, directory: Path, new_targets: List[str]
    ) -> Configuration:
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
        return configuration

    def collect_full_targets(self, targets: Dict[str, List[Target]]) -> List[str]:
        new_targets = []
        for path, targets in targets.items():
            new_targets += [
                "//" + path.replace("/TARGETS", "") + ":" + target.name
                for target in targets
                if target.check_types
            ]
        return new_targets

    def convert_directory(self, directory: Path) -> None:
        all_targets = find_targets(directory, pyre_only=self._pyre_only)
        if not all_targets:
            LOG.warning("No configuration created because no targets found.")
            return

        # Set strict default to true if any binary or unittest targets are strict.
        apply_strict = self._strict and any(
            target.strict
            for target in [
                target for target_list in all_targets.values() for target in target_list
            ]
        )

        # Collect targets.
        new_targets = self.collect_full_targets(all_targets)
        targets_files = [Path(path) for path in all_targets.keys()]
        configuration = self.find_or_create_configuration(directory, new_targets)

        # Try setting a glob target.
        glob_threshold = self._glob_threshold
        all_errors = None
        if glob_threshold is not None:
            original_targets = configuration.targets
            configuration.targets = ["//" + str(directory) + "/..."]
            configuration.write()

            all_errors = configuration.get_errors()
            if any(
                len(errors) > glob_threshold
                for errors in all_errors.paths_to_errors.values()
            ):
                # Fall back to non-glob codemod.
                LOG.info(
                    "Exceeding error threshold of %d; falling back to listing "
                    "individual targets.",
                    glob_threshold,
                )
                configuration.targets = original_targets
                configuration.write()
                all_errors = configuration.get_errors()
            else:
                targets_files = [
                    directory / path
                    for path in get_filesystem().list(
                        str(directory), patterns=[r"**/TARGETS"]
                    )
                ]
        if not all_errors:
            all_errors = configuration.get_errors()

        # Remove all type-related target settings.
        self.remove_target_typing_fields(targets_files)
        if not self._pyre_only:
            remove_non_pyre_ignores(directory)

        # Suppress errors in individual files where fixme threshold is not exceeded.
        error_threshold = self._fixme_threshold
        for path, errors in all_errors.paths_to_errors.items():
            errors = list(errors)
            error_count = len(errors)
            if error_threshold and error_count > error_threshold:
                LOG.info(
                    "%d errors found in `%s`. Adding file-level ignore.",
                    error_count,
                    path,
                )
                add_local_mode(path, LocalMode.IGNORE)
            else:
                self._apply_suppressions(Errors(errors))

        # Spin up strict codemod if applicable, otherwise skip to final clean and lint.
        if apply_strict:
            LOG.info(
                "Some targets were running strict type checking. "
                "Adding strict setting to configuration."
            )
            strict_codemod = StrictDefault(
                command_arguments=CommandArguments(
                    comment=self._comment,
                    max_line_length=self._max_line_length,
                    truncate=self._truncate,
                    unsafe=self._unsafe,
                    force_format_unsuppressed=self._force_format_unsuppressed,
                    lint=self._lint,
                    no_commit=True,
                    should_clean=True,
                ),
                repository=self._repository,
                local_configuration=directory,
                remove_strict_headers=True,
                fixme_threshold=0,
                remove_unsafe_headers=False,
                remove_strict_flag=False,
                paths=None,
            )
            strict_codemod.run()
        else:
            self._get_and_suppress_errors(configuration)

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

    @override
    def run(self) -> None:
        # TODO(T62926437): Basic integration testing.
        subdirectory = self._subdirectory
        subdirectory = Path(subdirectory) if subdirectory else Path.cwd()

        if self._only_clean_targets:
            LOG.info(
                "Cleaning typecheck targets from `%s`.",
                subdirectory,
            )
            LOG.info("No pyre configurations will be affected.")
            all_targets = find_targets(subdirectory, pyre_only=self._pyre_only)
            if not all_targets:
                LOG.warning("No targets found.")
                return
            targets_files = [Path(path) for path in all_targets.keys()]
            self.remove_target_typing_fields(targets_files)
            return

        LOG.info(
            "Converting typecheck targets to pyre configurations in `%s`.",
            subdirectory,
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
        glob = self._glob_threshold
        if glob:
            summary += (
                f"\n\nConfiguration target automatically expanded to include "
                f"all subtargets, expanding type coverage while introducing "
                f"no more than {glob} fixmes per file."
            )
        title = f"Convert type check targets in {subdirectory} to use configuration"
        self._repository.commit_changes(
            commit=(not self._no_commit),
            title=title,
            summary=summary,
            set_dependencies=False,
        )
