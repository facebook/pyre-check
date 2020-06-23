# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import json
import logging
import sys
import traceback
from logging import Logger
from pathlib import Path
from typing import List, Optional

from typing_extensions import Final

from ...client.commands import ExitCode
from . import UserError
from .ast import UnstableAST
from .commands.codemods import (
    MissingGlobalAnnotations,
    MissingOverrideReturnAnnotations,
)
from .commands.command import (
    Command,
    CommandArguments,
    ErrorSuppressingCommand,
    ProjectErrorSuppressingCommand,
)
from .commands.consolidate_nested_configurations import ConsolidateNestedConfigurations
from .commands.expand_target_coverage import ExpandTargetCoverage
from .commands.fixme import Fixme
from .commands.strict_default import StrictDefault
from .commands.targets_to_configuration import TargetsToConfiguration
from .configuration import Configuration
from .errors import errors_from_targets
from .filesystem import Target, find_targets, path_exists
from .repository import Repository


LOG: Logger = logging.getLogger(__name__)


class GlobalVersionUpdate(Command):
    def __init__(
        self, *, repository: Repository, hash: str, paths: List[Path], submit: bool
    ) -> None:
        super().__init__(repository)
        self._hash: str = hash
        self._paths: List[Path] = paths
        self._submit: bool = submit

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "GlobalVersionUpdate":
        return GlobalVersionUpdate(
            repository=repository,
            hash=arguments.hash,
            paths=arguments.paths,
            submit=arguments.submit,
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(GlobalVersionUpdate, GlobalVersionUpdate).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument("hash", help="Hash of new Pyre version")
        parser.add_argument(
            "--paths",
            nargs="*",
            help="A list of paths to local Pyre projects.",
            default=[],
            type=path_exists,
        )
        parser.add_argument("--submit", action="store_true", help=argparse.SUPPRESS)

    def _set_local_overrides(
        self, configuration_paths: List[Path], old_version: str
    ) -> None:
        for configuration_path in configuration_paths:
            if "mock_repository" in str(configuration_path):
                # Skip local configurations we have for testing.
                continue
            local_configuration = Configuration(configuration_path)
            if local_configuration.version:
                LOG.info(
                    "Skipping %s as it already has a custom version field.",
                    configuration_path,
                )
                continue
            if local_configuration.differential:
                LOG.info(
                    "Skipping differential configuration at `%s`", configuration_path
                )
                continue
            local_configuration.set_version(old_version)
            local_configuration.write()

    def _suppress_global_errors(self, global_configuration: Configuration) -> None:
        if global_configuration.targets or global_configuration.source_directories:
            LOG.info("Suppressing errors after upgrading global version.")
            command_arguments = CommandArguments(
                comment=None,
                max_line_length=None,
                truncate=False,
                unsafe=False,
                force_format_unsuppressed=False,
                lint=True,
            )
            fixme_command = Fixme(
                command_arguments, repository=self._repository, error_source="generate"
            )
            fixme_command.run()

        self._repository.submit_changes(
            commit=True,
            submit=self._submit,
            title="Update pyre global configuration version",
            summary=f"Automatic upgrade to hash `{self._hash}`",
            ignore_failures=True,
        )

    def run(self) -> None:
        global_configuration = Configuration.find_project_configuration()

        # Update to new global version.
        configuration = Configuration(global_configuration)
        old_version = configuration.version
        if not old_version:
            LOG.error(
                "Global configuration at %s has no version field.", global_configuration
            )
            return
        configuration.set_version(self._hash)
        configuration.write()

        paths = self._paths
        configuration_paths = (
            [path / ".pyre_configuration.local" for path in paths]
            if paths
            else [
                configuration.get_path()
                for configuration in Configuration.gather_local_configurations()
            ]
        )

        self._set_local_overrides(configuration_paths, old_version)
        self._suppress_global_errors(configuration)


class FixmeSingle(ProjectErrorSuppressingCommand):
    def __init__(
        self,
        command_arguments: CommandArguments,
        *,
        repository: Repository,
        only_fix_error_code: int,
        upgrade_version: bool,
        error_source: str,
        no_commit: bool,
        submit: bool,
        path: Path,
    ) -> None:
        super().__init__(
            command_arguments,
            repository=repository,
            only_fix_error_code=only_fix_error_code,
            upgrade_version=upgrade_version,
            error_source=error_source,
            no_commit=no_commit,
            submit=submit,
        )
        self._path: Path = path

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "FixmeSingle":
        command_arguments = CommandArguments.from_arguments(arguments)
        return FixmeSingle(
            command_arguments,
            repository=repository,
            only_fix_error_code=arguments.only_fix_error_code,
            upgrade_version=arguments.upgrade_version,
            error_source=arguments.error_source,
            no_commit=arguments.no_commit,
            submit=arguments.submit,
            path=arguments.path,
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(FixmeSingle, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "path",
            help="Path to project root with local configuration",
            type=path_exists,
        )

    def run(self) -> None:
        project_configuration = Configuration.find_project_configuration()
        configuration_path = self._path / ".pyre_configuration.local"
        configuration = Configuration(configuration_path)
        self._suppress_errors_in_project(configuration, project_configuration.parent)


class FixmeAll(ProjectErrorSuppressingCommand):
    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "FixmeAll":
        command_arguments = CommandArguments.from_arguments(arguments)
        return FixmeAll(
            command_arguments,
            repository=repository,
            only_fix_error_code=arguments.only_fix_error_code,
            upgrade_version=arguments.upgrade_version,
            error_source=arguments.error_source,
            no_commit=arguments.no_commit,
            submit=arguments.submit,
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(FixmeAll, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)

    def run(self) -> None:
        project_configuration = Configuration.find_project_configuration()
        configurations = Configuration.gather_local_configurations()
        for configuration in configurations:
            self._suppress_errors_in_project(
                configuration, project_configuration.parent
            )


class FixmeTargets(ErrorSuppressingCommand):
    def __init__(
        self,
        command_arguments: CommandArguments,
        *,
        repository: Repository,
        subdirectory: Optional[str],
        no_commit: bool,
        submit: bool,
    ) -> None:
        super().__init__(command_arguments, repository)
        self._subdirectory: Final[Optional[str]] = subdirectory
        self._no_commit: bool = no_commit
        self._submit: bool = submit

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "FixmeTargets":
        command_arguments = CommandArguments.from_arguments(arguments)
        return FixmeTargets(
            command_arguments,
            repository=repository,
            subdirectory=arguments.subdirectory,
            no_commit=arguments.no_commit,
            submit=arguments.submit,
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(FixmeTargets, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument("--submit", action="store_true", help=argparse.SUPPRESS)
        parser.add_argument(
            "--subdirectory", help="Only upgrade TARGETS files within this directory."
        )
        parser.add_argument(
            "--no-commit", action="store_true", help="Keep changes in working state."
        )

    def run(self) -> None:
        subdirectory = self._subdirectory
        subdirectory = Path(subdirectory) if subdirectory else None
        project_configuration = Configuration.find_project_configuration(subdirectory)
        project_directory = project_configuration.parent
        search_root = subdirectory if subdirectory else project_directory

        all_targets = find_targets(search_root)
        if not all_targets:
            return
        for path, targets in all_targets.items():
            self._run_fixme_targets_file(project_directory, path, targets)

        self._repository.submit_changes(
            commit=(not self._no_commit),
            submit=self._submit,
            title=f"Upgrade pyre version for {search_root} (TARGETS)",
        )

    def _run_fixme_targets_file(
        self, project_directory: Path, path: str, targets: List[Target]
    ) -> None:
        LOG.info("Processing %s...", path)
        target_names = [
            path.replace("/TARGETS", "") + ":" + target.name + "-pyre-typecheck"
            for target in targets
        ]
        errors = errors_from_targets(project_directory, path, target_names)
        if not errors:
            return
        LOG.info("Found %d type errors in %s.", len(errors), path)

        if not errors:
            return

        self._suppress_errors(errors)

        if not self._lint:
            return

        if self._repository.format():
            errors = errors_from_targets(project_directory, path, target_names)
            if not errors:
                LOG.info("Errors unchanged after linting.")
                return
            LOG.info("Found %d type errors after linting.", len(errors))
            self._suppress_errors(errors)


def run(repository: Repository) -> None:
    parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
    parser.add_argument("--verbose", action="store_true", help="Enable verbose logging")

    commands = parser.add_subparsers()

    # Subcommands: Codemods
    missing_overridden_return_annotations = commands.add_parser(
        "missing-overridden-return-annotations",
        help="Add annotations according to errors inputted through stdin.",
    )
    MissingOverrideReturnAnnotations.add_arguments(
        missing_overridden_return_annotations
    )

    missing_global_annotations = commands.add_parser(
        "missing-global-annotations",
        help="Add annotations according to errors inputted through stdin.",
    )
    MissingGlobalAnnotations.add_arguments(missing_global_annotations)

    # Subcommand: Change default pyre mode to strict and adjust module headers.
    strict_default = commands.add_parser("strict-default")
    StrictDefault.add_arguments(strict_default)

    # Subcommand: Set global configuration to given hash, and add version override
    # to all local configurations to run previous version.
    update_global_version = commands.add_parser("update-global-version")
    GlobalVersionUpdate.add_arguments(update_global_version)

    # Subcommand: Fixme all errors inputted through stdin.
    fixme = commands.add_parser("fixme")
    Fixme.add_arguments(fixme)

    # Subcommand: Fixme all errors for a single project.
    fixme_single = commands.add_parser("fixme-single")
    FixmeSingle.add_arguments(fixme_single)

    # Subcommand: Fixme all errors in all projects with local configurations.
    fixme_all = commands.add_parser("fixme-all")
    FixmeAll.add_arguments(fixme_all)

    # Subcommand: Fixme all errors in targets running type checking
    fixme_targets = commands.add_parser("fixme-targets")
    FixmeTargets.add_arguments(fixme_targets)

    # Subcommand: Remove targets integration and replace with configuration
    targets_to_configuration = commands.add_parser("targets-to-configuration")
    TargetsToConfiguration.add_arguments(targets_to_configuration)

    # Subcommand: Expand target coverage in configuration up to given error limit
    expand_target_coverage = commands.add_parser("expand-target-coverage")
    ExpandTargetCoverage.add_arguments(expand_target_coverage)

    # Subcommand: Consolidate nested local configurations
    consolidate_nested_configurations = commands.add_parser("consolidate-nested")
    ConsolidateNestedConfigurations.add_arguments(consolidate_nested_configurations)

    # Initialize default values.
    arguments = parser.parse_args()
    if not hasattr(arguments, "command"):
        # Reparsing with `fixme` as default subcommand.
        arguments = parser.parse_args(sys.argv[1:] + ["fixme"])

    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.DEBUG if arguments.verbose else logging.INFO,
    )

    try:
        exit_code = ExitCode.SUCCESS
        arguments.command(arguments, repository).run()
    except UnstableAST as error:
        LOG.error(str(error))
        exit_code = ExitCode.FOUND_ERRORS
    except UserError as error:
        LOG.error(str(error))
        exit_code = ExitCode.FAILURE
    except Exception as error:
        LOG.error(str(error))
        LOG.debug(traceback.format_exc())
        exit_code = ExitCode.FAILURE

    sys.exit(exit_code)


def main() -> None:
    run(Repository())


if __name__ == "__main__":
    main()
